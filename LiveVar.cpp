#include "LiveVar.h"
#include "BasicBlock.h"
#include "Value.h"
#include "internal_types.h"
#include <fstream>

#include <algorithm>
#include <memory>

namespace SysYF
{
namespace IR
{

// 为weak_ptr添加比较函数
struct WeakPtrCompare {
    bool operator()(const WeakPtr<Value>& a, const WeakPtr<Value>& b) const {
        auto a_ptr = a.lock();
        auto b_ptr = b.lock();
        if (!a_ptr || !b_ptr) return false;
        return a_ptr->get_name() < b_ptr->get_name();
    }
};

struct BasicBlockCompare {
    bool operator()(const Ptr<BasicBlock>& a, const Ptr<BasicBlock>& b) const {
        return a->get_name() < b->get_name();
    }
};

void LiveVar::execute() {
    module.lock()->set_print_name();

    for (auto func : this->module.lock()->get_functions()) {
        if (func->get_basic_blocks().empty()) {
            continue;
        }
        
        func_ = func;
        
        std::map<Ptr<BasicBlock>, 
                std::map<WeakPtr<Value>, 
                        std::set<WeakPtr<Value>, WeakPtrCompare>, 
                        WeakPtrCompare>, 
                BasicBlockCompare> wherefrom;

        while (true) {
            bool changed = false;
            
            for (auto bb : func->get_basic_blocks()) {
                auto last_in = bb->get_live_in();
                auto last_out = bb->get_live_out();
                
                WeakPtrSet<Value> new_in;
                WeakPtrSet<Value> def_set;

                // 计算DEF集合和IN集合
                for (auto instr : bb->get_instructions()) {
                    // 处理操作数（USE）
                    for (size_t i = 0; i < instr->get_operands().size(); i++) {
                        auto op = instr->get_operand(i);
                        
                        // 跳过常量和特殊类型
                        if (dynamic_cast<ConstantInt*>(op.get())) continue;
                        if (dynamic_cast<BasicBlock*>(op.get())) continue;
                        if (dynamic_cast<Function*>(op.get())) continue;
                        if (dynamic_cast<GlobalVariable*>(op.get())) continue;
                        
                        // 修改：如果操作数未定义或在phi指令中，加入IN集合
                        WeakPtr<Value> weak_op = op;
                        if (instr->is_phi() || def_set.find(op) == def_set.end()) {
                            if (instr->is_phi()) {
                                // 对phi指令特殊处理：记录变量来自哪个基本块
                                auto next_op = instr->get_operand(i + 1);
                                WeakPtr<Value> weak_next_op = std::dynamic_pointer_cast<Value>(next_op);
                                wherefrom[bb][weak_op].insert(weak_next_op);
                            } else {
                                // 非phi指令：记录当前基本块
                                WeakPtr<Value> weak_bb = std::dynamic_pointer_cast<Value>(bb);
                                wherefrom[bb][weak_op].insert(weak_bb);
                            }
                            new_in.insert(op);
                        }
                    }
                    
                    if (!instr->is_void()) {
                        def_set.insert(instr);
                    }
                }

                // 处理OUT中的变量
                for (auto& out : last_out) {
                    if (def_set.find(out) == def_set.end()) {
                        new_in.insert(out);
                        // 修改：传播活跃性
                        WeakPtr<Value> weak_bb = std::dynamic_pointer_cast<Value>(bb);
                        wherefrom[bb][out].insert(weak_bb);
                    }
                }

                // 计算新的OUT集合
                WeakPtrSet<Value> new_out;
                for (auto succ : bb->get_succ_basic_blocks()) {
                    auto succ_bb = succ.lock();
                    for (auto& in_val : succ_bb->get_live_in()) {
                        // 检查所有可能的活跃性来源
                        bool should_add = false;
                        auto it = wherefrom[succ_bb].find(in_val);
                        
                        if (it == wherefrom[succ_bb].end()) {
                            should_add = true;  // 没有明确的来源，保守处理
                        } else {
                            // 检查是否有任何来源与当前基本块相关
                            for (const auto& source : it->second) {
                                auto source_ptr = source.lock();
                                if (source_ptr == bb || source_ptr == succ_bb) {
                                    should_add = true;
                                    break;
                                }
                            }
                        }

                        if (should_add) {
                            new_out.insert(in_val);
                            // 传播活跃性给当前基本块
                            if (def_set.find(in_val) == def_set.end()) {
                                WeakPtr<Value> weak_bb = std::dynamic_pointer_cast<Value>(bb);
                                wherefrom[bb][in_val].insert(weak_bb);
                            }
                        }
                    }
                }

                // 检查是否有变化
                bool in_changed = false;
                bool out_changed = false;

                // 比较IN集合
                if (last_in.size() != new_in.size()) {
                    in_changed = true;
                } else {
                    for (auto& val : last_in) {
                        if (new_in.find(val) == new_in.end()) {
                            in_changed = true;
                            break;
                        }
                    }
                }

                // 比较OUT集合
                if (last_out.size() != new_out.size()) {
                    out_changed = true;
                } else {
                    for (auto& val : last_out) {
                        if (new_out.find(val) == new_out.end()) {
                            out_changed = true;
                            break;
                        }
                    }
                }

                if (in_changed || out_changed) {
                    changed = true;
                }

                // 更新IN和OUT集合
                bb->set_live_in(new_in);
                bb->set_live_out(new_out);
            }
            
            if (!changed) break;
        }
    }

    dump();
}

void LiveVar::dump() {
    std::fstream f;
    f.open(lvdump, std::ios::out);
    for (auto &func: module.lock()->get_functions()) {
        for (auto &bb: func->get_basic_blocks()) {
            f << bb->get_name() << std::endl;
            auto &in = bb->get_live_in();
            auto &out = bb->get_live_out();
            auto sorted_in = sort_by_name(in);
            auto sorted_out = sort_by_name(out);
            f << "in:\n";
            for (auto in_v: sorted_in) {
                if(in_v.lock()->get_name() != "")
                {
                    f << in_v.lock()->get_name() << " ";
                }
            }
            f << "\n";
            f << "out:\n";
            for (auto out_v: sorted_out) {
                if(out_v.lock()->get_name() != ""){
                    f << out_v.lock()->get_name() << " ";
                }
            }
            f << "\n";
        }
    }
    f.close();
}


bool ValueCmp(WeakPtr<Value> a, WeakPtr<Value> b) {
    return a.lock()->get_name() < b.lock()->get_name();
}

WeakPtrVec<Value> sort_by_name(WeakPtrSet<Value> &val_set) {
    WeakPtrVec<Value> result;
    result.assign(val_set.begin(), val_set.end());
    std::sort(result.begin(), result.end(), ValueCmp);
    return result;
}

}
}
