#include "Check.h"
#include "Module.h"

// #include "logging.hpp"
// #include <algorithm>
// #include <unordered_map>

namespace SysYF {
namespace IR {

void Check::execute() {
    //TODO write your IR Module checker here.
    // 4 check in total
    // BB pred and succ check
    // use-def chain check
    // last inst of BB check
    // def before use check

    std::cout << "Check Start" << std::endl;

    // some vars
    // std::unordered_map<Ptr<BasicBlock>, std::set<Ptr<Value>>> bb_insts_map;
    // std::unordered_map<Ptr<BasicBlock>, std::set<Ptr<Value>>> pre_def;
    std::set<Ptr<Value>> defs;
    std::set<Ptr<Value>> globaldefs;

    // get all the globalvars first
    for(const auto &global : this->module.lock()->get_global_variable()){
        globaldefs.insert(global);
    }
    // 4 check  
    for(const auto &func : this->module.lock()->get_functions()){
        // if no bb, next
        if(func->get_basic_blocks().empty())
            continue;
        std::cout << "Now at Func: " << func->get_name() << std::endl;
        // ----------check BB pred and succ----------
        for(const auto &bb : func->get_basic_blocks()){
            for(const auto &pre_bb : bb->get_pre_basic_blocks()){
                auto pre_succ_bbs = pre_bb.lock()->get_succ_basic_blocks();
                bool found = false;
                for (const auto &succ_bb_weak : pre_succ_bbs) {
                    // weak_ptr -> shared_ptr
                    auto succ_bb = succ_bb_weak.lock();
                    if (succ_bb && succ_bb == bb) {
                        found = true;
                        break;
                    }
                }
                if(!found){
                    // there is an error 
                    std::cout << "A bb's pre-bb's succ-bb has not itself!" << std::endl;
                    std::cout << "Error bb: " << bb->get_name() <<", whose pre-bb is " << pre_bb.lock()->get_name() << std::endl;
                    exit(0);
                }
            }
            for(const auto &succ_bb : bb->get_succ_basic_blocks()){
                auto succ_pre_bbs = succ_bb.lock()->get_pre_basic_blocks();
                bool found = false;
                for (const auto &pre_bb_weak : succ_pre_bbs) {
                    // weak_ptr->shared_ptr
                    auto pre_bb = pre_bb_weak.lock();
                    if (pre_bb && pre_bb == bb) {
                        found = true;
                        break;
                    }
                }
                if(!found){
                     // there is an error 
                    std::cout << "A bb's succ-bb's pre-bb has not itself!" << std::endl;
                    std::cout << "Error bb: " << bb->get_name() << ", whose succ-bb is " << succ_bb.lock()->get_name() << std::endl;
                    exit(0);
                }
            }
        }
        std::cout << "BB Pred-Succ Check Pass." << std::endl;

        // ----------check last inst of BB----------
        for(const auto &bb : func->get_basic_blocks()){
            // get final inst of a bb
            auto last_inst = bb->get_instructions().back();
            if(!last_inst->isTerminator()){
                std::cout << "The last instruction of a bb is neither ret or br!" << std::endl;
                std::cout << "Error bb: "<< bb->get_name() << ", whose last instruction is " << last_inst->get_instr_op_name() << std::endl;
                exit(0);
            }
        }
        std::cout << "Last inst of BB Check Pass." << std::endl;

        // ----------check use-def chain----------
        for(const auto &bb : func->get_basic_blocks()){
            for(const auto &inst : bb->get_instructions()){
                for(const auto &operand : inst->get_operands()){
                    // use std::find_if() for comlpex search with Lambda exp
                    auto item = std::find_if(operand.lock()->get_use_list().begin(), operand.lock()->get_use_list().end(), [inst](Use use){ return inst == use.val_.lock(); });
                    if(item == operand.lock()->get_use_list().end()){
                        std::cout << "Use-Def is not valid!" << std::endl;
                        std::cout << "Error bb: " << bb->get_name() << ", whose unvalid instruction is " << inst->get_instr_op_name() << 
                        ", unvalid operand is " << operand.lock()->get_name() << std::endl;
                        exit(0);
                    }
                }
            }
        }
        std::cout << "Use-Def Chain Check Pass." << std::endl;

        // ----------check def before use----------
            // get all the defs
        for(const auto &args : func->get_args())
            defs.insert(args);
        for(const auto &bb : func->get_basic_blocks()){ 
            for(const auto &inst : bb->get_instructions()){
                if(!inst->is_void())
                    defs.insert(inst);
            }
        }
            // make sure all operands are defs
        for(const auto &bb : func->get_basic_blocks()){ 
            for(const auto &inst : bb->get_instructions()){
                for(const auto &op : inst->get_operands()){
                    auto Type = op.lock()->get_type();
                    // is const, jump it
                    if(dynamic_pointer_cast<Constant>(op.lock())) 
                        continue;
                    if(!(Type->is_array_type() || 
                    Type->is_float_type() || 
                    Type->is_pointer_type() || 
                    Type->is_integer_type())) 
                        continue;
                    if(!(defs.count(op.lock()) || globaldefs.count(op.lock()))){
                        std::cout << "Use before define!" << std::endl;
                        std::cout << "Error bb: " << bb->get_name() << ", whose instruction is " << inst->get_instr_op_name() << ", operand is " << op.lock()->get_name() << std::endl;
                        exit(0);
                    }
                }
            }
        }
        std::cout << "Def Before Use Check Pass." << std::endl;
    }
}
}
}

