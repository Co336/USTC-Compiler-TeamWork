#include "Pass.h"
#include "ComSubExprEli.h"
#include <set>
#include <algorithm>

namespace SysYF {
namespace IR {

void ComSubExprEli::execute() {
    for(auto func : module.lock()->get_functions()) {
        if(func->get_basic_blocks().empty())continue;
        bb_in.clear();
        bb_out.clear();
        bb_gen.clear();
        del_expr.clear();
        ComSubExprEli::compute_local_gen(func);
        ComSubExprEli::compute_global_in_out(func);
        // ComSubExprEli::debug_print_in_out_gen(func);
        ComSubExprEli::compute_global_common_expr(func);
    }
}

void ComSubExprEli::debug_print_in_out_gen(Ptr<Function> func) {
    for(auto bb : func->get_basic_blocks()) {
        printf("%s: \n", bb->get_name().c_str());
        printf("IN:\n");
        for(auto inst : bb_in[bb]) {
            std::cout << dynamic_pointer_cast<Instruction>(inst)->print() << "\t";
        }
        std::cout << std::endl;
        printf("OUT:\n");
        for(auto inst : bb_out[bb]) {
            std::cout << dynamic_pointer_cast<Instruction>(inst)->print() << "\t";
        }
        std::cout << std::endl;
        printf("GEN:\n");
        for(auto inst : bb_gen[bb]) {
            std::cout << dynamic_pointer_cast<Instruction>(inst)->print() << "\t";
        }
        std::cout << std::endl;
    }
    printf("all_exprs\n");
    for(auto inst : availableExprs) {
        std::cout << dynamic_pointer_cast<Instruction>(inst)->print() << std::endl;
    }
}

bool ComSubExprEli::is_valid_expr(Ptr<Instruction> inst) {
    return !(
        inst->is_void() // ret, br, store, void call
        || inst->is_phi()
        || inst->is_alloca()
        || inst->is_load()
        || inst->is_call()
        || inst->is_cmp()
        || inst->is_fcmp()
    );
}

void ComSubExprEli::compute_local_gen(Ptr<Function> func) {
    for(auto bb : func->get_basic_blocks()) {
        for(auto inst : bb->get_instructions()) {
            if(!ComSubExprEli::is_valid_expr(inst)) continue;
            bb_gen[bb].insert(inst);
            availableExprs.insert(inst);
        }
    }
}

void ComSubExprEli::compute_global_in_out(Ptr<Function> func) {
    for(auto bb : func->get_basic_blocks()) {
        if(bb->get_name() == "label_entry") {
            bb_in[bb].clear();
        }
        bb_out[bb] = availableExprs;
    }

    rerun = true;
    while(rerun) {
        rerun = false;
        for(auto bb : func->get_basic_blocks()) {
            std::set<Ptr<Instruction>, cmp_expr> pre_Y(availableExprs);
            std::set<Ptr<Instruction>, cmp_expr> tmp_Y;

            if(bb->get_name() == "label_entry") {
                bb_in[bb].clear();
                tmp_Y = bb_gen[bb];
                if(tmp_Y != bb_out[bb]) rerun = true;
                bb_out[bb] = tmp_Y;
                continue;
            }

            for (auto bb_pre : bb->get_pre_basic_blocks()) {
                tmp_Y.clear();
                // std::set_intersection(
                //     pre_Y.begin(), pre_Y.end(),
                //     bb_out[bb_pre].begin(), bb_out[bb_pre].end(),
                //     std::insert_iterator<std::set<Ptr<Instruction>, cmp_expr> >(tmp_Y, tmp_Y.begin())
                // );
                // pre_Y = tmp_Y;

                for(auto inst : pre_Y) {
                    if(bb_out[bb_pre].find(inst) != bb_out[bb_pre].end()) {
                        tmp_Y.insert(inst);
                    }
                }
                pre_Y = tmp_Y;
            }

            bb_in[bb] = pre_Y;

            tmp_Y.clear();
            std::set_union(
                bb_in[bb].begin(), bb_in[bb].end(),
                bb_gen[bb].begin(), bb_gen[bb].end(),
                std::insert_iterator<std::set<Ptr<Instruction>, cmp_expr> >(tmp_Y, tmp_Y.begin())
            );
            if(tmp_Y != bb_out[bb]) rerun = true;
            bb_out[bb] = tmp_Y;
        }
    } 

}

void ComSubExprEli::compute_global_common_expr(Ptr<Function> f){
    for(auto bb : f->get_basic_blocks()) {
        std::set<Ptr<Instruction>, cmp_expr> bb_cur_expr;
        for(auto inst : bb->get_instructions()) {
			if(!ComSubExprEli::is_valid_expr(inst)) continue;
            if(availableExprs.find(inst) != availableExprs.end() && bb_in[bb].find(inst) != bb_in[bb].end()) {
                bb_vis.clear();
                Ptr<Value> pre_def = find_definition_expr(bb, inst);
                inst->replace_all_use_with(pre_def);
                // bb->delete_instr(inst);
                del_expr.insert({inst, bb});
            } else {
                if(bb_cur_expr.find(inst) != bb_cur_expr.end()) {
					auto iter = bb_cur_expr.find(inst);
                    inst->replace_all_use_with(*iter);
                    // bb->delete_instr(inst);
                    del_expr.insert({inst, bb});
                } else {
                    bb_cur_expr.insert(inst);
                    availableExprs.insert(inst);
                }
                // int find_flag = 0;
                // for(auto expr : bb_cur_expr) {
                //     if(!cmp_exp(inst, expr) && !cmp_exp(expr, inst)) {
                //         inst->replace_all_use_with(expr);
                //         del_expr.insert({inst, bb});
                //         find_flag = 1;
                //         break;
                //     }
                // }
                // if(!find_flag) {
                //     bb_cur_expr.insert(inst);
                // }
            }
        }
    }

    for(auto iter = del_expr.begin(); iter != del_expr.end(); ++iter)
        iter->second->delete_instr(iter->first);
}

Ptr<Value> ComSubExprEli::find_definition_expr(Ptr<BasicBlock> bb_cur, Ptr<Instruction> inst) {
    std::unordered_map<Ptr<Value>, WeakPtr<BasicBlock> > bb_def;
	bb_vis.insert(bb_cur);
    for(auto bb_pre : bb_cur->get_pre_basic_blocks()) {
		if(bb_vis.find(bb_pre.lock()) != bb_vis.end()) continue;
        if(bb_in[bb_pre].find(inst) == bb_in[bb_pre].end() && bb_gen[bb_pre].find(inst) != bb_gen[bb_pre].end()) {
            // auto inst_iter = bb_pre.lock()->find_instruction(inst);
			// bb_def.insert({*inst_iter, bb_pre});
			// bb_vis.insert(bb_pre.lock());
            for(auto expr : bb_pre.lock()->get_instructions()) {\

                if(!cmp_exp(inst, expr) && !cmp_exp(expr, inst)) {
                    bb_def.insert({expr, bb_pre});
                    bb_vis.insert(bb_pre.lock());
                    break;
                }
            }
        } else {
            Ptr<Value> tmp = ComSubExprEli::find_definition_expr(bb_pre.lock(), inst);
            if(tmp != nullptr) {
                bb_def.insert({tmp, bb_pre});
            }
        }
    }

    if(bb_def.size() == 0) 
        return nullptr;
    else if(bb_def.size() == 1) 
        return bb_def.begin()->first;
    else {
        Ptr<Type> phi_ty = bb_def.begin()->first->get_type();
        Ptr<PhiInst> phi_inst = PhiInst::create_phi(phi_ty, bb_cur);
        for(auto iter = bb_def.begin(); iter != bb_def.end(); ++iter) 
            phi_inst->add_phi_pair_operand(iter->first, iter->second.lock());
        bb_cur->add_instr_begin(phi_inst);
        return phi_inst;
    }
}

}
}
