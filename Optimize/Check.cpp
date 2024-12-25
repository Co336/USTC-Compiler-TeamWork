#include "Check.h"
#include "Module.h"

#include "logging.hpp"
#include <algorithm>
#include <unordered_map>

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
    std::unordered_map<Ptr<BasicBlock>, std::set<Ptr<Value>>> def_bb;
    std::unordered_map<Ptr<BasicBlock>, std::set<Ptr<Value>>> pre_def;
    std::set<Ptr<Value>> all_def;

    for(const auto &func : this->module.lock()->get_functions()){
        // if no bb, next
        if(func->get_basic_blocks().empty())
            continue;
        std::cout << "Now at Func: " << func->get_name() << std::endl;
        // -----check BB pred and succ-----
        for(const auto &bb : func->get_basic_blocks()){
            for(const auto &pre_bb : bb->get_pre_basic_blocks()){
                auto pre_succ_bbs = pre_bb.lock()->get_succ_basic_blocks();
                if(std::find(pre_succ_bbs.begin(), pre_succ_bbs.end(), bb) == pre_succ_bbs.end()){
                    // there is an error 
                    std::cout << "A bb's pre-bb's succ-bb has not itself!" << std::endl;
                    std::cout << "Error bb: " << bb->get_name() <<", whose pre-bb is " << pre_bb.lock()->get_name() << std::endl;
                    exit(0);
                }
            }
            for(const auto &succ_bb : bb->get_succ_basic_blocks()){
                auto succ_pre_bbs = succ_bb.lock()->get_pre_basic_blocks();
                if(std::find(succ_pre_bbs.begin(), succ_pre_bbs.end(), bb) == succ_pre_bbs.end()){
                     // there is an error 
                    std::cout << "A bb's succ-bb's pre-bb has not itself!" << std::endl;
                    std::cout << "Error bb: " << bb->get_name() << ", whose succ-bb is " << succ_bb.lock()->get_name() << std::endl;
                    exit(0);
                }
            }
        }
        std::cout << " BB Pred-Succ Check Pass." << std::endl;

        // -----check last inst of BB-----
        for(const auto &bb : func->get_basic_blocks()){
            // get final inst of a bb
            auto last_inst = bb->get_instructions().back();
            if(!last_inst->is_ret() && !last_inst->is_br()){
                std::cout << "The last instruction of a bb is neither ret or br!" << std::endl;
                std::cout << "Error bb: "<< bb->get_name() << ", whose last instruction is " << last_inst->get_instr_op_name() << std::endl;
                exit(0);
            }
        }
        std::cout << " Last inst of BB Check Pass." << std::endl;

        // -----check use-def chain-----
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
        std::cout << " Use-Def Chain Check Pass." << std::endl;

        // -----check def before use-----



    }

}

}
}

