#ifndef SYSYF_COMSUBEXPRELI_H
#define SYSYF_COMSUBEXPRELI_H

#include "BasicBlock.h"
#include "Pass.h"
#include <map>
#include <memory>
#include <set>
#include <unordered_set>
#include <iterator>
#include "Instruction.h"
#include "internal_types.h"

namespace SysYF {
namespace IR {

struct numbering {
    int const_expr_numbering(Ptr<Value> opr) {
        if(opr->as<ConstantInt>()) return 1;
        else if(opr->as<ConstantFloat>()) return 2;
        else return 3;
    }
};

struct cmp_expr{
    bool operator()(WeakPtr<Instruction> a, WeakPtr<Instruction> b) const {
        // TODO if a < b return true
        struct numbering nu;
        Instruction::OpID opa = a.lock()->get_instr_type();
        Instruction::OpID opb = b.lock()->get_instr_type();
        // if(opa == Instruction::OpID::mul) return false;
        if(opa < opb) return true;
        else if(opa > opb) return false;

        if(a.lock()->isBinary()) {
            Ptr<Value> opral = a.lock()->get_operand(0);
            Ptr<Value> oprar = a.lock()->get_operand(1);
            Ptr<Value> oprbl = b.lock()->get_operand(0);
            Ptr<Value> oprbr = b.lock()->get_operand(1);

            int opral_numbering = nu.const_expr_numbering(opral);
            int oprar_numbering = nu.const_expr_numbering(oprar);
            int oprbl_numbering = nu.const_expr_numbering(oprbl);
            int oprbr_numbering = nu.const_expr_numbering(oprbr);

            if(opa == Instruction::OpID::add || opa == Instruction::OpID::mul || opa == Instruction::OpID::fadd || opa == Instruction::OpID::fmul ) {
                if(opral_numbering > oprar_numbering) {std::swap(opral, oprar);std::swap(opral_numbering, oprar_numbering);}
                if(oprbl_numbering > oprbr_numbering) {std::swap(oprbl, oprbr);std::swap(oprbl_numbering, oprbr_numbering);}
            }

            if(opral_numbering < oprbl_numbering) return true;
            else if(opral_numbering == oprbl_numbering && oprar_numbering < oprbr_numbering) return true;
            else if(opral_numbering == oprbl_numbering && oprar_numbering == oprbr_numbering) {}
            else return false;

            if(opral_numbering == 1 && oprbl_numbering == 1) {
                int opral_val = opral->as<ConstantInt>()->get_value();
                int oprbl_val = oprbl->as<ConstantInt>()->get_value();
                if(opral_val < oprbl_val) return true;
                else if(opral_val > oprbl_val) return false;
            } else if(opral_numbering == 2 && oprbl_numbering == 2) {
                float opral_val = opral->as<ConstantFloat>()->get_value();
                float oprbl_val = oprbl->as<ConstantFloat>()->get_value();
                if(opral_val < oprbl_val) return true;
                else if(opral_val > oprbl_val) return false;
            } else {
                Value *opral_ptr = opral.get();
                Value *oprbl_ptr = oprbl.get();
                if(opral_ptr < oprbl_ptr) return true;
                else if(opral_ptr > oprbl_ptr) return false;
            }

            if(oprar_numbering == 1 && oprbr_numbering == 1) {
                int oprar_val = oprar->as<ConstantInt>()->get_value();
                int oprbr_val = oprbr->as<ConstantInt>()->get_value();
                if(oprar_val < oprbr_val) return true;
                else return false;
            } else if(oprar_numbering == 2 && oprbr_numbering == 2) {
                float oprar_val = oprar->as<ConstantFloat>()->get_value();
                float oprbr_val = oprbr->as<ConstantFloat>()->get_value();
                if(oprar_val < oprbr_val) return true;
                else return false;
            } else {
                Value *oprar_ptr = oprar.get();
                Value *oprbr_ptr = oprbr.get();
                if(oprar_ptr < oprbr_ptr) return true;
                else return false;
            }
        } else if(a.lock()->is_gep()){
            int opra_num = a.lock()->get_num_operand();
            int oprb_num = b.lock()->get_num_operand();
            if(opra_num <= oprb_num) {
                for(int i = 0; i < opra_num; ++i) {
                    int opra_numbering = nu.const_expr_numbering(a.lock()->get_operand(i));
                    int oprb_numbering = nu.const_expr_numbering(b.lock()->get_operand(i));
                    if(opra_numbering < oprb_numbering) return true;
                    else if(opra_numbering > oprb_numbering) return false;

                    if(opra_numbering == 1 && oprb_numbering == 1) {
                        int opra_val = a.lock()->get_operand(i)->as<ConstantInt>()->get_value();
                        int oprb_val = b.lock()->get_operand(i)->as<ConstantInt>()->get_value();
                        if(opra_val < oprb_val) return true;
                        else if (opra_val > oprb_val) return false;
                    } else if(opra_numbering == 2 && oprb_numbering == 2) {
                        float opra_val = a.lock()->get_operand(i)->as<ConstantFloat>()->get_value();
                        float oprb_val = b.lock()->get_operand(i)->as<ConstantFloat>()->get_value();
                        if(opra_val < oprb_val) return true;
                        else if(opra_val > oprb_val) return false;
                    } else {
                        Value *opra_ptr = a.lock()->get_operand(i).get();
                        Value *oprb_ptr = b.lock()->get_operand(i).get();
                        if(opra_ptr < oprb_ptr) return true;
                        else if(opra_ptr > oprb_ptr) return false;
                    }
                }
                if(opra_num == oprb_num) 
                    return false;
                else 
                    return true;
            } else {
                for(int i = 0; i < oprb_num; ++i) {
                    int opra_numbering = nu.const_expr_numbering(a.lock()->get_operand(i));
                    int oprb_numbering = nu.const_expr_numbering(b.lock()->get_operand(i));
                    if(opra_numbering < oprb_numbering) return true;
                    else if(opra_numbering > oprb_numbering) return false;

                    if(opra_numbering == 1 && oprb_numbering == 1) {
                        int opra_val = a.lock()->get_operand(i)->as<ConstantInt>()->get_value();
                        int oprb_val = b.lock()->get_operand(i)->as<ConstantInt>()->get_value();
                        if(opra_val < oprb_val) return true;
                        else if (opra_val > oprb_val) return false;
                    } else if(opra_numbering == 2 && oprb_numbering == 2) {
                        float opra_val = a.lock()->get_operand(i)->as<ConstantFloat>()->get_value();
                        float oprb_val = b.lock()->get_operand(i)->as<ConstantFloat>()->get_value();
                        if(opra_val < oprb_val) return true;
                        else if(opra_val > oprb_val) return false;
                    } else {
                        Value *opra_ptr = a.lock()->get_operand(i).get();
                        Value *oprb_ptr = b.lock()->get_operand(i).get();
                        if(opra_ptr < oprb_ptr) return true;
                        else if(opra_ptr > oprb_ptr) return false;
                    }
                }
                return false;
            }
        } else { //sitofp
            int opra_numbering = nu.const_expr_numbering(a.lock()->get_operand(0));
            int oprb_numbering = nu.const_expr_numbering(b.lock()->get_operand(0));
            if(opra_numbering < oprb_numbering) return true;
            else if(opra_numbering > oprb_numbering) return false;

            if(opra_numbering == 1 && oprb_numbering == 1) {
                int opra_val = a.lock()->get_operand(0)->as<ConstantInt>()->get_value();
                int oprb_val = b.lock()->get_operand(0)->as<ConstantInt>()->get_value();
                if(opra_val < oprb_val) return true;
                else return false;
            } else if(opra_numbering == 2 && oprb_numbering == 2) {
                float opra_val = a.lock()->get_operand(0)->as<ConstantFloat>()->get_value();
                float oprb_val = b.lock()->get_operand(0)->as<ConstantFloat>()->get_value();
                if(opra_val < oprb_val) return true;
                else return false;
            } else {
                Value *opra_ptr = a.lock()->get_operand(0).get();
                Value *oprb_ptr = b.lock()->get_operand(0).get();
                if(opra_ptr < oprb_ptr) return true;
                else return false;
            }
        }
        return false;
    }
};


// struct cmp_expr{
//     bool operator()(WeakPtr<Instruction> a, WeakPtr<Instruction> b) const {
//         // TODO if a < b return true
//         // struct numbering nu;
//         Instruction::OpID opa = a.lock()->get_instr_type();
//         Instruction::OpID opb = b.lock()->get_instr_type();
//         if(opa < opb) return true;
//         else if(opa > opb) return false;

//         if(a.lock()->isBinary()) {
//             Value *opral = a.lock()->get_operand(0).get();
//             Value *oprar = a.lock()->get_operand(1).get();
//             Value *oprbl = b.lock()->get_operand(0).get();
//             Value *oprbr = b.lock()->get_operand(1).get();

//             if(opa == Instruction::OpID::add || opa == Instruction::OpID::mul ||
//                 opa == Instruction::OpID::fadd || opa == Instruction::OpID::fmul ) {
//                 if(opral >= oprar) std::swap(opral, oprar);
//                 if(oprbl >= oprbr) std::swap(oprbl, oprbr);
//             }

//             if(opral < oprbl) return true;
//             else if(opral == oprbl && oprar < oprbr) return true;
//             else return false;
//         } else if(a.lock()->is_gep()){
//             int opra_num = a.lock()->get_num_operand();
//             int oprb_num = b.lock()->get_num_operand();
//             if(opra_num <= oprb_num) {
//                 for(int i = 0; i < opra_num; ++i) {
//                     Value *opra_ptr = a.lock()->get_operand(i).get();
//                     Value *oprb_ptr = b.lock()->get_operand(i).get();

//                     if(opra_ptr < oprb_ptr) return true;
//                     else if (opra_ptr > oprb_ptr) return false;
//                 }
//                 if(opra_num == oprb_num) 
//                     return false;
//                 else 
//                     return true;
//             } else {
//                 for(int i = 0; i < oprb_num; ++i) {
//                     Value *opra = a.lock()->get_operand(i).get();
//                     Value *oprb = b.lock()->get_operand(i).get();

//                     if(opra < oprb) return true;
//                     else return false;
//                 }
//                 return false;
//             }
//         } else { //sitofp
//             Value *opra = a.lock()->get_operand(0).get();
//             Value *oprb = b.lock()->get_operand(0).get();

//             if(opra < oprb) return true;
//             else return false;
//         }
//         return false;
//     }
// };

/*****************************CommonSubExprElimination**************************************/
/***************************This class is based on SSA form*********************************/
class ComSubExprEli : public Pass {
public:
    explicit ComSubExprEli(WeakPtr<Module> m):Pass(m){}
    const std::string get_name() const override {return name;}
    void execute() override;
    void compute_local_gen(Ptr<Function> func);
    void compute_global_in_out(Ptr<Function> func);
    void compute_global_common_expr(Ptr<Function> func);
    void debug_print_in_out_gen(Ptr<Function> func);
	Ptr<Value> find_definition_expr(Ptr<BasicBlock> bb_cur, Ptr<Instruction> inst);
    /**
     * @brief init bb in/out/gen map with empty set
     * 
     * @param f 
     */
    void initial_map(Ptr<Function> f);
    static bool is_valid_expr(Ptr<Instruction> inst);
private:
    struct cmp_expr cmp_exp;
    const std::string name = "ComSubExprEli";
    std::set<Ptr<Instruction>,cmp_expr> availableExprs;
	std::unordered_set<Ptr<BasicBlock> > bb_vis;
    std::map<Ptr<Instruction>, Ptr<BasicBlock> > del_expr;
    WeakPtrMap<BasicBlock, std::set<Ptr<Instruction>, cmp_expr>> bb_in;
    WeakPtrMap<BasicBlock, std::set<Ptr<Instruction>, cmp_expr>> bb_out;
    WeakPtrMap<BasicBlock, std::set<Ptr<Instruction>, cmp_expr>> bb_gen;
    bool rerun = false;
};

}
}

#endif // SYSYF_COMSUBEXPRELI_H
