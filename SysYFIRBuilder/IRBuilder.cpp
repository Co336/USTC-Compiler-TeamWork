#include "IRBuilder.h"
#include "BasicBlock.h"
#include "Function.h"
#include "SyntaxTree.h"
#include "Type.h"

namespace SysYF
{
namespace IR
{
#define CONST_INT(num) ConstantInt::create(num, module)
#define CONST_FLOAT(num) ConstantFloat::create(num, module)

// types
Ptr<Type> VOID_T;
Ptr<Type> INT1_T;
Ptr<Type> INT32_T;
Ptr<Type> FLOAT_T;
Ptr<Type> INT32PTR_T;
Ptr<Type> FLOATPTR_T;

// global variables for implemention
Ptr<Value> latest_value = nullptr;    // 指示当前最近处理的表达式的值
Ptr<BasicBlock> CondBB = nullptr;     // 指示当前while语句的条件基本块
Ptr<BasicBlock> NextBB = nullptr;     // 指示当前while循环和if语句的下一个基本块
Ptr<BasicBlock> TrueBB = nullptr;     // 指示当前条件判断的True分支基本块
Ptr<BasicBlock> FalseBB = nullptr;    // 指示当前条件判断的False分支基本块
Ptr<BasicBlock> CurrentBB = nullptr;  // 指示当前所处的基本块位置
long BB_id = 0;                       // 创建的BB的 label ：格式为 "......BB_<BB_id>"
Ptr<Function> CurrentFunction = nullptr; // 指示当前所处的函数

void IRBuilder::visit(SyntaxTree::Assembly &node) {
    VOID_T = Type::get_void_type(module);
    INT1_T = Type::get_int1_type(module);
    INT32_T = Type::get_int32_type(module);
    FLOAT_T = Type::get_float_type(module);
    INT32PTR_T = Type::get_int32_ptr_type(module);
    FLOATPTR_T = Type::get_float_ptr_type(module);
    for (const auto &def : node.global_defs) {
        def->accept(*this);
    }
}

// TODO: You need to fill them.
// NOTE: The following codes are just examples, you can modify them as you like.

void IRBuilder::visit(SyntaxTree::InitVal &node) {}

void IRBuilder::visit(SyntaxTree::FuncDef &node) {
    auto fn = Function::create(FunctionType::create(INT32_T, {}, this->module), "main", this->module);
    this->cur_func = fn;
    auto entry = BasicBlock::create(this->module, "entry", fn);
    builder->set_insert_point(entry);
    node.body->accept(*this);
}

void IRBuilder::visit(SyntaxTree::FuncFParamList &node) {}

void IRBuilder::visit(SyntaxTree::FuncParam &node) {}

void IRBuilder::visit(SyntaxTree::VarDef &node) {}

void IRBuilder::visit(SyntaxTree::LVal &node) {}

void IRBuilder::visit(SyntaxTree::AssignStmt &node) {
    
}

void IRBuilder::visit(SyntaxTree::Literal &node) {
    switch (node.literal_type)
    {
    case SyntaxTree::Type::INT: {
        this->visitee_val = CONST_INT(node.int_const);
        break;
    }
    case SyntaxTree::Type::FLOAT: {
        this->visitee_val = CONST_FLOAT(node.float_const);
        break;
    }
    default:
        throw UnreachableException();
        break;
    }
}

void IRBuilder::visit(SyntaxTree::ReturnStmt &node) {
    this->visitee_val.reset();
    node.ret->accept(*this);
    builder->create_ret(this->visitee_val);
}

void IRBuilder::visit(SyntaxTree::BlockStmt &node) {
    for (auto stmt: node.body) {
        stmt->accept(*this);
    }
}

void IRBuilder::visit(SyntaxTree::EmptyStmt &node) {}

void IRBuilder::visit(SyntaxTree::ExprStmt &node) {
    node.exp->accept(*this);
}

void IRBuilder::visit(SyntaxTree::UnaryCondExpr &node) {}

void IRBuilder::visit(SyntaxTree::BinaryCondExpr &node) {}

void IRBuilder::visit(SyntaxTree::BinaryExpr &node) {}

void IRBuilder::visit(SyntaxTree::UnaryExpr &node) {
    // 单目运算符 正 和 负
    // 需要判断操作数是否是常量，若是则不需发射指令，直接设置latest_value即可
    node.rhs->accept(*this);
    // 得到latest_value
    auto value_temp = latest_value;
    // 若转换不合法则会返回nullptr，即不是常量
    if(std::dynamic_pointer_cast<ConstantFloat>(value_temp))
    {
        // 右操作数是一个浮点常量，则直接返回需要的latest_value
        if(node.op == SyntaxTree::UnaryOp::PLUS)
            latest_value = value_temp;
        else 
        {
            float value = std::dynamic_pointer_cast<ConstantFloat>(value_temp)->get_value();
            latest_value = ConstantFloat::create(-1.0 * value, module);
        }
    }
    else if(std::dynamic_pointer_cast<ConstantInt>(value_temp))
    {
        // 右操作数是一个整形常量，则直接返回需要的latest_value
        if(node.op == SyntaxTree::UnaryOp::PLUS)
            latest_value = value_temp;
        else 
        {
            int value = std::dynamic_pointer_cast<ConstantInt>(value_temp)->get_value();
            latest_value = ConstantInt::create(-1 * value, module);
        }
    }
    else
    {
        // 右操作数不是常数
        if(node.op == SyntaxTree::UnaryOp::PLUS)
        {
            latest_value = value_temp;
        }
        else
        {
            if(value_temp->get_type() == FLOAT_T)
            {   //发射一条浮点乘法，得到 -1.0 * value_temp->get_value()
                latest_value = builder->create_fmul(value_temp, CONST_FLOAT(-1));
            }
            else if(value_temp->get_type() == INT32_T)
            {   //发射一条整数乘法，得到 -1 * value_temp->get_value()
                latest_value = builder->create_imul(value_temp, CONST_INT(-1));
            }
        }
    }
}

void IRBuilder::visit(SyntaxTree::FuncCallStmt &node) {
    // 需要在Funcdef时将函数push进符号表
    // 查找符号表，得到Function指针
    Ptr<Value> Func_value = scope.find(node.name, true);
    // 基类到派生类的类型转换
    Ptr<Function> Func = std::dynamic_pointer_cast<Function>(Func_value);
    // 获取函数形参，便于在检查类型时进行类型转换
    std::vector<Ptr<Value>> func_params;
    for(auto param = Func->arg_begin(); param != Func->arg_end(); param++)
    {   //派生类到基类的自动隐式转换
        func_params.emplace_back(*param);
    }
    // 下面递归访问获得函数实参列表
    std::vector<Ptr<Value>> true_params;
    auto index = 0;
    for(auto &arg : node.params)
    {
        arg->accept(*this);
        // latest_value得到实参的信息
        if(func_params[index]->get_type() == FLOAT_T && latest_value->get_type() == INT32_T )
        {
            //实参转换为 float
            latest_value = builder->create_sitofp(latest_value, FLOAT_T);
            true_params.emplace_back(latest_value);
        }
        else if(func_params[index]->get_type() == INT32_T && latest_value->get_type() == FLOAT_T )
        {
            //实参转换为 INT
            latest_value = builder->create_fptosi(latest_value, INT32_T);
            true_params.emplace_back(latest_value);
        }
        else
        {
            true_params.emplace_back(latest_value);
        }
        index++;
    }
    // 实参列表创建完毕，创建call指令
    latest_value = builder->create_call(Func, true_params);
}

void IRBuilder::visit(SyntaxTree::IfStmt &node) {
    // 由于if可能是嵌套的，所以在进入一次此函数时需要将与if有关的全局变量暂存，退出时再恢复
    // 这样能保证全局变量一直指示的是当前信息
    // 暂存全局变量
    std::tuple<Ptr<BasicBlock>, Ptr<BasicBlock>, Ptr<BasicBlock>> temp_BBs = std::make_tuple(TrueBB, FalseBB, NextBB);
    // 创建此if相关的基本块
    std::string BB_id_string;
    BB_id_string = "TrueBB";
    BB_id_string += std::to_string(BB_id++);
    TrueBB = BasicBlock::create(module, BB_id_string, CurrentFunction);
    BB_id_string = "FalseBB";
    BB_id_string += std::to_string(BB_id++);
    FalseBB = BasicBlock::create(module, BB_id_string, CurrentFunction);
    if(!node.else_statement)
    { // 看是否有else分支，这里是没有
      // 没有则FalseBB和NextBB相同
        NextBB = FalseBB;
    }
    else 
    {
      // 重新创建NextBB
        BB_id_string = "NextBB";
        BB_id_string += std::to_string(BB_id++);
        NextBB = BasicBlock::create(module, BB_id_string, CurrentFunction);
    }

    // 递归访问 AST 上 ifstmt 中的 cond_exp
    node.cond_exp->accept(*this);
    // 根据返回后 latest_value 的值判断，同while部分的处理
    // 由于cond_exp可能是int或者float等类型，即（）中不是一个bool值，所以需要
    // 再进行一次比较，得到 bool 值
    if(latest_value->get_type() != INT1_T)
    {
        if(latest_value->get_type() == INT32_T)
        {   // 再进行一次int的eq比较，得到 INT1_T
            latest_value = builder->create_icmp_ne(latest_value, CONST_INT(0));
        }
        else if(latest_value->get_type() == FLOAT_T)
        {   // float 再比较一次得到 INT1_T
            latest_value = builder->create_fcmp_ne(latest_value, CONST_FLOAT(0));
        }
    }
    if(latest_value->get_type() != VOID_T)
    {   // 是空类型则说明cond_br已经生成过了
        latest_value = builder->create_cond_br(latest_value, TrueBB, FalseBB);
    }
    // 当前所处块变为 TrueBB
    CurrentBB = TrueBB;
    // 插入 TrueBB 的 label
    builder->set_insert_point(TrueBB);
    // 递归访问 AST 上 ifstmt 中的 if_stmt
    node.if_statement->accept(*this);
    // 是不是终止指令（如ret），不是才需要加br
    if(!CurrentBB->get_terminator())
    {
        latest_value = builder->create_br(NextBB);
    }
    // 处理 else 分支
    if(node.else_statement != nullptr)
    {
        // 当前所处块变为 FalseBB
        CurrentBB = FalseBB;
        // 插入 FalseBB 的 label
        builder->set_insert_point(FalseBB);
        // 递归访问 AST 上 ifstmt 中的 else_stmt
        node.else_statement->accept(*this);
        // 是不是终止指令（如ret），不是才需要加br
        if(!CurrentBB->get_terminator())
        {
            latest_value = builder->create_br(NextBB);
        }
    }
    // 有 else 分支
    if(node.else_statement)
    {
        // 当前所处块变为 NextBB
        CurrentBB = NextBB;
        // 插入 NextBB 的 label
        builder->set_insert_point(NextBB);
    }
    
    // 退出此if，需要恢复之前暂存的全局变量
    TrueBB = std::get<0>(temp_BBs);
    FalseBB = std::get<1>(temp_BBs);
    NextBB = std::get<2>(temp_BBs);
}

void IRBuilder::visit(SyntaxTree::WhileStmt &node) {
    // 由于while循环可能是嵌套的，所以在进入一次此函数时需要将与while循环有关的全局变量暂存，退出时再恢复
    // 这样能保证全局变量一直指示的是当前信息
    // 暂存全局变量
    std::tuple<Ptr<BasicBlock>, Ptr<BasicBlock>, Ptr<BasicBlock>, Ptr<BasicBlock>> temp_BBs= 
            std::make_tuple(CondBB, TrueBB, FalseBB, NextBB);
    // 创建此while循环相关的基本块
    std::string BB_id_string;
    BB_id_string = "CondBB";
    BB_id_string += std::to_string(BB_id++);
    CondBB = BasicBlock::create(module, BB_id_string, CurrentFunction);
    BB_id_string = "TrueBB";
    BB_id_string += std::to_string(BB_id++);
    TrueBB = BasicBlock::create(module, BB_id_string, CurrentFunction);
    BB_id_string = "FalseBB";
    BB_id_string += std::to_string(BB_id++);
    FalseBB = BasicBlock::create(module, BB_id_string, CurrentFunction);
    // while中 NextBB = FalseBB
    NextBB = FalseBB;

    // 开始创建 while 循环的 IR，先进入 CondBB
    latest_value = builder->create_br(CondBB);
    // 当前所处块变为 CondBB
    CurrentBB = CondBB;
    // 插入 CondBB 的 label
    builder->set_insert_point(CondBB);
    // 递归访问 AST 上 whilestmt 中的 cond_exp
    // 在Expr函数中builder后会更新latest_value的值，通过它可以知道表达式类型
    node.cond_exp->accept(*this);
    // 由于cond_exp可能是int或者float等类型，即（）中不是一个bool值，所以需要
    // 再进行一次比较，得到 bool 值
    if(latest_value->get_type() != INT1_T)
    {
        if(latest_value->get_type() == INT32_T)
        {   // 再进行一次int的eq比较，得到 INT1_T
            latest_value = builder->create_icmp_ne(latest_value, CONST_INT(0));
        }
        else if(latest_value->get_type() == FLOAT_T)
        {   // float 再比较一次得到 INT1_T
            latest_value = builder->create_fcmp_ne(latest_value, CONST_FLOAT(0));
        }
    }
    if(latest_value->get_type() != VOID_T)
    {   // 是空类型则说明cond_br已经生成过了
        latest_value = builder->create_cond_br(latest_value, TrueBB, FalseBB);
    }
    // 当前所处块变为 TrueBB
    CurrentBB = TrueBB;
    // 插入 TrueBB 的 label
    builder->set_insert_point(TrueBB);
    // 递归访问 AST 上 whilestmt 中的 stmt
    node.statement->accept(*this);
    // 接下来需要无条件转回CondBB，但是需要先判断TrueBB的最后一条指令
    // 是不是终止指令（如ret），不是才需要加br
    if(!CurrentBB->get_terminator())
    {
        latest_value = builder->create_br(CondBB);
    }
    // 然后是 whilestmt 的下一个基本块
    // 当前所处块变为 NextBB
    CurrentBB = NextBB;
    // 插入 NextBB 的 label
    builder->set_insert_point(NextBB);
    // 退出此while，需要恢复之前暂存的全局变量
    CondBB = std::get<0>(temp_BBs);
    TrueBB = std::get<1>(temp_BBs);
    FalseBB = std::get<2>(temp_BBs);
    NextBB = std::get<3>(temp_BBs);    
}

void IRBuilder::visit(SyntaxTree::BreakStmt &node) {
    // 无条件跳转到当前while循坏的下一个基本块
    latest_value = builder->create_br(NextBB);
}

void IRBuilder::visit(SyntaxTree::ContinueStmt &node) {
    // 无条件返回到当前while循坏的条件基本块
    latest_value = builder->create_br(CondBB);
}

}
}
