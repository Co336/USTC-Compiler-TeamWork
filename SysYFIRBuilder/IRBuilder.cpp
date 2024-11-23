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
bool LVal_retPtr;                     // 需要LVal函数返回标识符对应的值
bool LVal_retValue;                   // 需要LVal函数返回标识符对应的指针
Ptr<Value> latest_value = nullptr;    // 指示当前最近处理的表达式的值
Ptr<Value> latest_ptr   = nullptr;    // 指示当前最近处理的标识符的指针
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

// 初始化向量
typedef struct InitItem
{
    bool value_flag;                  // 表示当前初始化项是否是单一值（true 表示是单一值，false 表示嵌套列表）
    Value *expr;                      // 指向表达式对应的 LLVM IR 值，如果是单一值则存储该值；如果是嵌套列表则此值为空
    std::vector<InitItem> list;       // 嵌套的初始化项列表，当 value_flag 为 false 时有效，表示初始化为一个复合类型
} InitItem;


InitItem last_InitItem;               // 存最近生成的初始化项，作为遍历和构造递归结构时的中间结果

void IRBuilder::visit(SyntaxTree::InitVal &node) {
    InitItem now_init_item;
    // 如果节点是一个表达式类型的初始化值
    if (node.isExp)
    {
        node.expr->accept(*this);            // 递归访问表达式节点，生成对应的 LLVM IR
        now_init_item.value_flag = true;    // 标记当前初始化项为单一值
        now_init_item.expr = recent_value;  // 将生成的值赋给当前项的 expr 字段
    }
    else
    {
        // 如果节点是一个嵌套的初始化列表
        std::vector<InitItem> element_list; // 定义一个列表，存储子项
        for (auto &child_node : node.elementList)
        {
            child_node->accept(*this);       // 递归访问每个子节点，生成对应的初始化项
            element_list.emplace_back(last_InitItem); // 将生成的子项加入列表
        }
        now_init_item.value_flag = false;   // 标记当前初始化项为嵌套列表
        now_init_item.list = std::move(element_list); // 将子项列表移动到当前项的 list 字段
    }
    last_InitItem = now_init_item;
}

void IRBuilder::visit(SyntaxTree::FuncDef &node) {
    auto fn = Function::create(FunctionType::create(INT32_T, {}, this->module), "main", this->module);
    this->cur_func = fn;
    auto entry = BasicBlock::create(this->module, "entry", fn);
    builder->set_insert_point(entry);
    node.body->accept(*this);
}

void IRBuilder::visit(SyntaxTree::FuncFParamList &node) {}

void IRBuilder::visit(SyntaxTree::FuncParam &node) {}

//  To be done: 这个函数没有完成(所有初始化都没有做， 也没有考虑数组)， 只是实现了一个非常非常粗糙的版本用来测试后面的一些visit函数。
void IRBuilder::visit(SyntaxTree::VarDef &node) {
    if(scope.in_global()) {
		if(node.btype == SyntaxTree::Type::INT) {
			auto zero_initializer = ConstantZero::create(INT32_T, module);
			auto GlobalAlloca = GlobalVariable::create(node.name, module, INT32_T, false, zero_initializer);
			scope.push(node.name, GlobalAlloca);
		} else {
			auto zero_initializer = ConstantZero::create(FLOAT_T, module);
			auto GlobalAlloca = GlobalVariable::create(node.name, module, FLOAT_T, false, zero_initializer);
			scope.push(node.name, GlobalAlloca);
		}
		
	} else {
		if(node.btype == SyntaxTree::Type::INT) {
			auto VarAlloca = builder->create_alloca(INT32_T);
			scope.push(node.name, VarAlloca);
		} else {
			auto VarAlloca = builder->create_alloca(FLOAT_T);
			scope.push(node.name, VarAlloca);
		}
	}
}

//  To be done: scope作用域的查找似乎是从大作用域往小作用域查找到第一个标识符返回， 
//      但是实际上我们应该取最近作用域， 需要到IRBuilder.h里面修改。
//  To be done: 暂时没有考虑数组标识符的存法， 也就是task2 中的{CONST_INT(0), CONST_INT(0)} 和 {CONST_INT(0)}
void IRBuilder::visit(SyntaxTree::LVal &node) {
    //  在VarDef时需要将<name, Ptr<value> >一起压入当前作用域中。
    if(node.array_index.size() == 0) {
        // Lval -> Ident
        auto tmpPtr = scope.find(node.name, false);
        //  需要保持Lval_retPtr 和 LVal_retValue 只有一个为1， 后面我会检查其他函数的情况
        if (LVal_retPtr) {
            //  这里说明调用者希望返回一个表达式左值， 我们需要返回的是对应标识符的指针，方便调用者进行赋值。
            //  在压栈的时候压的Ptr<value> 是alloc的空间， 也就是说这里本身作用域里储存的就是指针。
            latest_ptr = tmpPtr;
            return;
        } else if(LVal_retValue) {
            //  这里说明调用者是调用的exp 函数，然后进入到LVal 进行表达式的求值。
            //  我们需要先用Load 从指针中取出值， 再将值返回给调用者。
            auto tmpValue = builder->create_load(tmpPtr);
            latest_value = tmpValue;
            return;
        }
    } else {
        //  Lval -> Ident[exp]
        //  我们暂不考虑多维数组情况
        auto tmpArray = scope.find(node.name, false);

        //  处理数组下标
        auto tmpIndex = node.array_index[0];
        tmpIndex->accept(*this);
        //  latest_value 就是数组下标的值。 
        auto IndexType = latest_value->get_type();
        if(IndexType == FLOAT_T) {
            throw UnreachableException();
            return;
        }
        
        //  tmpIndex 一定是访问exp, 这里要求exp 返回的latest_value 一定是值(不能是指针)。 
        //  如果exp 返回的不一定是值， 有可能是指针， 告诉我一下这里需要类型检查:(
        auto tmpPtr = builder->create_gep(tmpArray, {CONST_INT(0), latest_value});
        if(LVal_retPtr) {
            //  这里和单标识符一样了
            latest_ptr = tmpPtr;
            return;
        } else {
            auto tmpValue = builder->create_load(latest_value);
            latest_value = tmpValue;
            return;
        }
    }
}

//  如果测试时这里出现问题请告诉我
void IRBuilder::visit(SyntaxTree::AssignStmt &node) {
    //  处理表达式的左值， 得到的应该是指针
    //  设置左值返回值
    LVal_retValue = 0;  LVal_retPtr = 1;
    node.target->accept(*this);
    auto TargetPtr = latest_ptr;

    //  获取右值， 访问exp， 过程中无论何时出现LVal 都直接返回值而不返回指针。
    LVal_retValue = 1; LVal_retPtr = 0;
    node.value->accept(*this);
    auto tmpValue = latest_value;
    
    //  TargetPtr, tmpValue 基类都是 Ptr<value>， 我们需要确定赋值号左右类型匹配。
    auto TargetType = TargetPtr->get_type()->get_pointer_element_type();
    auto ValueType = tmpValue->get_type();
    if(TargetType == INT32_T && ValueType == INT32_T) {
        //  赋值号两边都是int32， 直接store
        builder->create_store(TargetPtr, tmpValue);
        return;
    } else if(TargetType == FLOAT_T && ValueType == FLOAT_T) {
        //  赋值号两边都是float， 直接store
        builder->create_store(TargetPtr, tmpValue);
    } else if(TargetType == INT32_T && ValueType == FLOAT_T) {
        //  FLOAT 转 INT 再store
        auto fti_res = builder->create_fptosi(tmpValue, INT32_T);
        builder->create_store(TargetPtr, tmpValue);
    } else if(TargetType == FLOAT_T && ValueType == INT32_T) {
        //  INT 转 FLOAT 再store
        auto itf_res = builder->create_sitofp(tmpValue, FLOAT_T);
        builder->create_store(TargetPtr, tmpValue);
    } else {
        //  基本类型只能为int | float， 我们不考虑给变量赋布尔值的情况。
        throw UnreachableException();
    }
}

//  因为我们开全局变量返回， 所以原本的visitee_val我替换成了latest_value
void IRBuilder::visit(SyntaxTree::Literal &node) {
    switch (node.literal_type)
    {
    case SyntaxTree::Type::INT: {
        //  change from visitee_val to latest_value
        latest_value = CONST_INT(node.int_const);
        break;
    }
    case SyntaxTree::Type::FLOAT: {
        //  change from visitee_val to latest_value
        latest_value = CONST_FLOAT(node.float_const);
        break;
    }
    default:
        throw UnreachableException();
        break;
    }
}

//  这里也是同样， 我们将原本的visitee_val 替换成 latest_value
//  To be done: 需要检查函数返回值类型和得到的ret参数类型是否一致， 如果不一致， 需要进行类型转换。
void IRBuilder::visit(SyntaxTree::ReturnStmt &node) {
    if(node.ret == nullptr) {
        //  void类型， 查了一下可以这么返回空。
        builder->create_void_ret();
    } else {
        node.ret->accept(*this);
        builder->create_ret(latest_value);
    }
}

//  To be done: 等上个厕所回来改， 我想统一开新作用域的位置， 就是在调用者的位置开作用域。
//          就比如函数就在FunDef开， 然后if和while在对应的stmt开， 等会儿回来去那边开一个， 这边block就不动了。
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
