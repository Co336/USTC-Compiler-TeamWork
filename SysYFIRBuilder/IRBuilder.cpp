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
bool func_block;                      // 指示FuncDef调用的block函数
//  To be done :这里进入FuncDef 需要把func_ret置0， 离开FuncDef时需要判断， 如果是在int main里面且没有return， 需要补充返回0。
bool func_ret;                        // 指示当前函数是否有返回标识
Ptr<Value> latest_value = nullptr;    // 指示当前最近处理的表达式的值
Ptr<Value> latest_ptr   = nullptr;    // 指示当前最近处理的标识符的指针
//  To be done :这里进入FuncDef 需要用tmpAlloc暂存retAlloc, 然后把retAlloc置为nullptr， returnstmt会把返回值存在retAlloc里再取出来返回，
//      离开FuncDef 的时候需要用tmpAlloc复原原本的retAlloc的值。 
Ptr<Value> retAlloc     = nullptr;    // FuncDef为返回值alloc的空间。
Ptr<BasicBlock> CondBB = nullptr;     // 指示当前while语句的条件基本块
Ptr<BasicBlock> NextBB = nullptr;     // 指示当前while循环和if语句的下一个基本块
Ptr<BasicBlock> TrueBB = nullptr;     // 指示当前条件判断的True分支基本块
Ptr<BasicBlock> FalseBB = nullptr;    // 指示当前条件判断的False分支基本块
Ptr<BasicBlock> CurrentBB = nullptr;  // 指示当前所处的基本块位置
Ptr<BasicBlock> retBB   = nullptr;    // 指示函数返回值基本块的位置， 在retBB中统一处理函数返回。
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
        builder->create_store(tmpValue, TargetPtr);
        return;
    } else if(TargetType == FLOAT_T && ValueType == FLOAT_T) {
        //  赋值号两边都是float， 直接store
        builder->create_store(tmpValue, TargetPtr);
    } else if(TargetType == INT32_T && ValueType == FLOAT_T) {
        //  FLOAT 转 INT 再store
        auto fti_res = builder->create_fptosi(tmpValue, INT32_T);
        builder->create_store(fti_res, TargetPtr);
    } else if(TargetType == FLOAT_T && ValueType == INT32_T) {
        //  INT 转 FLOAT 再store
        auto itf_res = builder->create_sitofp(tmpValue, FLOAT_T);
        builder->create_store(itf_res, TargetPtr);
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
//  一个基本块一个ret， 所以create_ret统一交给FuncDef处理
void IRBuilder::visit(SyntaxTree::ReturnStmt &node) {
    func_ret = 1;
    auto curFun = this->cur_func;
    //  存的cur_func是Ptr<Function> 类型， 里面内置了get_return_type函数用来获取返回值类型。
    auto FuncRetType = curFun->get_return_type();

    if(node.ret == nullptr) {
        if(FuncRetType != VOID_T) {
            //  非 void 函数需要返回值
            throw UnreachableException();
        }
        //  void类型， 查了一下可以这么返回空。
        builder->create_br(retBB);
    } else {
        node.ret->accept(*this);
        auto RetValueType = latest_value->get_type();
        //  需要检查函数返回值类型和得到的ret参数类型是否一致， 如果不一致， 需要进行类型转换。
        if(FuncRetType == INT32_T && RetValueType == INT32_T) {
            builder->create_store(latest_value, retAlloc);
        } else if(FuncRetType == FLOAT_T && RetValueType == FLOAT_T) {
            //  赋值号两边都是float， 直接store
            builder->create_store(latest_value, retAlloc);
        } else if(FuncRetType == INT32_T && RetValueType == FLOAT_T) {
            //  FLOAT 转 INT 再store
            auto fti_res = builder->create_fptosi(latest_value, INT32_T);
            builder->create_store(fti_res, retAlloc);
        } else if(FuncRetType == FLOAT_T && RetValueType == INT32_T) {
            //  INT 转 FLOAT 再store
            auto itf_res = builder->create_sitofp(latest_value, FLOAT_T);
            builder->create_store(itf_res, retAlloc);
        } else {
            //  基本类型只能为int | float， 我们不考虑给变量赋布尔值的情况。
            throw UnreachableException();
        }

        builder->create_br(retBB);
    }
}

void IRBuilder::visit(SyntaxTree::BlockStmt &node) {
    if(!func_block) {
        //  Func的作用域在Func进， 其余的都在block进， 包括if， while和直接的大括号
        scope.enter();
        for (auto stmt: node.body) {
            stmt->accept(*this);
        }
        scope.exit();
    } else {
        //  这里标识是FuncDef调用的block， 不需要进scope， 并且重置func_block， 防止后续stmt的block被误认。
        func_block = false;
        for (auto stmt: node.body) {
            stmt->accept(*this);
        }
    }
}

void IRBuilder::visit(SyntaxTree::EmptyStmt &node) {}

void IRBuilder::visit(SyntaxTree::ExprStmt &node) {
    node.exp->accept(*this);
}

void IRBuilder::visit(SyntaxTree::UnaryCondExpr &node) {
    // 单目条件运算，只能是 NOT
    if(node.op != SyntaxTree::UnaryCondOp::NOT)
        return;

    std::tuple<Ptr<BasicBlock>, Ptr<BasicBlock>> temp_BBS = std::make_tuple(TrueBB, FalseBB);
    // 先暂存 TrueBB 和 FalseBB，因为在访问操作数时可能改变他们的值
    // 因为括号中的整个条件可能很复杂，然后访问操作数时，由于需要短路计算
    // 所以可能会改变全局变量的值，使得全局变量的值指示当时的情况，为了能够恢复之前的状态，需要暂存
    // 将NOT去掉后，True和False情况会相反
    // 更新当前true,fasle分支
    TrueBB = std::get<1>(temp_BBS);
    FalseBB = std::get<0>(temp_BBS);
    // 访问操作数
    node.rhs->accept(*this);
    //得到新的 latest_value
    auto value_temp = latest_value;
    // 可能是INT1_T，先进行类型转换
    if(value_temp->get_type() == INT1_T)
    {
        value_temp = builder->create_zext(value_temp, INT32_T);
    }
    //发射比较指令
    if(value_temp->get_type() == INT32_T)
    {
        latest_value = builder->create_icmp_eq(value_temp, CONST_INT(0));
    }
    else if(value_temp->get_type() == FLOAT_T)
    {
        latest_value = builder->create_fcmp_eq(value_temp, CONST_FLOAT(0));
    }

    // 恢复
    TrueBB = std::get<0>(temp_BBS);
    FalseBB = std::get<1>(temp_BBS);
}

void IRBuilder::visit(SyntaxTree::BinaryCondExpr &node) {
    // 这里需要处理 AND 和 OR 的短路计算
    // 方法是先根据操作符的类型去处理左操作数
    // 对于 OR , 处理左操作数时的 False 分支应该为进入右操作数处理的分支，True 分支为整个条件的 True 分支，从而可以跳过右操作数的计算
    // 对于 AND, 处理左操作数时的 True 分支应该为进入右操作数处理的分支，False 分支为整个条件的 False 分支，从而可以跳过右操作数的计算
    if(node.op == SyntaxTree::BinaryCondOp::LOR)
    {
        // 对于OR，处理左操作数时的 False 分支应该为进入右操作数处理的分支，True 分支为整个条件的 True 分支，从而可以跳过右操作数的计算
        // 需要加一个 RightBB_local，在左操作数为假时进入右操作数基本块
        // 所以对于左操作数，其falseBB不是整个条件的FalseBB，但是TrueBB是一样的
        // 所以保险起见，先暂存FasleBB
        Ptr<BasicBlock> tempBB = FalseBB;

        // 创建 RightBB_local基本块
        std::string BB_id_string;
        BB_id_string = "RightBB";
        BB_id_string += std::to_string(BB_id++);
        Ptr<BasicBlock> RightBB_local = BasicBlock::create(module, BB_id_string, CurrentFunction);
        // 左操作数的假分支为RightBB
        FalseBB = RightBB_local;
        node.lhs->accept(*this);
        // 得到左边的 latest_value
        // 可能不是bool类型，要先转成bool类型
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
            // 这里左边为假时需要继续看右边，否则直接跳到最后的TrueBB
            latest_value = builder->create_cond_br(latest_value, TrueBB, RightBB_local);
        }
        // 需要在访问右边之前，先恢复FalseBB，因为进入右边后仍会暂存
        FalseBB = tempBB;

        // 当前所处块变为 RightBB_local
        CurrentBB = RightBB_local;
        // 插入 RightBB_local 的 label
        builder->set_insert_point(RightBB_local);
        // 继续访问右边
        node.rhs->accept(*this);
        // 得到lateet_value
        // 可能不是bool类型，要先转成bool类型
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
            // 这里右边为假时需要去FalseBB，否则跳到最后的TrueBB
            latest_value = builder->create_cond_br(latest_value, TrueBB, FalseBB);
        }
    }
    else if(node.op == SyntaxTree::BinaryCondOp::LAND)
    {
        // 对于AND，处理左操作数时的 True 分支应该为进入右操作数处理的分支，False 分支为整个条件的 False 分支，从而可以跳过右操作数的计算
        // 需要加一个 RightBB_local，在左操作数为真时进入右操作数基本块
        // 所以对于左操作数，其TrueBB不是整个条件的TrueBB，但是FalseBB是一样的
        // 所以保险起见，先暂存TrueBB
        Ptr<BasicBlock> tempBB = TrueBB;

        // 创建 RightBB_local基本块
        std::string BB_id_string;
        BB_id_string = "RightBB";
        BB_id_string += std::to_string(BB_id++);
        Ptr<BasicBlock> RightBB_local = BasicBlock::create(module, BB_id_string, CurrentFunction);
        // 左操作数的真分支为RightBB
        TrueBB = RightBB_local;
        node.lhs->accept(*this);
        // 得到左边的 latest_value
        // 可能不是bool类型，要先转成bool类型
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
            // 这里左边为真时需要继续看右边，否则直接跳到最后的FalseBB
            latest_value = builder->create_cond_br(latest_value, RightBB_local, FalseBB);
        }
        // 需要在访问右边之前，先恢复TrueBB，因为进入右边后仍会暂存
        TrueBB = tempBB;

        // 当前所处块变为 RightBB_local
        CurrentBB = RightBB_local;
        // 插入 RightBB_local 的 label
        builder->set_insert_point(RightBB_local);
        // 继续访问右边
        node.rhs->accept(*this);
        // 得到lateet_value
        // 可能不是bool类型，要先转成bool类型
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
            // 这里右边为真时需要去TrueBB，否则跳到最后的FalseBB
            latest_value = builder->create_cond_br(latest_value, TrueBB, FalseBB);
        }
    }
    else
    {
        // 为关系算符
        // 得到左右操作数的值
        node.lhs->accept(*this);
        auto l_value_temp = latest_value;
        node.rhs->accept(*this);
        auto r_value_temp = latest_value;

        // 先进行类型转换，再进行比较
        // 这里可能会有INT1_T类型的存在，需要考虑多种情况
        if(l_value_temp->get_type() == INT1_T && r_value_temp->get_type() == INT1_T)
        {
            //均扩展到INT32_T再进行比较
            l_value_temp = builder->create_zext(l_value_temp, INT32_T);
            r_value_temp = builder->create_zext(r_value_temp, INT32_T);
        }
        else if(l_value_temp->get_type() == INT1_T && r_value_temp->get_type() == INT32_T)
        {
            //将l扩展到32
            l_value_temp = builder->create_zext(l_value_temp, INT32_T);
        }   
        else if(l_value_temp->get_type() == INT32_T && r_value_temp->get_type() == INT1_T)
        {
            //将r扩展到32
            r_value_temp = builder->create_zext(r_value_temp, INT32_T);
        } 
        else if(l_value_temp->get_type() == INT1_T && r_value_temp->get_type() == FLOAT_T)
        {
            //将l变为FLOAT
            l_value_temp = builder->create_zext(l_value_temp, INT32_T);
            l_value_temp = builder->create_sitofp(l_value_temp, FLOAT_T);
        } 
        else if(l_value_temp->get_type() == FLOAT_T && r_value_temp->get_type() == INT1_T)
        {
            //将r变为FLOAT
            r_value_temp = builder->create_zext(r_value_temp, INT32_T);
            r_value_temp = builder->create_sitofp(r_value_temp, FLOAT_T);
        } 
        else if(l_value_temp->get_type() == INT32_T && r_value_temp->get_type() == FLOAT_T)
        {
            //将l变为FLOAT
            l_value_temp = builder->create_sitofp(l_value_temp, FLOAT_T);
        } 
        else if(l_value_temp->get_type() == FLOAT_T && r_value_temp->get_type() == INT32_T)
        {
            //将r变为FLOAT
            r_value_temp = builder->create_sitofp(r_value_temp, FLOAT_T);
        }
        // 现在类型一致，发射指令 
        if(l_value_temp->get_type() == FLOAT_T)
        {
            switch(node.op)
            {
                case SyntaxTree::BinaryCondOp::EQ:
                    latest_value = builder->create_fcmp_eq(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinaryCondOp::NEQ:
                    latest_value = builder->create_fcmp_ne(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinaryCondOp::GT:
                    latest_value = builder->create_fcmp_gt(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinaryCondOp::GTE:
                    latest_value = builder->create_fcmp_ge(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinaryCondOp::LT:
                    latest_value = builder->create_fcmp_lt(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinaryCondOp::LTE:
                    latest_value = builder->create_fcmp_le(l_value_temp, r_value_temp);
                    break;
                default:
                    break;
            }
        }
        else if(l_value_temp->get_type() == INT32_T)
        {
            switch(node.op)
            {
                case SyntaxTree::BinaryCondOp::EQ:
                    latest_value = builder->create_icmp_eq(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinaryCondOp::NEQ:
                    latest_value = builder->create_icmp_ne(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinaryCondOp::GT:
                    latest_value = builder->create_icmp_gt(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinaryCondOp::GTE:
                    latest_value = builder->create_icmp_ge(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinaryCondOp::LT:
                    latest_value = builder->create_icmp_lt(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinaryCondOp::LTE:
                    latest_value = builder->create_icmp_le(l_value_temp, r_value_temp);
                    break;
                default:
                    break;
            }
        }
    }
}

// to do
// 操作数可能出现INT1_T的情况，但是我暂时未添加这个判断，感觉是不会出现的
void IRBuilder::visit(SyntaxTree::BinaryExpr &node) {
    // 双目运算 加 减 乘 除 模
    node.lhs->accept(*this);
    // 得到返回后更新的latest_value值
    auto l_value_temp = latest_value;
    node.rhs->accept(*this);
    // 得到返回后更新的latest_value值)
    auto r_value_temp = latest_value;
    // 下面先处理常量情况，再处理变量情况
    // 两操作数都是常量，则不用发射指令，直接更新latest_value值
    // 四种情况
    if(std::dynamic_pointer_cast<ConstantFloat>(l_value_temp) && std::dynamic_pointer_cast<ConstantFloat>(r_value_temp))
    {
        auto l_value = std::dynamic_pointer_cast<ConstantFloat>(l_value_temp)->get_value();
        auto r_value = std::dynamic_pointer_cast<ConstantFloat>(r_value_temp)->get_value();
        //两边都是常量浮点数
        switch(node.op)
        {
            case SyntaxTree::BinOp::PLUS:
                latest_value = CONST_FLOAT(l_value + r_value);
                break;
            case SyntaxTree::BinOp::MINUS:
                latest_value = CONST_FLOAT(l_value - r_value);
                break;
            case SyntaxTree::BinOp::MULTIPLY:
                latest_value = CONST_FLOAT(l_value * r_value);
                break;
            case SyntaxTree::BinOp::DIVIDE:
                latest_value = CONST_FLOAT(l_value / r_value);
                break;
            default:
                break;
        }
    }
    else if(std::dynamic_pointer_cast<ConstantFloat>(l_value_temp) && std::dynamic_pointer_cast<ConstantInt>(r_value_temp))
    {
        auto l_value = std::dynamic_pointer_cast<ConstantFloat>(l_value_temp)->get_value();
        auto r_value = std::dynamic_pointer_cast<ConstantInt>(r_value_temp)->get_value();
        //左边是常量浮点数，右边是常量整数
        switch(node.op)
        {
            case SyntaxTree::BinOp::PLUS:
                latest_value = CONST_FLOAT(l_value + float(r_value));
                break;
            case SyntaxTree::BinOp::MINUS:
                latest_value = CONST_FLOAT(l_value - float(r_value));
                break;
            case SyntaxTree::BinOp::MULTIPLY:
                latest_value = CONST_FLOAT(l_value * float(r_value));
                break;
            case SyntaxTree::BinOp::DIVIDE:
                latest_value = CONST_FLOAT(l_value / float(r_value));
                break;
            default:
                break;
        }
    }
    else if(std::dynamic_pointer_cast<ConstantInt>(l_value_temp) && std::dynamic_pointer_cast<ConstantFloat>(r_value_temp))
    {
        auto l_value = std::dynamic_pointer_cast<ConstantInt>(l_value_temp)->get_value();
        auto r_value = std::dynamic_pointer_cast<ConstantFloat>(r_value_temp)->get_value();
        //右边是常量浮点数，左边是常量整数
        switch(node.op)
        {
            case SyntaxTree::BinOp::PLUS:
                latest_value = CONST_FLOAT(r_value + float(l_value));
                break;
            case SyntaxTree::BinOp::MINUS:
                latest_value = CONST_FLOAT(float(l_value) - r_value);
                break;
            case SyntaxTree::BinOp::MULTIPLY:
                latest_value = CONST_FLOAT(r_value * float(l_value));
                break;
            case SyntaxTree::BinOp::DIVIDE:
                latest_value = CONST_FLOAT(float(l_value) / r_value);
                break;
            default:
                break;
        }
    }
    else if(std::dynamic_pointer_cast<ConstantInt>(l_value_temp) && std::dynamic_pointer_cast<ConstantInt>(r_value_temp))
    {
        auto l_value = std::dynamic_pointer_cast<ConstantInt>(l_value_temp)->get_value();
        auto r_value = std::dynamic_pointer_cast<ConstantInt>(r_value_temp)->get_value();
        //左边是常量整数，右边是常量整数
        switch(node.op)
        {
            case SyntaxTree::BinOp::PLUS:
                latest_value = CONST_INT(r_value + l_value);
                break;
            case SyntaxTree::BinOp::MINUS:
                latest_value = CONST_INT(l_value - r_value);
                break;
            case SyntaxTree::BinOp::MULTIPLY:
                latest_value = CONST_INT(r_value * l_value);
                break;
            case SyntaxTree::BinOp::DIVIDE:
                latest_value = CONST_INT(l_value / r_value);
                break;
            case SyntaxTree::BinOp::MODULO:
                latest_value = CONST_INT(l_value % r_value);
            default:
                break;
        }
    }
    else
    {
        //有变量，则发射指令
        //左右的类型可能不一样，要先进行类型转换
        if(l_value_temp->get_type() == INT32_T && r_value_temp->get_type() == FLOAT_T)
        {
            l_value_temp = builder->create_sitofp(l_value_temp, FLOAT_T);
        }
        else if(r_value_temp->get_type() == INT32_T && l_value_temp->get_type() == FLOAT_T)
        {
            r_value_temp = builder->create_sitofp(r_value_temp, FLOAT_T);
        }
        
        // 左右类型一致，发射指令
        if(l_value_temp->get_type() == FLOAT_T)
        {
            switch(node.op)
            {
                case SyntaxTree::BinOp::PLUS:
                    latest_value = builder->create_fadd(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinOp::MINUS:
                    latest_value = builder->create_fsub(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinOp::MULTIPLY:
                    latest_value = builder->create_fmul(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinOp::DIVIDE:
                    latest_value = builder->create_fdiv(l_value_temp, r_value_temp);
                    break;
                default:
                    break;
            }
        }
        else
        {
            switch(node.op)
            {
                case SyntaxTree::BinOp::PLUS:
                    latest_value = builder->create_iadd(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinOp::MINUS:
                    latest_value = builder->create_isub(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinOp::MULTIPLY:
                    latest_value = builder->create_imul(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinOp::DIVIDE:
                    latest_value = builder->create_isdiv(l_value_temp, r_value_temp);
                    break;
                case SyntaxTree::BinOp::MODULO:
                    latest_value = builder->create_isrem(l_value_temp, r_value_temp);
                default:
                    break;
            }
        }
    }
}

// to do
// 操作数可能出现INT1_T的情况，但是我暂时未添加这个判断，感觉是不会出现的
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
            latest_value = CONST_FLOAT(-1.0 * value);
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
            latest_value = CONST_INT(-1 * value);
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
    // 当前所处块变为 NextBB
    CurrentBB = NextBB;
    // 插入 NextBB 的 label
    builder->set_insert_point(NextBB);
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
