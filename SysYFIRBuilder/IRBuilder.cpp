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
        std::vector<Ptr<Type>> Params;        // 函数形参类型表
        std::vector<std::string> Param_names; // 函数形参名表

        bool LVal_retPtr;   // 需要LVal函数返回标识符对应的值
        bool LVal_retValue; // 需要LVal函数返回标识符对应的指针
        bool func_block;    // 指示FuncDef调用的block函数
        //  To be done :这里进入FuncDef 需要把func_ret置0， 离开FuncDef时需要判断， 如果是在int main里面且没有return， 需要补充返回0。
        bool func_ret;                     // 指示当前函数是否有返回标识
        Ptr<Value> latest_value = nullptr; // 指示当前最近处理的表达式的值
        Ptr<Value> latest_ptr = nullptr;   // 指示当前最近处理的标识符的指针
        //  To be done :这里进入FuncDef 需要用tmpAlloc暂存retAlloc, 然后把retAlloc置为nullptr， returnstmt会把返回值存在retAlloc里再取出来返回，
        //      离开FuncDef 的时候需要用tmpAlloc复原原本的retAlloc的值。
        Ptr<Value> retAlloc = nullptr;       // FuncDef为返回值alloc的空间。
        Ptr<BasicBlock> CondBB = nullptr;    // 指示当前while语句的条件基本块
        Ptr<BasicBlock> NextBB = nullptr;    // 指示当前while循环和if语句的下一个基本块
        Ptr<BasicBlock> TrueBB = nullptr;    // 指示当前条件判断的True分支基本块
        Ptr<BasicBlock> FalseBB = nullptr;   // 指示当前条件判断的False分支基本块
        Ptr<BasicBlock> CurrentBB = nullptr; // 指示当前所处的基本块位置
        Ptr<BasicBlock> retBB = nullptr;     // 指示函数返回值基本块的位置， 在retBB中统一处理函数返回。
        long BB_id = 0;                      // 创建的BB的 label ：格式为 "......BB_<BB_id>"
        int Literal_Number;
        // to do ：在Fundef时需要将构造的fn赋给CurrentFunction，因为我后面BB的创建都是用的这个！！！！！！！！！
        Ptr<Function> CurrentFunction = nullptr; // 指示当前所处的函数

        void IRBuilder::visit(SyntaxTree::Assembly &node)
        {
            VOID_T = Type::get_void_type(module);
            INT1_T = Type::get_int1_type(module);
            INT32_T = Type::get_int32_type(module);
            FLOAT_T = Type::get_float_type(module);
            INT32PTR_T = Type::get_int32_ptr_type(module);
            FLOATPTR_T = Type::get_float_ptr_type(module);
            for (const auto &def : node.global_defs)
            {
                def->accept(*this);
            }
        }

        // TODO: You need to fill them.
        // NOTE: The following codes are just examples, you can modify them as you like.

        // 初始化向量
        typedef struct InitItem
        {
            bool value_flag;            // 表示当前初始化项是否是单一值（true 表示是单一值，false 表示嵌套列表）
            Ptr<Value> expr;            // 指向表达式对应的 LLVM IR 值，如果是单一值则存储该值；如果是嵌套列表则此值为空
            std::vector<InitItem> list; // 嵌套的初始化项列表，当 value_flag 为 false 时有效，表示初始化为一个复合类型
        } InitItem;

        InitItem last_InitItem; // 存最近生成的初始化项，作为遍历和构造递归结构时的中间结果

        
        void assignVal(Ptr<Value> TargetPtr, Ptr<Value> tmpValue, Ptr<IRStmtBuilder> builder) {
            //  TargetPtr, tmpValue 基类都是 Ptr<value>， 我们需要确定赋值号左右类型匹配。
            auto TargetType = TargetPtr->get_type()->get_pointer_element_type();
            auto ValueType = tmpValue->get_type();
            if (TargetType == INT32_T && ValueType == INT32_T)
            {
                //  赋值号两边都是int32， 直接store
                builder->create_store(tmpValue, TargetPtr);
                return;
            }
            else if (TargetType == FLOAT_T && ValueType == FLOAT_T)
            {
                //  赋值号两边都是float， 直接store
                builder->create_store(tmpValue, TargetPtr);
            }
            else if (TargetType == INT32_T && ValueType == FLOAT_T)
            {
                //  FLOAT 转 INT 再store
                auto fti_res = builder->create_fptosi(tmpValue, INT32_T);
                builder->create_store(fti_res, TargetPtr);
            }
            else if (TargetType == FLOAT_T && ValueType == INT32_T)
            {
                //  INT 转 FLOAT 再store
                auto itf_res = builder->create_sitofp(tmpValue, FLOAT_T);
                builder->create_store(itf_res, TargetPtr);
            }
            else
            {
                //  基本类型只能为int | float， 我们不考虑给变量赋布尔值的情况。
                throw UnreachableException();
            }
        }


        Ptr<Type> get_type(SyntaxTree::Type type, bool is_pointer)
        { // 不知道会不会和gettype()冲突？
            switch (type)
            {
            case SyntaxTree::Type::VOID:
                return VOID_T;
            case SyntaxTree::Type::BOOL:
                return INT1_T;
            case SyntaxTree::Type::INT:
                if (is_pointer)
                    return INT32PTR_T;
                else
                    return INT32_T;
            case SyntaxTree::Type::FLOAT:
                if (is_pointer)
                    return FLOATPTR_T;
                else
                    return FLOAT_T;
            default:
                return VOID_T;
            }
        }

        void IRBuilder::visit(SyntaxTree::InitVal &node)
        {
            InitItem now_init_item;
            // 如果节点是一个表达式类型的初始化值
            if (node.isExp)
            {
                node.expr->accept(*this);          // 递归访问表达式节点，生成对应的 LLVM IR
                now_init_item.value_flag = true;   // 标记当前初始化项为单一值
                now_init_item.expr = latest_value; // 将生成的值赋给当前项的 expr 字段
            }
            else
            {
                // 如果节点是一个嵌套的初始化列表
                std::vector<InitItem> element_list; // 定义一个列表，存储子项
                for (auto &child_node : node.elementList)
                {
                    child_node->accept(*this);                // 递归访问每个子节点，生成对应的初始化项
                    element_list.emplace_back(last_InitItem); // 将生成的子项加入列表
                }
                now_init_item.value_flag = false;             // 标记当前初始化项为嵌套列表
                now_init_item.list = std::move(element_list); // 将子项列表移动到当前项的 list 字段
            }
            last_InitItem = now_init_item;
        }

        void IRBuilder::visit(SyntaxTree::FuncDef &node)
        {
            // 标记当前为一个新的函数定义
            func_block = true;

            // 清空函数参数类型和名称的存储列表
            Params.clear();
            Param_names.clear();

            // 获取函数的返回类型
            Ptr<Type> ret_type = get_type(node.ret_type, false);

            // 如果函数有参数列表，处理参数
            if (node.param_list != nullptr)
            {
                node.param_list->accept(*this);
            }

            // 构造函数类型（包含返回类型和参数类型）
            Ptr<FunctionType> func_type = FunctionType::create(ret_type, Params, module);

            // 创建函数对象并设置为当前函数
            Ptr<Function> fn = Function::create(func_type, node.name, this->module);
            CurrentFunction = fn;
            scope.push(node.name, fn);

            // 准备函数形参
            std::vector<Ptr<Value>> args;
            for (auto arg = fn->arg_begin(); arg != fn->arg_end(); ++arg)
            {
                args.push_back(*arg);
            }

            // 进入函数作用域
            scope.enter();

            // 创建入口基本块并设置为当前基本块
            Ptr<BasicBlock> entry_bb = BasicBlock::create(this->module, "entry", fn);
            CurrentBB = entry_bb;
            builder->set_insert_point(entry_bb);

            // 为形参分配内存空间并存储
            for (int i = 0; i < (int)Param_names.size(); ++i)
            {
                auto alloc = builder->create_alloca(Params[i]);
                builder->create_store(args[i], alloc);
                scope.push(Param_names[i], alloc);
            }

            // 初始化返回相关变量
            retBB = BasicBlock::create(this->module, "ret", fn);
            Ptr<Value> tmpAlloc = retAlloc; // 临时保存 retAlloc
            retAlloc = (ret_type != VOID_T) ? builder->create_alloca(ret_type) : nullptr;

            // 设置 `func_ret` 为 false，表示尚未检测到返回
            func_ret = false;

            CurrentBB = retBB;
            // 设置返回块的插入点并生成返回指令
            builder->set_insert_point(retBB);
            if (ret_type == VOID_T)
            {
                // void 类型函数直接生成 void 返回指令
                builder->create_void_ret();
            }
            else
            {
                auto retLoad = builder->create_load(retAlloc);
                builder->create_ret(retLoad);
            }

            // 恢复插入点到入口块
            CurrentBB = entry_bb;
            builder->set_insert_point(entry_bb);

            // 处理函数体
            node.body->accept(*this);

            // 如果函数体内没有返回指令，补充默认返回
            if (!func_ret)
            {
                if (node.name == "main" && ret_type == INT32_T)
                {
                    // 特殊处理 main 函数，补充返回值 0
                    builder->create_store(CONST_INT(0), retAlloc);
                    builder->create_br(retBB);
                }
                else if (ret_type == VOID_T)
                {
                    // void 类型函数补充返回 void
                    builder->create_br(retBB);
                }
                else
                {
                    // 非 void 类型函数补充默认返回值 0
                    builder->create_store(CONST_INT(0), retAlloc);
                    builder->create_br(retBB);
                }
            }

            // 恢复 retAlloc 并退出作用域
            retAlloc = tmpAlloc;
            scope.exit();
        }

        void IRBuilder::visit(SyntaxTree::FuncFParamList &node)
        {
            for (auto param : node.params)
            {
                param->accept(*this);
            }
        }

        void IRBuilder::visit(SyntaxTree::FuncParam &node)
        {
            // 获取函数参数的类型
            Ptr<Type> paramType = get_type(node.param_type, false);

            // 判断是否为数组类型的参数
            if (!node.array_index.empty())
            {
                LVal_retValue = true; // 标记需要返回值指针

                // 逐层处理数组维度（从内到外）
                for (size_t i = node.array_index.size(); i-- > 0;)
                {
                    auto dimension = node.array_index[i];
                    dimension->accept(*this);

                    // 使用当前维度大小更新参数类型为数组类型
                    paramType = ArrayType::get(paramType, static_cast<ConstantInt *>(latest_value.get())->get_value());
                }

                LVal_retValue = false;

                // 将参数类型更新为指针类型
                paramType = PointerType::get(paramType);
            }

            // 将参数类型和参数名存入参数列表
            Params.push_back(paramType);
            Param_names.push_back(node.name);

        }

        
        //  To be done: 这个函数没有完成(所有初始化都没有做， 也没有考虑数组)， 只是实现了一个非常非常粗糙的版本用来测试后面的一些visit函数。
        void IRBuilder::visit(SyntaxTree::VarDef &node) {
            if (scope.in_global()) {
                if(node.array_length.empty()) {
                    if (node.btype == SyntaxTree::Type::INT) {
                        if(node.is_inited) { 
                            LVal_retPtr = 0; LVal_retValue = 1;
                            node.initializers->accept(*this);
                            auto var_initializer = last_InitItem.expr;
                            if(node.is_constant) {
                                auto GlobalAlloca = GlobalVariable::create(node.name, module, INT32_T, true, std::dynamic_pointer_cast<ConstantInt>(var_initializer));
                                scope.push(node.name, GlobalAlloca);
                            }
                                
                            else {
                                auto GlobalAlloca = GlobalVariable::create(node.name, module, INT32_T, false, std::dynamic_pointer_cast<ConstantInt>(var_initializer));
                                scope.push(node.name, GlobalAlloca);
                            }
                                
                            
                        } else {
                            auto zero_initializer = ConstantZero::create(INT32_T, module);
                            if(node.is_constant) {
                                auto GlobalAlloca = GlobalVariable::create(node.name, module, INT32_T, true, zero_initializer);
                                scope.push(node.name, GlobalAlloca);
                            }
                                
                            else {
                                auto GlobalAlloca = GlobalVariable::create(node.name, module, INT32_T, false, zero_initializer);
                                scope.push(node.name, GlobalAlloca);
                            }
                                
                            
                        }
                    }
                    else {
                        if(node.is_inited) { 
                            LVal_retPtr = 0; LVal_retValue = 1;
                            node.initializers->accept(*this);
                            auto var_initializer = last_InitItem.expr;
                            if(node.is_constant){ 
                                auto GlobalAlloca = GlobalVariable::create(node.name, module, FLOAT_T, true, std::dynamic_pointer_cast<ConstantInt>(var_initializer));
                                scope.push(node.name, GlobalAlloca);
                            }
                            else{
                                auto GlobalAlloca = GlobalVariable::create(node.name, module, FLOAT_T, false, std::dynamic_pointer_cast<ConstantInt>(var_initializer));
                                scope.push(node.name, GlobalAlloca);
                            }
                        } else {
                            auto zero_initializer = ConstantZero::create(FLOAT_T, module);
                            if(node.is_constant) {
                                auto GlobalAlloca = GlobalVariable::create(node.name, module, FLOAT_T, true, zero_initializer);
                                scope.push(node.name, GlobalAlloca);
                            }
                            else {
                                auto GlobalAlloca = GlobalVariable::create(node.name, module, FLOAT_T, false, zero_initializer);
                                scope.push(node.name, GlobalAlloca);
                            }
                        }
                    }
                }
                else {
                    LVal_retPtr = 0; LVal_retValue = 1;

                    node.array_length[0]->accept(*this);
                    auto arrayLength = latest_value;
                    auto arrayLenghtLiteral = Literal_Number;
                    std::vector<Ptr<Constant> > init_val;

                    if(node.is_inited) {
                        node.initializers->accept(*this);
                        for(auto item : last_InitItem.list) {
                            LVal_retValue = 1; LVal_retPtr = 0;
                            init_val.emplace_back(std::dynamic_pointer_cast<Constant>(item.expr));
                        }
                        if (node.btype == SyntaxTree::Type::INT) {
                            auto arrayType_num = ArrayType::get(INT32_T, arrayLenghtLiteral);
                            auto array_initializer = ConstantArray::create(arrayType_num, init_val, module);
                            if(node.is_constant) {
                                auto array = GlobalVariable::create(node.name, module, arrayType_num, true, array_initializer);//          是否是常量定义，初始化常量(ConstantZero类)
                                scope.push(node.name, array);
                            }
                            else {
                                auto array = GlobalVariable::create(node.name, module, arrayType_num, false, array_initializer);
                                scope.push(node.name, array);
                            }
                        }
                        else {
                            auto arrayType_num = ArrayType::get(FLOAT_T, arrayLenghtLiteral);
                            auto array_initializer = ConstantArray::create(arrayType_num, init_val, module);    
                            if(node.is_constant) {
                                auto array = GlobalVariable::create(node.name, module, arrayType_num, true, array_initializer);//          是否是常量定义，初始化常量(ConstantZero类)
                                scope.push(node.name, array);
                            }
                            else {
                                auto array = GlobalVariable::create(node.name, module, arrayType_num, false, array_initializer);
                                scope.push(node.name, array);
                            }
                        }
                    } else {
                        if (node.btype == SyntaxTree::Type::INT) {
                            auto zero_initializer = ConstantZero::create(INT32_T, module);
                            auto arrayType_num = ArrayType::get(INT32_T, arrayLenghtLiteral);
                            if(node.is_constant) {
                                auto array = GlobalVariable::create(node.name, module, arrayType_num, true, zero_initializer);// 参数解释：  名字name，所属module，全局变量类型type，
                                scope.push(node.name, array);
                            } else {
                                auto array = GlobalVariable::create(node.name, module, arrayType_num, false, zero_initializer);// 参数解释：  名字name，所属module，全局变量类型type，
                                scope.push(node.name, array);
                            }
                        } else {
                            auto zero_initializer = ConstantZero::create(FLOAT_T, module);
                            auto arrayType_num = ArrayType::get(FLOAT_T, arrayLenghtLiteral);
                            if(node.is_constant) {
                                auto array = GlobalVariable::create(node.name, module, arrayType_num, true, zero_initializer);// 参数解释：  名字name，所属module，全局变量类型type，
                                scope.push(node.name, array);
                            } else {
                                auto array = GlobalVariable::create(node.name, module, arrayType_num, false, zero_initializer);// 参数解释：  名字name，所属module，全局变量类型type，
                                scope.push(node.name, array);
                            }
                        }
                    }
                }
            }
            else {
                if(node.array_length.empty()) {
                    if (node.btype == SyntaxTree::Type::INT) {
                        auto VarAlloca = builder->create_alloca(INT32_T);
                        if(node.is_inited) {
                            LVal_retValue = 1;LVal_retPtr = 0;
                            node.initializers->accept(*this);
                            auto var_initializer = last_InitItem.expr;
                            assignVal(VarAlloca, var_initializer, builder);
                        }
                        scope.push(node.name, VarAlloca);
                    }
                    else {
                        auto VarAlloca = builder->create_alloca(FLOAT_T);
                        if(node.is_inited) {
                            node.initializers->accept(*this);
                            auto var_initializer = last_InitItem.expr;
                            assignVal(VarAlloca, var_initializer, builder);
                        }
                        scope.push(node.name, VarAlloca);
                    }
                }
                else {
                    LVal_retPtr = 0; LVal_retValue = 1;
                    node.array_length[0]->accept(*this);
                    auto arrayLength = latest_value;
                    auto arrayLenghtLiteral = Literal_Number;
                    std::vector<Ptr<Constant> > init_val;

                    if(node.is_inited) {
                        node.initializers->accept(*this);
                        for(auto item : last_InitItem.list) {
                            LVal_retValue = 1; LVal_retPtr = 0;
                            init_val.emplace_back(std::dynamic_pointer_cast<Constant>(item.expr));
                        }
                        if (node.btype == SyntaxTree::Type::INT) {
                            auto arrayType_num = ArrayType::get(INT32_T, arrayLenghtLiteral);
                            auto array = builder->create_alloca(arrayType_num); //          是否是常量定义，初始化常量(ConstantZero类)
                            for(int i = 0; i < init_val.size(); ++i) {
                                auto InitAlloca = builder->create_gep(array, {CONST_INT(0), CONST_INT(i)});
                                assignVal(InitAlloca, init_val[i], builder);
                            }
                            scope.push(node.name, array);
                        }
                        else {
                            auto arrayType_num = ArrayType::get(FLOAT_T, arrayLenghtLiteral);
                            auto array = builder->create_alloca(arrayType_num); //          是否是常量定义，初始化常量(ConstantZero类)
                            for(int i = 0; i < init_val.size(); ++i) {
                                auto InitAlloca = builder->create_gep(array, {CONST_INT(0), CONST_INT(i)});
                                assignVal(InitAlloca, init_val[i], builder);
                            }
                            scope.push(node.name, array);
                        }
                    } else {
                        if (node.btype == SyntaxTree::Type::INT) {
                            auto zero_initializer = ConstantZero::create(INT32_T, module);
                            auto arrayType_num = ArrayType::get(INT32_T, arrayLenghtLiteral);
                            auto array = builder->create_alloca(arrayType_num);
                            scope.push(node.name, array);
                        } else {
                            auto zero_initializer = ConstantZero::create(FLOAT_T, module);
                            auto arrayType_num = ArrayType::get(FLOAT_T, arrayLenghtLiteral);
                            auto array = builder->create_alloca(arrayType_num);
                            scope.push(node.name, array);
                        }
                    }
                }
                
            }
        }

        void IRBuilder::visit(SyntaxTree::LVal &node)
        {
            //  在VarDef时需要将<name, Ptr<value> >一起压入当前作用域中。

            //  处理数组index的时候也可能调用LVal， 所以这里可能嵌套， 先保存全局变量的值。
            auto tmp_LVal_retPtr = LVal_retPtr;
            auto tmp_LVal_retValue = LVal_retValue;

            if (node.array_index.empty())
            {
                // Lval -> Ident
                auto tmpPtr = scope.find(node.name, false);
                //  需要保持Lval_retPtr 和 LVal_retValue 只有一个为1， 后面我会检查其他函数的情况
                if (tmp_LVal_retPtr)
                {
                    //  这里说明调用者希望返回一个表达式左值， 我们需要返回的是对应标识符的指针，方便调用者进行赋值。
                    //  在压栈的时候压的Ptr<value> 是alloc的空间， 也就是说这里本身作用域里储存的就是指针。
                    latest_ptr = tmpPtr;
                    return;
                }
                else if (tmp_LVal_retValue)
                {
                    //  这里说明调用者是调用的exp 函数，然后进入到LVal 进行表达式的求值。
                    //  我们需要先用Load 从指针中取出值， 再将值返回给调用者
                    auto tmpValue = builder->create_load(tmpPtr);
                    latest_value = tmpValue;
                    return;
                }
            }
            else
            {
                //  Lval -> Ident[exp]
                //  我们暂不考虑多维数组情况
                auto tmpArray = scope.find(node.name, false);

                //  处理数组下标
                auto tmpIndex = node.array_index[0];
                tmpIndex->accept(*this);
                //  latest_value 就是数组下标的值。
                auto IndexType = latest_value->get_type();
                if (IndexType == FLOAT_T)
                {
                    throw UnreachableException();
                    return;
                }

                //  如果考虑到函数传参， 这里应该有两种tmpArray 的类型
                //  如果函数传参 int a[] , 符号表里推的应该是 INT32PTR_T, 我们取gep的时候就应该直接取偏移量，
                //    否则需要先用常数 0 进入数组第一维， 再取偏移量。
                if (tmpArray->as<Ptr<PointerType>>())
                {
                    // 说明tmpArray 是pointer类型， 那么我们直接走偏移量
                    auto tmpPtr = builder->create_gep(tmpArray, {latest_value});
                    if (tmp_LVal_retPtr)
                    {
                        //  这里和单标识符一样了
                        latest_ptr = tmpPtr;
                        return;
                    }
                    else if (tmp_LVal_retValue)
                    {
                        auto tmpValue = builder->create_load(latest_value);
                        latest_value = tmpValue;
                        return;
                    }
                }
                else if (tmpArray->as<Ptr<ArrayType>>())
                {
                    auto tmpPtr = builder->create_gep(tmpArray, {CONST_INT(0), latest_value});
                    if (tmp_LVal_retPtr)
                    {
                        //  这里和单标识符一样了
                        latest_ptr = tmpPtr;
                        return;
                    }
                    else if (tmp_LVal_retValue)
                    {
                        auto tmpValue = builder->create_load(latest_value);
                        latest_value = tmpValue;
                        return;
                    }
                }
            }

            LVal_retPtr = tmp_LVal_retPtr;
            LVal_retValue = tmp_LVal_retValue;
        }

        //  如果测试时这里出现问题请告诉我
        void IRBuilder::visit(SyntaxTree::AssignStmt &node)
        {
            //  处理表达式的左值， 得到的应该是指针
            //  设置左值返回值
            LVal_retValue = 0; LVal_retPtr = 1;
            node.target->accept(*this);
            auto TargetPtr = latest_ptr;

            //  获取右值， 访问exp， 过程中无论何时出现LVal 都直接返回值而不返回指针。
            LVal_retValue = 1; LVal_retPtr = 0;
            node.value->accept(*this);
            auto tmpValue = latest_value;

            assignVal(TargetPtr, tmpValue, builder);
        }

        //  因为我们开全局变量返回， 所以原本的visitee_val我替换成了latest_value
        void IRBuilder::visit(SyntaxTree::Literal &node)
        {
            switch (node.literal_type)
            {
            case SyntaxTree::Type::INT:
            {
                //  change from visitee_val to latest_value
                latest_value = CONST_INT(node.int_const);
                Literal_Number = node.int_const;
                break;
            }
            case SyntaxTree::Type::FLOAT:
            {
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
        void IRBuilder::visit(SyntaxTree::ReturnStmt &node)
        {
            func_ret = 1;
            auto curFun = CurrentFunction;
            //  存的cur_func是Ptr<Function> 类型， 里面内置了get_return_type函数用来获取返回值类型。
            auto FuncRetType = curFun->get_return_type();

            if (node.ret == nullptr)
            {
                if (FuncRetType != VOID_T)
                {
                    //  非 void 函数需要返回值
                    throw UnreachableException();
                }
                //  void类型， 查了一下可以这么返回空。
                builder->create_br(retBB);
            }
            else
            {
                LVal_retValue = 1; LVal_retPtr = 0;
                node.ret->accept(*this);
                auto RetValueType = latest_value->get_type();
                //  需要检查函数返回值类型和得到的ret参数类型是否一致， 如果不一致， 需要进行类型转换。
                if (FuncRetType == INT32_T && RetValueType == INT32_T)
                {
                    builder->create_store(latest_value, retAlloc);
                }
                else if (FuncRetType == FLOAT_T && RetValueType == FLOAT_T)
                {
                    //  赋值号两边都是float， 直接store
                    builder->create_store(latest_value, retAlloc);
                }
                else if (FuncRetType == INT32_T && RetValueType == FLOAT_T)
                {
                    //  FLOAT 转 INT 再store
                    auto fti_res = builder->create_fptosi(latest_value, INT32_T);
                    builder->create_store(fti_res, retAlloc);
                }
                else if (FuncRetType == FLOAT_T && RetValueType == INT32_T)
                {
                    //  INT 转 FLOAT 再store
                    auto itf_res = builder->create_sitofp(latest_value, FLOAT_T);
                    builder->create_store(itf_res, retAlloc);
                }
                else
                {
                    //  基本类型只能为int | float， 我们不考虑给变量赋布尔值的情况。
                    throw UnreachableException();
                }

                builder->create_br(retBB);
            }
        }

        void IRBuilder::visit(SyntaxTree::BlockStmt &node)
        {
            if (!func_block)
            {
                //  Func的作用域在Func进， 其余的都在block进， 包括if， while和直接的大括号
                scope.enter();
                for (auto stmt : node.body)
                {
                    stmt->accept(*this);
                }
                scope.exit();
            }
            else
            {
                //  这里标识是FuncDef调用的block， 不需要进scope， 并且重置func_block， 防止后续stmt的block被误认。
                func_block = false;
                for (auto stmt : node.body)
                {
                    stmt->accept(*this);
                }
            }
        }

        void IRBuilder::visit(SyntaxTree::EmptyStmt &node) {}

        void IRBuilder::visit(SyntaxTree::ExprStmt &node)
        {
            node.exp->accept(*this);
        }

        void IRBuilder::visit(SyntaxTree::UnaryCondExpr &node)
        {
            // 单目条件运算，只能是 NOT
            if (node.op != SyntaxTree::UnaryCondOp::NOT)
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
            // 得到新的 latest_value
            auto value_temp = latest_value;
            // 可能是INT1_T，先进行类型转换
            if (value_temp->get_type() == INT1_T)
            {
                value_temp = builder->create_zext(value_temp, INT32_T);
            }
            // 发射比较指令
            if (value_temp->get_type() == INT32_T)
            {
                latest_value = builder->create_icmp_eq(value_temp, CONST_INT(0));
            }
            else if (value_temp->get_type() == FLOAT_T)
            {
                latest_value = builder->create_fcmp_eq(value_temp, CONST_FLOAT(0));
            }

            // 恢复
            TrueBB = std::get<0>(temp_BBS);
            FalseBB = std::get<1>(temp_BBS);
        }

        void IRBuilder::visit(SyntaxTree::BinaryCondExpr &node)
        {
            // 这里需要处理 AND 和 OR 的短路计算
            // 方法是先根据操作符的类型去处理左操作数
            // 对于 OR , 处理左操作数时的 False 分支应该为进入右操作数处理的分支，True 分支为整个条件的 True 分支，从而可以跳过右操作数的计算
            // 对于 AND, 处理左操作数时的 True 分支应该为进入右操作数处理的分支，False 分支为整个条件的 False 分支，从而可以跳过右操作数的计算
            if (node.op == SyntaxTree::BinaryCondOp::LOR)
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
                LVal_retValue = 1;LVal_retPtr = 0;
                node.lhs->accept(*this);
                // 得到左边的 latest_value
                // 可能不是bool类型，要先转成bool类型
                if (latest_value->get_type() != INT1_T)
                {
                    if (latest_value->get_type() == INT32_T)
                    { // 再进行一次int的eq比较，得到 INT1_T
                        latest_value = builder->create_icmp_ne(latest_value, CONST_INT(0));
                    }
                    else if (latest_value->get_type() == FLOAT_T)
                    { // float 再比较一次得到 INT1_T
                        latest_value = builder->create_fcmp_ne(latest_value, CONST_FLOAT(0));
                    }
                }
                if (latest_value->get_type() != VOID_T)
                { // 是空类型则说明cond_br已经生成过了
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
                LVal_retValue = 1;LVal_retPtr = 0;
                node.rhs->accept(*this);
                // 得到lateet_value
                // 可能不是bool类型，要先转成bool类型
                if (latest_value->get_type() != INT1_T)
                {
                    if (latest_value->get_type() == INT32_T)
                    { // 再进行一次int的eq比较，得到 INT1_T
                        latest_value = builder->create_icmp_ne(latest_value, CONST_INT(0));
                    }
                    else if (latest_value->get_type() == FLOAT_T)
                    { // float 再比较一次得到 INT1_T
                        latest_value = builder->create_fcmp_ne(latest_value, CONST_FLOAT(0));
                    }
                }
                if (latest_value->get_type() != VOID_T)
                { // 是空类型则说明cond_br已经生成过了
                    // 这里右边为假时需要去FalseBB，否则跳到最后的TrueBB
                    latest_value = builder->create_cond_br(latest_value, TrueBB, FalseBB);
                }
            }
            else if (node.op == SyntaxTree::BinaryCondOp::LAND)
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
                if (latest_value->get_type() != INT1_T)
                {
                    if (latest_value->get_type() == INT32_T)
                    { // 再进行一次int的eq比较，得到 INT1_T
                        latest_value = builder->create_icmp_ne(latest_value, CONST_INT(0));
                    }
                    else if (latest_value->get_type() == FLOAT_T)
                    { // float 再比较一次得到 INT1_T
                        latest_value = builder->create_fcmp_ne(latest_value, CONST_FLOAT(0));
                    }
                }
                if (latest_value->get_type() != VOID_T)
                { // 是空类型则说明cond_br已经生成过了
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
                if (latest_value->get_type() != INT1_T)
                {
                    if (latest_value->get_type() == INT32_T)
                    { // 再进行一次int的eq比较，得到 INT1_T
                        latest_value = builder->create_icmp_ne(latest_value, CONST_INT(0));
                    }
                    else if (latest_value->get_type() == FLOAT_T)
                    { // float 再比较一次得到 INT1_T
                        latest_value = builder->create_fcmp_ne(latest_value, CONST_FLOAT(0));
                    }
                }
                if (latest_value->get_type() != VOID_T)
                { // 是空类型则说明cond_br已经生成过了
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
                if (l_value_temp->get_type() == INT1_T && r_value_temp->get_type() == INT1_T)
                {
                    // 均扩展到INT32_T再进行比较
                    l_value_temp = builder->create_zext(l_value_temp, INT32_T);
                    r_value_temp = builder->create_zext(r_value_temp, INT32_T);
                }
                else if (l_value_temp->get_type() == INT1_T && r_value_temp->get_type() == INT32_T)
                {
                    // 将l扩展到32
                    l_value_temp = builder->create_zext(l_value_temp, INT32_T);
                }
                else if (l_value_temp->get_type() == INT32_T && r_value_temp->get_type() == INT1_T)
                {
                    // 将r扩展到32
                    r_value_temp = builder->create_zext(r_value_temp, INT32_T);
                }
                else if (l_value_temp->get_type() == INT1_T && r_value_temp->get_type() == FLOAT_T)
                {
                    // 将l变为FLOAT
                    l_value_temp = builder->create_zext(l_value_temp, INT32_T);
                    l_value_temp = builder->create_sitofp(l_value_temp, FLOAT_T);
                }
                else if (l_value_temp->get_type() == FLOAT_T && r_value_temp->get_type() == INT1_T)
                {
                    // 将r变为FLOAT
                    r_value_temp = builder->create_zext(r_value_temp, INT32_T);
                    r_value_temp = builder->create_sitofp(r_value_temp, FLOAT_T);
                }
                else if (l_value_temp->get_type() == INT32_T && r_value_temp->get_type() == FLOAT_T)
                {
                    // 将l变为FLOAT
                    l_value_temp = builder->create_sitofp(l_value_temp, FLOAT_T);
                }
                else if (l_value_temp->get_type() == FLOAT_T && r_value_temp->get_type() == INT32_T)
                {
                    // 将r变为FLOAT
                    r_value_temp = builder->create_sitofp(r_value_temp, FLOAT_T);
                }
                // 现在类型一致，发射指令
                if (l_value_temp->get_type() == FLOAT_T)
                {
                    switch (node.op)
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
                else if (l_value_temp->get_type() == INT32_T)
                {
                    switch (node.op)
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
        void IRBuilder::visit(SyntaxTree::BinaryExpr &node)
        {
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
            if (std::dynamic_pointer_cast<ConstantFloat>(l_value_temp) && std::dynamic_pointer_cast<ConstantFloat>(r_value_temp))
            {
                auto l_value = std::dynamic_pointer_cast<ConstantFloat>(l_value_temp)->get_value();
                auto r_value = std::dynamic_pointer_cast<ConstantFloat>(r_value_temp)->get_value();
                // 两边都是常量浮点数
                switch (node.op)
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
            else if (std::dynamic_pointer_cast<ConstantFloat>(l_value_temp) && std::dynamic_pointer_cast<ConstantInt>(r_value_temp))
            {
                auto l_value = std::dynamic_pointer_cast<ConstantFloat>(l_value_temp)->get_value();
                auto r_value = std::dynamic_pointer_cast<ConstantInt>(r_value_temp)->get_value();
                // 左边是常量浮点数，右边是常量整数
                switch (node.op)
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
            else if (std::dynamic_pointer_cast<ConstantInt>(l_value_temp) && std::dynamic_pointer_cast<ConstantFloat>(r_value_temp))
            {
                auto l_value = std::dynamic_pointer_cast<ConstantInt>(l_value_temp)->get_value();
                auto r_value = std::dynamic_pointer_cast<ConstantFloat>(r_value_temp)->get_value();
                // 右边是常量浮点数，左边是常量整数
                switch (node.op)
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
            else if (std::dynamic_pointer_cast<ConstantInt>(l_value_temp) && std::dynamic_pointer_cast<ConstantInt>(r_value_temp))
            {
                auto l_value = std::dynamic_pointer_cast<ConstantInt>(l_value_temp)->get_value();
                auto r_value = std::dynamic_pointer_cast<ConstantInt>(r_value_temp)->get_value();
                // 左边是常量整数，右边是常量整数
                switch (node.op)
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
                // 先进行类型转换，再进行比较
                // 这里可能会有INT1_T类型的存在，需要考虑多种情况
                if (l_value_temp->get_type() == INT1_T && r_value_temp->get_type() == INT1_T)
                {
                    // 均扩展到INT32_T再进行比较
                    l_value_temp = builder->create_zext(l_value_temp, INT32_T);
                    r_value_temp = builder->create_zext(r_value_temp, INT32_T);
                }
                else if (l_value_temp->get_type() == INT1_T && r_value_temp->get_type() == INT32_T)
                {
                    // 将l扩展到32
                    l_value_temp = builder->create_zext(l_value_temp, INT32_T);
                }
                else if (l_value_temp->get_type() == INT32_T && r_value_temp->get_type() == INT1_T)
                {
                    // 将r扩展到32
                    r_value_temp = builder->create_zext(r_value_temp, INT32_T);
                }
                else if (l_value_temp->get_type() == INT1_T && r_value_temp->get_type() == FLOAT_T)
                {
                    // 将l变为FLOAT
                    l_value_temp = builder->create_zext(l_value_temp, INT32_T);
                    l_value_temp = builder->create_sitofp(l_value_temp, FLOAT_T);
                }
                else if (l_value_temp->get_type() == FLOAT_T && r_value_temp->get_type() == INT1_T)
                {
                    // 将r变为FLOAT
                    r_value_temp = builder->create_zext(r_value_temp, INT32_T);
                    r_value_temp = builder->create_sitofp(r_value_temp, FLOAT_T);
                }
                else if (l_value_temp->get_type() == INT32_T && r_value_temp->get_type() == FLOAT_T)
                {
                    // 将l变为FLOAT
                    l_value_temp = builder->create_sitofp(l_value_temp, FLOAT_T);
                }
                else if (l_value_temp->get_type() == FLOAT_T && r_value_temp->get_type() == INT32_T)
                {
                    // 将r变为FLOAT
                    r_value_temp = builder->create_sitofp(r_value_temp, FLOAT_T);
                }

                // 左右类型一致，发射指令
                if (l_value_temp->get_type() == FLOAT_T)
                {
                    switch (node.op)
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
                    switch (node.op)
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
        void IRBuilder::visit(SyntaxTree::UnaryExpr &node)
        {
            // 单目运算符 正 和 负
            // 需要判断操作数是否是常量，若是则不需发射指令，直接设置latest_value即可
            node.rhs->accept(*this);
            // 得到latest_value
            auto value_temp = latest_value;
            // 若转换不合法则会返回nullptr，即不是常量
            if (std::dynamic_pointer_cast<ConstantFloat>(value_temp))
            {
                // 右操作数是一个浮点常量，则直接返回需要的latest_value
                if (node.op == SyntaxTree::UnaryOp::PLUS)
                    latest_value = value_temp;
                else
                {
                    float value = std::dynamic_pointer_cast<ConstantFloat>(value_temp)->get_value();
                    latest_value = CONST_FLOAT(-1.0 * value);
                }
            }
            else if (std::dynamic_pointer_cast<ConstantInt>(value_temp))
            {
                // 右操作数是一个整形常量，则直接返回需要的latest_value
                if (node.op == SyntaxTree::UnaryOp::PLUS)
                    latest_value = value_temp;
                else
                {
                    int value = std::dynamic_pointer_cast<ConstantInt>(value_temp)->get_value();
                    latest_value = CONST_INT(-1 * value);
                }
            }
            else
            {
                // 如果是INT1 需要先转成INT32
                if(value_temp->get_type() == INT1_T)
                {
                    value_temp = builder->create_zext(value_temp, INT32_T);
                }
                // 右操作数不是常数
                if (node.op == SyntaxTree::UnaryOp::PLUS)
                {
                    latest_value = value_temp;
                }
                else
                {
                    if (value_temp->get_type() == FLOAT_T)
                    { // 发射一条浮点乘法，得到 -1.0 * value_temp->get_value()
                        latest_value = builder->create_fmul(value_temp, CONST_FLOAT(-1));
                    }
                    else if (value_temp->get_type() == INT32_T)
                    { // 发射一条整数乘法，得到 -1 * value_temp->get_value()
                        latest_value = builder->create_imul(value_temp, CONST_INT(-1));
                    }
                }
            }
        }

        void IRBuilder::visit(SyntaxTree::FuncCallStmt &node)
        {
            // 需要在Funcdef时将函数push进符号表
            // 查找符号表，得到Function指针
            Ptr<Value> Func_value = scope.find(node.name, true);
            // 基类到派生类的类型转换
            Ptr<Function> Func = std::dynamic_pointer_cast<Function>(Func_value);
            // 获取函数形参，便于在检查类型时进行类型转换
            std::vector<Ptr<Value>> func_params;
            for (auto param = Func->arg_begin(); param != Func->arg_end(); param++)
            { // 派生类到基类的自动隐式转换
                func_params.emplace_back(*param);
            }
            // 下面递归访问获得函数实参列表
            std::vector<Ptr<Value>> true_params;
            auto index = 0;
            for (auto &arg : node.params)
            {
                arg->accept(*this);
                // latest_value得到实参的信息
                if (func_params[index]->get_type() == FLOAT_T && latest_value->get_type() == INT32_T)
                {
                    // 实参转换为 float
                    latest_value = builder->create_sitofp(latest_value, FLOAT_T);
                    true_params.emplace_back(latest_value);
                }
                else if (func_params[index]->get_type() == INT32_T && latest_value->get_type() == FLOAT_T)
                {
                    // 实参转换为 INT
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

        void IRBuilder::visit(SyntaxTree::IfStmt &node)
        {
            // 由于if可能是嵌套的，所以在进入一次此函数时需要将与if有关的全局变量暂存，退出时再恢复
            // 这样能保证全局变量一直指示的是当前信息
            // 暂存全局变量
            // NextBB不能使用全局变量
            std::tuple<Ptr<BasicBlock>, Ptr<BasicBlock>> temp_BBs = std::make_tuple(TrueBB, FalseBB);
            // 创建此if相关的基本块
            std::string BB_id_string;
            BB_id_string = "TrueBB";
            BB_id_string += std::to_string(BB_id++);
            TrueBB = BasicBlock::create(module, BB_id_string, CurrentFunction);
            BB_id_string = "FalseBB";
            BB_id_string += std::to_string(BB_id++);
            FalseBB = BasicBlock::create(module, BB_id_string, CurrentFunction);
            Ptr<BasicBlock> NextBB_local;
            if (!node.else_statement)
            { // 看是否有else分支，这里是没有
              // 没有则FalseBB和NextBB相同
                NextBB_local = FalseBB;
            }
            else
            {
                // 重新创建NextBB
                BB_id_string = "NextBB";
                BB_id_string += std::to_string(BB_id++);
                NextBB_local = BasicBlock::create(module, BB_id_string, CurrentFunction);
            }

            // 递归访问 AST 上 ifstmt 中的 cond_exp
            node.cond_exp->accept(*this);
            // 根据返回后 latest_value 的值判断，同while部分的处理
            // 由于cond_exp可能是int或者float等类型，即（）中不是一个bool值，所以需要
            // 再进行一次比较，得到 bool 值
            if (latest_value->get_type() != INT1_T)
            {
                if (latest_value->get_type() == INT32_T)
                { // 再进行一次int的eq比较，得到 INT1_T
                    latest_value = builder->create_icmp_ne(latest_value, CONST_INT(0));
                }
                else if (latest_value->get_type() == FLOAT_T)
                { // float 再比较一次得到 INT1_T
                    latest_value = builder->create_fcmp_ne(latest_value, CONST_FLOAT(0));
                }
            }
            if (latest_value->get_type() != VOID_T)
            { // 是空类型则说明cond_br已经生成过了
                latest_value = builder->create_cond_br(latest_value, TrueBB, FalseBB);
            }
            bool NextBB_Avaiable = false;
            // 当前所处块变为 TrueBB
            CurrentBB = TrueBB;
            // 插入 TrueBB 的 label
            builder->set_insert_point(TrueBB);
            // 递归访问 AST 上 ifstmt 中的 if_stmt
            node.if_statement->accept(*this);
            // 是不是终止指令（如ret），不是才需要加br
            if (!CurrentBB->get_terminator())
            {
                NextBB_Avaiable = true;
                latest_value = builder->create_br(NextBB_local);
            }
            // 处理 else 分支
            if (node.else_statement != nullptr)
            {
                // 当前所处块变为 FalseBB
                CurrentBB = FalseBB;
                // 插入 FalseBB 的 label
                builder->set_insert_point(FalseBB);
                // 递归访问 AST 上 ifstmt 中的 else_stmt
                node.else_statement->accept(*this);
                // 是不是终止指令（如ret），不是才需要加br
                if (!CurrentBB->get_terminator())
                {
                    NextBB_Avaiable = true;
                    latest_value = builder->create_br(NextBB_local);
                }
            }
            if (NextBB_Avaiable || !node.else_statement)
            {    // 当前所处块变为 NextBB
                CurrentBB = NextBB_local;
                // 插入 NextBB 的 label
                builder->set_insert_point(NextBB_local);
            }
            else
            {
                NextBB_local->erase_from_parent();
            }
            // 退出此if，需要恢复之前暂存的全局变量
            TrueBB = std::get<0>(temp_BBs);
            FalseBB = std::get<1>(temp_BBs);
        }

        void IRBuilder::visit(SyntaxTree::WhileStmt &node)
        {
            // 由于while循环可能是嵌套的，所以在进入一次此函数时需要将与while循环有关的全局变量暂存，退出时再恢复
            // 这样能保证全局变量一直指示的是当前信息
            // 暂存全局变量
            std::tuple<Ptr<BasicBlock>, Ptr<BasicBlock>, Ptr<BasicBlock>, Ptr<BasicBlock>> temp_BBs =
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
            if (latest_value->get_type() != INT1_T)
            {
                if (latest_value->get_type() == INT32_T)
                { // 再进行一次int的eq比较，得到 INT1_T
                    latest_value = builder->create_icmp_ne(latest_value, CONST_INT(0));
                }
                else if (latest_value->get_type() == FLOAT_T)
                { // float 再比较一次得到 INT1_T
                    latest_value = builder->create_fcmp_ne(latest_value, CONST_FLOAT(0));
                }
            }
            if (latest_value->get_type() != VOID_T)
            { // 是空类型则说明cond_br已经生成过了
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
            if (!CurrentBB->get_terminator())
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

        void IRBuilder::visit(SyntaxTree::BreakStmt &node)
        {
            // 无条件跳转到当前while循坏的下一个基本块
            latest_value = builder->create_br(NextBB);
        }

        void IRBuilder::visit(SyntaxTree::ContinueStmt &node)
        {
            // 无条件返回到当前while循坏的条件基本块
            latest_value = builder->create_br(CondBB);
        }

    }
}