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

void IRBuilder::visit(SyntaxTree::ExprStmt &node) {}

void IRBuilder::visit(SyntaxTree::UnaryCondExpr &node) {}

void IRBuilder::visit(SyntaxTree::BinaryCondExpr &node) {}

void IRBuilder::visit(SyntaxTree::BinaryExpr &node) {}

void IRBuilder::visit(SyntaxTree::UnaryExpr &node) {}

void IRBuilder::visit(SyntaxTree::FuncCallStmt &node) {
}

void IRBuilder::visit(SyntaxTree::IfStmt &node) {}

void IRBuilder::visit(SyntaxTree::WhileStmt &node) {}

void IRBuilder::visit(SyntaxTree::BreakStmt &node) {}

void IRBuilder::visit(SyntaxTree::ContinueStmt &node) {}

}
}
