#include "ast.hpp"
#include "sformats.hpp"
#include "debug.hpp"
#include "env.hpp"

namespace ms::ast {

    BlockNode* blockNode(Tree tree) {
        if (tree) {
            /* If the current node is a block, just use it. */
            if (NodeClass::Block == tree->base.clazz)
                return static_cast<BlockNode*>(tree);

            BlockNode* block;

            /* There are several cases where nodes have an internal "block" variable; check them here. */
            switch (tree->base.type) {
                case NodeType::Program:
                    return &static_cast<ProgramNode*>(tree)->block;
                
                case NodeType::FnDecl:
                    return &static_cast<FnDecl*>(tree)->body;

                default:
                    return nullptr;
            }
        }

        return nullptr;
    }

    bool isVarDecl(Tree scope, const std::string& ident) {
        debug::printsf(" ? var: %%", ident);
        
        return findFirst<VarDecl>(scope, NodeType::VarDecl, [&](const VarDecl& decl) { return decl.name == ident; });
    }

}

namespace ms::ast {

    #define PRINT_VEC(vec) MS_STMT_MAKRO(for (size_t i = 0; i < vec.size(); i++) { print0(vec[i].get(), i, vec.size(), level + 1); })

    void print0(Tree tree, int index, int count, int level = 0) {
        if (tree) {
            const std::string padding(level, ' ');
            const std::string prefix = padding + ((index | level) == 0 ? "#-" : (index < (count - 1) ? "|-" : "\\-"));

            const NodeType type = tree->base.type;
            BlockNode* block {nullptr};

            switch (type) {
                case NodeType::Program: {
                    ProgramNode* program = static_cast<ProgramNode*>(tree);

                    debug::printsf("%% (%%) %%", prefix, type, program->name);
                    block = &program->block;

                    break;
                }

                case NodeType::Module: {
                    ModuleNode* moduleNode = static_cast<ModuleNode*>(tree);

                    debug::printsf("%% (%%) %%", prefix, type, moduleNode->name);
                    block = moduleNode;

                    break;
                }

                case NodeType::ImportStmt: {
                    debug::printsf("%% (%%) %%", prefix, type, static_cast<ImportStmt*>(tree)->name);
                    break;
                }

                case NodeType::AssignStmt: {
                    AssignStmt* stmt = static_cast<AssignStmt*>(tree);

                    debug::printsf("%% (%%)", prefix, type);
                    debug::printsf("%% + target: %%", padding, ast::type(stmt->target.get()));
                    print0(stmt->target.get(), 0, 1, level + 1);

                    debug::printsf("%% + expr: %%", padding, ast::type(stmt->expr.get()));
                    print0(stmt->expr.get(), 0, 1, level + 1);

                    break;
                }

                case NodeType::TypeDecl: {
                    TypeDecl* decl = static_cast<TypeDecl*>(tree);

                    debug::printsf("%% (%%) %%", prefix, type, decl->name);
                    break;
                }

                case NodeType::TypeRef: {
                    TypeRef* ref = static_cast<TypeRef*>(tree);

                    debug::printsf("%% (%%) -> *(%%) %%", prefix, type,
                        ref->isset() ? ref->decl->base.type : NodeType::None,
                        ref->isset() ? ref->decl->name : "");

                    break;
                }

                case NodeType::FnDecl: {
                    FnDecl* decl = static_cast<FnDecl*>(tree);

                    debug::printsf("%% (%%) %%", prefix, type, decl->name);
                    debug::printsf("%% + params: %%", padding, decl->params.base.type);
                    
                    for (size_t i = 0; i < decl->params.nodes.size(); i++) {
                        print0(decl->params.nodes[i].get(), i, decl->params.nodes.size(), level + 1);
                    }

                    debug::printsf("%% + body: %%", padding, decl->body.base.type);
                    
                    for (size_t i = 0; i < decl->body.nodes.size(); i++) {
                        print0(decl->body.nodes[i].get(), i, decl->body.nodes.size(), level + 1);
                    }

                    break;
                }

                case NodeType::VarDecl: {
                    VarDecl* var = static_cast<VarDecl*>(tree);

                    debug::printsf("%% (%%) %% [%%]", prefix, type, var->name, var->scope);

                    if (var->scope == types::VarScope::Param) {
                        ParamDecl* param = static_cast<ParamDecl*>(var);

                        debug::printsf("%% + guard type: %%", padding, param->typeGuard.isset() ? param->typeGuard.base.type : NodeType::None);

                        if (param->typeGuard.isset())
                            print0(&param->typeGuard, 0, 1, level + 1);

                        debug::printsf("%% + initial value: %%", padding, ast::type(param->initialValue.get()));
                        print0(param->initialValue.get(), 0, 1, level + 1);
                    }

                    break;
                }

                case NodeType::LiteralExpr: {
                    LiteralExpr* expr = static_cast<LiteralExpr*>(tree);

                    debug::printsf("%% (%%) %% [%%]", prefix, type, expr->literalId, "TBD: literal info");
                    print0(expr->literalType, 0, 1, level + 1);

                    break;
                }

                case NodeType::RefExpr: {
                    RefExpr* expr = static_cast<RefExpr*>(tree);

                    debug::printsf("%% (%%) %% [%%]", prefix, type, expr->symbol, "TBD: ref info");
                    break;
                }

                case NodeType::BinaryExpr: {
                    BinaryExpr* expr = static_cast<BinaryExpr*>(tree);

                    debug::printsf("%% (%%)", prefix, type);
                    debug::printsf("%% + op: %%", padding, expr->op);

                    debug::printsf("%% + left: %%", padding, ast::type(expr->left.get()));
                    print0(expr->left.get(), 0, 1, level + 1);

                    debug::printsf("%% + right: %%", padding, ast::type(expr->right.get()));
                    print0(expr->right.get(), 0, 1, level + 1);

                    break;
                }

                case NodeType::CommaExpr: {
                    CommaExpr* expr = static_cast<CommaExpr*>(tree);

                    debug::printsf("%% (%%)", prefix, type);
                    debug::printsf("%% + exprs: %%", padding, expr->exprs.size());
                    
                    for (size_t i = 0; i < expr->exprs.size(); i++) {
                        print0(expr->exprs[i].get(), i, expr->exprs.size(), level + 1);
                    }

                    break;
                }

                case NodeType::FnCall: {
                    FnCall* call = static_cast<FnCall*>(tree);

                    debug::printsf("%% (%%) %%", prefix, type, call->decl ? call->decl->name : call->name.value);
                    debug::printsf("%% + params: %%", padding, call->params.size());

                    PRINT_VEC(call->params);

                    break;
                }

                default:
                    debug::printsf("%% (%%)", prefix, type);
                    break;
            }

            if (block) {
                const size_t count = block->nodes.size();

                for (int i = 0; i < count; i++) {
                    print0(block->nodes[i].get(), i, (int) count, level + 1);
                }
            }
        }
    }

    void print(Tree tree) {
        print0(tree, 0, 1, 0);
    }

    #undef PRINT_VEC

}