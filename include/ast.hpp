#ifndef MS_AST_HPP
#define MS_AST_HPP

#include <string>
#include <vector>
#include <stack>
#include <utility>
#include <concepts>
#include <type_traits>

#include "env.hpp"
#include "types.hpp"
#include "source.hpp"
#include "lang.hpp"
#include "debug.hpp"

namespace ms::ast {

    struct CommonNode;
    
    using Tree = CommonNode*;
    using UTree = UPtr<CommonNode>;

    enum class NodeClass : int {

        None,

        Block,
        Stmt,
        Decl,

        __count__

    };

    enum class NodeType : int {

        None = 0,

        Program,
        Module,

        ImportStmt,
        AssignStmt,

        TypeDecl,
        FnDecl,
        VarDecl,

        LiteralExpr,
        
        RefExpr,
        RefChainExpr,

        MemberRefExpr,
        InlineFnDeclExpr,
        BinaryExpr,
        CommaExpr,

        FnCall,

        ProgramBlock,
        ParamsBlock,
        FnBodyBlock,

        TypeRef,

        __count__

    };

    /* */

    template <typename N, NodeType type = NodeType::None>
    struct TypedNode {
        constexpr static inline NodeType DEF_TYPE {type};
    };

    // decls
    struct ModuleNode;
    struct TypeDecl;
    struct FnDecl;
    struct ExprNode;

    struct BaseNode {

        NodeClass clazz {NodeClass::None};
        NodeType type {NodeType::None};

        BaseNode() {}
        BaseNode(NodeClass pClass) : clazz(pClass) {}
        BaseNode(NodeType pType) : type(pType) {}
        BaseNode(NodeClass pClass, NodeType pType) : clazz(pClass), type(pType) {}

    };

    struct CommonNode {

        Tree parent {nullptr};
        BaseNode base;
        SourceLocation location;

        CommonNode() {}
        CommonNode(NodeType pType) : base(pType) {}
        CommonNode(NodeClass pClass, NodeType pType) : base(pClass, pType) {}

        virtual ~CommonNode() {
            // debug::printsf("~ast::Node(%%)", base.type);
        }

    };

    /* Main Types */

    struct DeclNode : CommonNode {
        std::string name;

        DeclNode() {}
        DeclNode(const NodeType pType, const std::string& pName) :
            CommonNode(pType), name(pName) {}
    };

    struct StmtNode : CommonNode {

        StmtNode() {}
        StmtNode(const NodeType pType) : CommonNode(pType) {}
    };

    struct BlockNode : CommonNode {

        std::vector<UTree> nodes;

        BlockNode() {}
        BlockNode(const NodeType pType) : CommonNode(NodeClass::Block, pType) {}

        inline Status append(UTree tree) {
            nodes.push_back(std::move(tree));
            
            return Status::Success;
        }

    };

    /* */

    struct ImportStmt : StmtNode {

        std::string name;

        ImportStmt() : StmtNode(NodeType::ImportStmt) {}
        ImportStmt(const std::string& pName) : StmtNode(NodeType::ImportStmt), name(pName) {}

    };

    struct ProgramNode : CommonNode {

        BlockNode block {NodeType::ProgramBlock};
        UPtrList<FnDecl> inlineFnDecls;

        std::string name;
        HMap<std::string, ModuleNode*> modules;

        ProgramNode(const std::string& pName) : CommonNode(NodeType::Program), name(pName) {}

    };

    struct ModuleNode : BlockNode {

        std::string name;
        Source& source;

        /* easy access; nodes are part of block */
        std::vector<ImportStmt*> imports;

        ModuleNode(const std::string& pName, Source& pSource) :
            BlockNode(NodeType::Module), name(pName), source(pSource) {}

    };

    /* Variables */

    struct TypeDecl : DeclNode {

        TypeDecl() : DeclNode(NodeType::TypeDecl, "<unnamed>") {}
        TypeDecl(const std::string& pName) : DeclNode(NodeType::TypeDecl, pName) {}

    };

    struct TypeRef : CommonNode {

        TypeDecl* decl {nullptr};

        TypeRef() : CommonNode(NodeType::TypeRef) {}
        TypeRef(TypeDecl* pDecl) : CommonNode(NodeType::TypeRef), decl(pDecl) {}

        inline bool isset() const {
            return decl;
        }

    };

    struct TypeNode : CommonNode {

        TypeDecl* decl;

    };

    struct VarDecl : DeclNode {

        types::VarScope scope;

        VarDecl(const std::string& pName, const types::VarScope pScope) :
            DeclNode(NodeType::VarDecl, pName), scope(pScope) {}

    };

    struct ParamDecl : VarDecl {

        TypeRef typeGuard;
        URef<ExprNode> initialValue;

        ParamDecl() : VarDecl("<unnamed>", types::VarScope::Param) {}
        ParamDecl(const std::string& pName) : VarDecl(pName, types::VarScope::Param) {}

    };

    struct FnDecl : DeclNode {

        std::vector<types::FnMod> mods;
        BlockNode params {NodeType::ParamsBlock};
        BlockNode body {NodeType::FnBodyBlock};

        /* No need for a return type (or "returns only void" flag) since on runtime
         * a function can be re-addressed to something which does return a value.
         * Functions without a return statement (or without a returned value) just
         * return undefined as the result on runtime.
         */

        FnDecl() : DeclNode(NodeType::FnDecl, "<unnamed>") {}
        FnDecl(const std::string& pName) : DeclNode(NodeType::FnDecl, pName) {}

    };

    struct ExprNode : CommonNode {

        ExprNode() {}
        ExprNode(const NodeType pType) : CommonNode(pType) {}

    };

    struct LiteralExpr : ExprNode {

        long literalId {-1};
        TypeDecl* literalType {nullptr};

        LiteralExpr() {}
        LiteralExpr(const long pLiteralId, TypeDecl* pLiteralType) :
            ExprNode(NodeType::LiteralExpr), literalId(pLiteralId), literalType(pLiteralType) {}

    };

    struct RefExpr : ExprNode {

        Symbol symbol;
        Tree node {nullptr}; // VarDecl, FnDecl

        RefExpr() : ExprNode(NodeType::RefExpr) {}
        RefExpr(const Symbol& pSymbol) : ExprNode(NodeType::RefExpr), symbol(pSymbol) {}

    };

    struct RefChainExpr : ExprNode {

        UPtrList<RefExpr> chain;

        RefChainExpr() : ExprNode(NodeType::RefChainExpr) {}

    };

    struct MemberRefExpr : ExprNode {

        Symbol symbol;

        MemberRefExpr() : ExprNode(NodeType::MemberRefExpr) {}
        MemberRefExpr(const Symbol& pSymbol) :
            ExprNode(NodeType::MemberRefExpr), symbol(pSymbol) {}


    };

    struct InlineFnDeclExpr : ExprNode {

        size_t uid {0};
        FnDecl* decl;

        InlineFnDeclExpr() : ExprNode(NodeType::InlineFnDeclExpr) {}
        InlineFnDeclExpr(FnDecl* inlineDecl) :
            ExprNode(NodeType::InlineFnDeclExpr), decl(inlineDecl) {}

    };

    struct BinaryExpr : ExprNode {

        types::Operator op;
        UPtr<ExprNode> left;
        UPtr<ExprNode> right;

        BinaryExpr() : ExprNode(NodeType::BinaryExpr) {}
        BinaryExpr(types::Operator pOp, UPtr<ExprNode> pLeft, UPtr<ExprNode> pRight) :
            ExprNode(NodeType::BinaryExpr), op(pOp), left(std::move(pLeft)), right(std::move(pRight)) {}

    };

    struct CommaExpr : ExprNode {

        std::vector<UPtr<ExprNode>> exprs;

        CommaExpr() : ExprNode(NodeType::CommaExpr) {}

    };

    struct FnCall : ExprNode {

        Symbol name;
        FnDecl* decl {nullptr};
        UPtrList<ExprNode> params;

        FnCall() : ExprNode(NodeType::FnCall) {}
        FnCall(FnDecl* pDecl) : ExprNode(NodeType::FnCall), decl(pDecl) {}
        FnCall(Symbol pSymbol) : ExprNode(NodeType::FnCall), name(pSymbol) {}

    };
    
    struct AssignStmt : StmtNode {

        UPtr<ExprNode> target;
        UPtr<ExprNode> expr;

        AssignStmt() : StmtNode(NodeType::AssignStmt) {}
        AssignStmt(UPtr<ExprNode> pTarget, UPtr<ExprNode> pExpr) :
            StmtNode(NodeType::AssignStmt), target(std::move(pTarget)), expr(std::move(pExpr)) {}

    };

    template <>
    struct TypedNode<VarDecl, NodeType::VarDecl> {};

}

namespace ms::ast {

    struct ASTContext {

        UPtr<ProgramNode> program {nullptr};

        ModuleNode* module {nullptr};
        BlockNode* block {nullptr};

        FnDecl* function {nullptr};

        std::stack<Tree> layers;
        Tree scope {nullptr};

    };

    struct FnBuilder {

        Tree scope {nullptr};
        FnDecl* decl {nullptr};
        UPtr<FnDecl> node;

    };

    enum ExprPurpose : uint32_t {

        None            = 0,
        FuncName        = 1 << 1,
        FuncParams      = 1 << 2,
        OpNot           = 1 << 3,
        MemberRef       = 1 << 4

    };

    struct Expression {

        Tree scope {nullptr};
        
        /* Result node or list of nodes depending on the expression type. In most cases the
         * single node will be used but comma expressions or function parameters will refer
         * to the external vector provided (nodes will either be part of CommaExpr or FnCall.params).
         */
        UPtr<ExprNode> node;
        UPtrList<ExprNode>* nodes {nullptr};

        size_t start {0};
        size_t end {0};

        int purpose {0};
        bool empty {false};

        Expression() {}
        Expression(Tree pScope) : scope(pScope) {}
        Expression(Tree pScope, size_t pStart, size_t pEnd) :
            scope(pScope), start(pStart), end(pEnd) {}

        /* Returns wether this expression was parsed to an expression node as the result.
         * Due to the recursive parsing, there are expression structs which do not yield
         * an ExprNode and are just being used structurally (e.g. right side expansion
         * when parsing comma lists - only the last element on the right yields an ExprNode
         * as the result, all others are just remnants of the recursive parsing).
         */
        inline bool hasNode() const {
            return !empty && node.get();
        }

        /* Returns the count of result nodes. If the nodes list is existing and is not empty,
         * the size of the nodes vector is returned (might be 1). Otherwise if the single node
         * is not null, 1 is returned. In all other cases 0.
         */
        inline int nodeCount() const {
            return (nodes && nodes->size() > 0 ? nodes->size() : (hasNode() ? 1 : 0));
        }

        /* Returns the node's type if the result is set as a single node. */
        inline NodeType nodeType() const {
            return node.get() ? node.get()->base.type : NodeType::None;
        }

    };

    struct TreeNodeCondition {

        bool matches(Tree node);

    };

    struct TreeWalker {

        TreeNodeCondition condition;

    };

}

namespace ms::ast {

    struct NodeMeta {

        static inline unsigned char subtype[(unsigned long) NodeClass::__count__][(unsigned long) NodeType::__count__];

        static inline bool isBlock(const NodeType type) {
            return subtype[(unsigned long) NodeClass::Block][(unsigned long) type] == 1;
        }

        static inline bool isBlock(const Tree tree) {
            return isBlock(tree->base.type);
        }

    };

    template <NodeType type>
    struct NodeInfo {

        constexpr bool isSubtypeOf(const NodeType other) {
            return NodeMeta::subtype[(int) other][(int) type] == (unsigned char) 1;
        }

        constexpr bool isSuperOf(const NodeType other) {
            return NodeMeta::subtype[(int) type][(int) other] == (unsigned char) 1;
        }

        /* */

    };


    template <typename N>
    concept NodeLike = requires {
        N::base;
    };

    // TODO: see https://stackoverflow.com/a/67304893
    template <typename P, typename N>
    concept NodePredicate = requires (P predicate, N node) {
        { predicate(node) } -> std::same_as<bool>;
    };

    /* Constructs a new UPtr<N> from the constructor args. */
    template <typename N, typename... Args>
    static UPtr<N> makeNode(Args&&... args) {
        return std::make_unique<N>(args...);
    }

    BlockNode* blockNode(Tree tree);

    /* Appends a child node to block. When the tree is not a block node but holds
     * a block, the node will be added to that (in cases like functions where params
     * and the body are blocks within the function node).
     */
    template <typename N>
    Status append(Tree tree, UPtr<N> node) {
        if (tree) {
            /* If the current node is a block, just use it. */
            if (NodeClass::Block == tree->base.clazz) {
                static_cast<Tree>(node.get())->parent = tree;
                return static_cast<BlockNode*>(tree)->append(unique_ptr_cast<CommonNode, N>(std::move(node)));
            }

            BlockNode* block;

            /* There are several cases where nodes have an internal "block" variable; check them here. */
            switch (tree->base.type) {
                case NodeType::Program:
                    block = &static_cast<ProgramNode*>(tree)->block;
                    break;
                
                case NodeType::FnDecl:
                    block = &static_cast<FnDecl*>(tree)->body;
                    break;

                default:
                    return Status::InvalidNodeType;
            }

            if (block) {
                static_cast<Tree>(node.get())->parent = tree;
                return block->append(unique_ptr_cast<CommonNode, N>(std::move(node)));
            }
        }

        return Status::Fail;
    }

    /* Returns the backing NodeType of a tree node. */
    template <NodeLike Node>
    NodeType type(const Node* node) {
        return node ? node->base.type : NodeType::None;
    }

    template <typename N, NodeType type = TypedNode<N>::DEF_TYPE>
    std::vector<N*> nodesByType(const std::vector<UTree>& nodes) {
        std::vector<N*> filtered;

        for (const UTree& tree : nodes) {
            if (tree.get()->base.type == type) {
                filtered.push_back(static_cast<N*>(tree.get()));
            }
        }

        return filtered;
    }

    /* Iterates over all child nodes of the block by filtering for a given type.
     * Returns the first node where the predicate matches or nullptr.
     *
     * @param block BlockNode* to scan
     * @param type NodeType to filter
     * @param predicate lambda [](const N& node) -> bool
     */
    template <typename N, typename Predicate>
    N* filterBlock(BlockNode* block, const NodeType type, Predicate predicate) {
        if (block) {
            for (const UTree& node : block->nodes) {
                const N* child = static_cast<const N*>(node.get());

                if (node->base.type == type && predicate(*child)) {
                    return static_cast<N*>(node.get());
                }
            }
        }

        return nullptr;
    }

    /* Searches for a node in the tree hierarchy by going up the parent structure.
     * Each block layer will be scanned by the predicate. Layers (scopes) of types
     * which hold the block as a member (like functions) will reference that block.
     * Terminates when a node is found or after the Program layer.
     * 
     * @param block BlockNode* to scan
     * @param type NodeType to filter
     * @param predicate lambda [](const N& node) -> bool
     */
    template <typename N, typename Predicate>
    N* findFirst(Tree scope, const NodeType type, Predicate predicate) {
        int it = 0;

        while (scope && NodeType::None != scope->base.type) {
            BlockNode* block {nullptr};
            N* found {nullptr};

            /* If the current node is a block, just use it. */
            if (NodeClass::Block == scope->base.clazz) {
                block = static_cast<BlockNode*>(scope);
                found = filterBlock<N, Predicate>(block, type, predicate);
            } else {
                /* some scopes contain a block (or multiple) */
                switch (ast::type(scope)) {
                    case NodeType::Program: {
                        ProgramNode* program = static_cast<ProgramNode*>(scope);

                        if (found = filterBlock<N, Predicate>(&program->block, type, predicate))
                            return found;

                        break;
                    }
                    
                    case NodeType::FnDecl: {
                        FnDecl* decl = static_cast<FnDecl*>(scope);

                        if (found = filterBlock<N, Predicate>(&decl->params, type, predicate))
                            return found;

                        if (found = filterBlock<N, Predicate>(&decl->body, type, predicate))
                            return found;

                        break;
                    }
                }
            }

            if (found)
                return found;

            if (++it == 999) {
                return nullptr;
            }

            debug::printsf(" ^ %% -> %%", scope->base.type, scope->parent ? scope->parent->base.type : NodeType::None);
            scope = scope->parent;
        }

        return nullptr;
    }

    /* Returns true when a VarDecl is found in the tree hierarchy by name. */
    bool isVarDecl(Tree scope, const std::string& ident);

}

namespace ms::ast {

    void print(Tree);

}

#endif