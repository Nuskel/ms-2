#ifndef MS_LANG2_HPP
#define MS_LANG2_HPP

#include <string>
#include <iostream>
#include <functional>

#include "env.hpp"
#include "source.hpp"

namespace ms {

  /* -- Symbol -- */

  struct Symbol {

    std::string value;
    int modifiers;

    const static Symbol kNone;

    Symbol() : value("__none__") {}
    explicit Symbol(const std::string_view name) : value(name) {}
    Symbol(const Symbol& other) : value(other.value), modifiers(other.modifiers) {}

    inline Symbol& operator=(const Symbol& s) {
      this->value = s.value;
      this->modifiers = s.modifiers;

      return *this;
    }

    friend inline bool operator==(const Symbol& l, const Symbol& r) {
      return l.value == r.value;
    }

    friend inline std::ostream& operator<<(std::ostream& stream, const Symbol& symbol) {
      return stream << symbol.value;
    }

    static bool nameValid(const std::string& value) {
      return false;
    }

  };

  struct SymbolHash {
    
    size_t operator()(const ms::Symbol& symbol) const {
      return std::hash<std::string>{}(symbol.value);
    }

  };

  const Symbol Symbol::kNone = Symbol("__none__");

  /* --- */

  struct DeclSpec {

    Symbol name;

  };

  struct FunctionSpec : public DeclSpec {

    bool atomic { false };
    bool constant { false };

  };

  /* -- Node -- */

  enum class NodeType : int {

    NONE,

    VAR,

    /* Namespaces */
    MODULE,
    FUNCTION

  };

  struct Node {

    Node* parent { nullptr };
    NodeType type { NodeType::NONE };

    Node() {}
    explicit Node(NodeType ntype) : type(ntype) {}
    virtual ~Node() {}

  };

  struct Namespace : public Node {

    HMap<Symbol, URef<Node>> nodes;

    SRef<Node> find(Symbol);

  };

  struct Module : public Namespace {

    SRef<Source> source;
    HMap<Symbol, SRef<Module>> imports;

  };

  /* -- Lookup -- */

  struct NodeLookup {

    Symbol symbol;
    Namespace* scope { nullptr };

  };

  struct NodeMatch {

    Node* node {nullptr};
    Module* module {nullptr};

  };

  namespace lang {

  }

}

/* Functions */

namespace ms {

  /* internal */
  namespace lang {

    static bool isNamespace(const Node* node) {
      if (!node)
        return false;

      return node->type >= NodeType::MODULE &&
        node->type <= NodeType::FUNCTION;
    }

  }

    /**
     * Searches for a node by a lookup strategy. Will first go up in the namespaces until the
     * current module's level is reahched. Then all imported modules are searched; finally
     * the global scope will be looked for.
     * 
     * @param ctx Application context
     * @param lookup Strategy options like symbol and type
     * @param match Result which can contain a found node
     * @return True if a node could be found
     */
    bool lookupNode(const Context& ctx, const NodeLookup& lookup, NodeMatch& match) {
        Namespace* scope { lookup.scope ? lookup.scope : nullptr /*ctx.scope*/ };

        /* local lookup */
        while (scope) {
            auto it = scope->nodes.find(lookup.symbol);

            /* could find a node on the current level */
            if (it != scope->nodes.end()) {
                match.node = it->second.get();
                match.module = nullptr; /* ctx.module */

                return true;
            }

            /* go up in one level as long as the parent container is a namespace */
            if (lang::isNamespace(scope->parent)) {
                scope = static_cast<Namespace*>(scope->parent);

                /* module level reached */
                if (scope->type == NodeType::MODULE)
                    break;
            } else
                return false; // error(Invalid structure)
        }


        /* Could not find a node up to the local module level;
         * will no search through the importted modules.
         * (imports are not transitive: will not look into the
         *  current module's imports imports)
         */
        if (scope && scope->type == NodeType::MODULE) {
            const Module* currentModule = static_cast<Module*>(scope);
            auto import = currentModule->imports.begin();

            /* scan all imported modules */
            while (import != currentModule->imports.end()) {
                const SRef<Module> importModule = import->second;
                auto it = importModule->nodes.find(lookup.symbol);

                /* could find a node in one of the imported modules */
                if (it != importModule->nodes.end()) {
                    match.node = it->second.get();
                    match.module = importModule.get();

                    return true;
                }

                ++import;
            }
        }

        /* Finally, scan the global scope */
        // TODO! ctx.globalScope

        /* could not find a node neither in the namespace hierarchy nor in imported modules */
        return false;
    }

}

#endif