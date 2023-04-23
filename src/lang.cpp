#include "lang.hpp"

namespace ms {

  Module::Module(const Symbol& symbol) {
    this->symbol = symbol;
  }

  Status Namespace::add(SRef<Entity> entity) {
    if (!entity)
      return Status::FAIL;

    if (contains(entity->symbol))
      return Status::FAIL_DUPLICATE;

    entities.emplace(std::make_pair(entity->symbol, entity));

    return Status::SUCCESS;
  }

  SRef<Entity> Namespace::find(const std::string_view name) {
    return nullptr;
  }

  SRef<Function> Proto::castfn(const Type target) {
    const auto& fn = castFunctions.find(target);

    if (fn == castFunctions.end())
      return nullptr;

    return fn->second;
  }

  Expression& Expression::left() {
    if (!leftChild) {
      leftChild = std::make_unique<Expression>();
    }

    return *leftChild;
  }

  Expression& Expression::right() {
    if (!rightChild) {
      rightChild = std::make_unique<Expression>();
    }

    return *rightChild;
  }

  // --

  EntityMatch lookup(EntityLookup lookup) {
    SRef<Namespace> scope = lookup.scope;
    std::vector<SRef<Namespace>> path;

    while (scope) {
      const auto& ex = scope->entities.find(Symbol(lookup.name));

      path.push_back(scope);

      if (ex != scope->entities.end())
        return EntityMatch { .path = path, .found = ex->second };

      const auto& parent = scope->parent.lock();

      if (parent && parent->isNamespace())
        scope = toNamespace(parent);
      else
        break;
    }

    return EntityMatch {};
  }

  bool validIdentifier(const std::string_view ident) {
    static const std::vector<std::string> restrictedNames {
      "__main__",
    };

    for (size_t i = 0; i < restrictedNames.size(); i++) {
      if (restrictedNames[i] == ident) {
        return false;
      }
    }

    return ident.length() > 0 && ident[0] != '#';
  }

  SRef<Module> createModule(const Source& source) {
    SRef<Module> module { std::make_shared<Module>(Symbol { source.name }) };

    return module;
  }

  // --

  // !! NOT SUFFICIENT !!
  // must depend on operation
  Type combinedType(const Type left, const Type right) {
    switch (left.typeClass) {
      case TypeClass::LITERAL: {
        if (left.typeDef == types::IntType.typeDef) {
          if (right.typeClass == TypeClass::PROTO) {
            const SRef<Function> castfn = right.typeDef->castfn(left);

            if (!castfn)
              
          }
        }

        break;
      }

      default:
        break;
    }

    /* Cases:
     *  A) function invocation *function name* (params) -> Type C
     *  B) "simple combination" = fallback for A + B = *always* a type C; predefined
     */
    return Type { .name = left.name + "&" + right.name, .typeClass = TypeClass::LITERAL, .typeDef = nullptr };
  }

  // -- test

  struct FInvocation {

    std::vector<void*> params;
    Type result;

  };

  Status invoke(Context& ctx, Function& fn, /* inout */ FInvocation& invocation) {
    // check param conditions ?

  }

}