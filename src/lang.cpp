#include "lang.hpp"
#include "context.hpp"

namespace ms {

  SRef<Function> TypeDef::castfn(const Type type) {
    return nullptr;
  }

  SRef<Function> TypeDef::opfn(const Operation op, const Type param) {
    return nullptr;
  }

  // --

  Module::Module(const Symbol& symbol) {
    this->symbol = symbol;
    this->type = EntityType::MODULE;
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
      leftChild->isRoot = false;
      leftChild->scope = scope;
    }

    return *leftChild;
  }

  Expression& Expression::right() {
    if (!rightChild) {
      rightChild = std::make_unique<Expression>();
      rightChild->isRoot = false;
      rightChild->scope = scope;
    }

    return *rightChild;
  }

  // --

  EntityMatch lookup(EntityLookup lookup) {
    SRef<Namespace> scope = lookup.scope;
    std::vector<SRef<Namespace>> path;
    Symbol sym {lookup.name};

    debug::printsf("LOOKUP %% in <%%>", sym, entityName(lookup.scope));

    while (scope) {
      const auto& ex = scope->entities.find(sym);

      path.push_back(scope);

      if (ex != scope->entities.end())
        return EntityMatch {
          .path = path,
          .match = true,
          .entry = ex,
          .symbol = ex->first,
          .decl = ex->second,
          .entity = ex->second.entity
        };

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
      Symbol::NONE.value,
      "__main__",
    };

    for (size_t i = 0; i < restrictedNames.size(); i++) {
      if (restrictedNames[i] == ident) {
        return false;
      }
    }

    return ident.length() > 0 && ident[0] != '#';
  }

  SRef<Module> createModule(Context& ctx, const Source& source) {
    SRef<Module> module { std::make_shared<Module>(Symbol { source.name }) };

    module->parent = ctx.globalScope;

    return module;
  }

  // --

}