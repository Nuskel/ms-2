#ifndef MS_LANG_HPP
#define MS_LANG_HPP

#include <string>
#include <string_view>
#include <vector>
#include <iostream>
#include <variant>

#include "env.hpp"
#include "memory.hpp"
#include "source.hpp"

namespace ms {

  struct Symbol;
  struct Literal;
  struct Reference;
  struct Type;
  struct Entity;
  struct Namespace;

}

namespace ms {

  struct TraceElement {

    Status status { Status::FAIL };
    std::string message;
    CodePos pos;

  };

  struct Trace {

    std::vector<TraceElement> trace;
    
    inline void newlayer() {}

  };

  struct Symbol {

    std::string value;
    int modifiers;

    Symbol() {}
    Symbol(const std::string_view name) : value(name) {}

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

  /* ====================== */

  /* ********** Operations */

  enum class OpClass {

    UNKNOWN,

    NOT,

    ADD,
    SUB,
    MUL,
    DIV

  };

  enum class OpRank : char {

    UNARY = 1,
    BINARY = 2,
    TERTIARY = 3

  };

  struct Operation {

    OpClass type { OpClass::UNKNOWN };
    char rank { 2 }; // unary, binary, tertiary
    
  };

  struct Operations {
    
    static constexpr inline Operation NOT { .type = OpClass::NOT, .rank = 1 };
    static constexpr inline Operation ADD { .type = OpClass::ADD };

  };

  /* ********** */

  struct Type;
  enum class TypeClass;

  struct TypeDef {

    /* Can be an integral type (inbuilt).
     * - Proto
     * - Function (functions are returnables types)
     */

    virtual SRef<Function> castfn(Type target) = 0;

    virtual Status invokeOp(const Operation op, const std::vector<Type>& types) = 0;

  };

  enum class TypeClass {

    NONE,
    LITERAL,
    REFERENCE,
    FUNCTION,
    PROTO

  };

  struct Type {

    std::string name;
    TypeClass typeClass;
    SRef<TypeDef> typeDef;

    friend inline bool operator==(const Type& left, const Type& right) {
      return left.typeClass == right.typeClass;
    }

  };

  struct TypeMap {



  };

  /* ====================== */

  /* ********** */

  enum class EntityType : int {

    UNKNOWN,

    MODULE,
    FUNCTION,
    VAR

  };

  struct Entity {

    Symbol symbol;
    EntityType type { EntityType::UNKNOWN };
    WRef<Entity> parent; // weak_ptr to avoid cyclic depedencies
    msx::Address address { 0 };

    Entity() {}
    Entity(const Symbol& p_symbol, EntityType p_type = EntityType::UNKNOWN) : symbol(p_symbol), type(p_type) {}

    virtual ~Entity() {
      std::cout << "~Entity(" << symbol.value << ")\n";
    }

    inline bool isNamespace() {
      return type == EntityType::MODULE;
    }

  };

  struct Namespace : public Entity {

    HMap<Symbol, SRef<Entity>, SymbolHash> entities;

    Namespace() {}

    virtual Status add(SRef<Entity>);

    virtual SRef<Entity> find(const std::string_view name);

    virtual inline bool contains(const Symbol symbol) {
      return entities.find(symbol) != entities.end();
    }

  };

  struct Module : public Namespace {

    SRef<Source> source { nullptr };

    Module() {}

    Module(const Symbol& symbol);

  };

  struct Variable : public Entity {

    Type valueType;

  };

  struct Function : public Namespace {

    Type returnType;
    std::vector<SRef<Variable>> params;
    


  };

  struct Block {};

  // --

  

  struct Proto : public Namespace, public TypeDef {

    HMap<Type, SRef<Function>> castFunctions;
    HMap<Operation, SRef<Function>> opFunctions;

    inline SRef<Function> findOpFunc(const Operation& op) {
      const auto& entry = opFunctions.find(op);

      return entry != opFunctions.end() ? entry->second : nullptr;
    }

  };

  struct Literal {

    using ID = long; // Literal ID
    using LiteralValue = void*;
    using Intermediate = std::variant<msx::Integral, msx::Decimal>;

    ID id { -1 };
    Type type;
    Intermediate value;

    bool isIntermediate { false };

    LiteralValue fetch();

    template <typename T>
    inline const T& fetch0() {
      return *static_cast<T*>(fetch());
    }

  };

  struct Reference {

    Type refType;
    SRef<Entity> referenced;
    bool constant;

    msx::Address asAddress() const {
      return 0;
    }

  };

  enum class ValueClass {

    UNKNOWN,
    LITERAL,
    REFERENCE

  };

  struct Value {

    using ValueType = std::variant<Literal, Reference>;

    ValueClass valueClass;
    ValueType content;

    msx::Integral asIntegral() const {
      return std::get<msx::Integral>(std::get<Literal>(content).value);
    }

    msx::Decimal asDecimal() const {
      return std::get<msx::Decimal>(std::get<Literal>(content).value);
    }

    msx::Address asAddress() const {
      return std::get<Reference>(content).asAddress();
    }

  };

  struct TypedValue {

    Type type;
    Value value;

  };

  inline msx::Integral integral(const Value& value) {
    return std::get<msx::Integral>(std::get<Literal>(value.content).value);
  }

  // DEFAULT TYPES
  namespace types {

    struct SimpleType : public TypeDef {

      inline Status invokeOp(const Operation op, const std::vector<Type>& types) {
        return Status::ERR_INVALID_TYPE;
      }

    };

    struct IntDef : public SimpleType {

      inline Status invokeOp(const Operation op, const std::vector<Type>& types) {
        if (op.type == OpClass::ADD && types.size() == 1) {
          Type t { types[0] };

          if (t.typeDef.get() == this) {
            // int + int OK
          }

          SRef<Function> castfn = t.typeDef->castfn(t);

          if (castfn) {
            // int + (int) any OK
          }

          if (t.typeClass == TypeClass::PROTO) {
            Proto* proto = dynamic_cast<Proto*>(t.typeDef.get());
            SRef<Function> opfn = proto->findOpFunc(op);

            if (opfn) {
              // int + fncall() OK
            }

            // TODO: is there a standard concat function??
          }
        }

        return Status::ERR_INVALID_TYPE;
      }

    };

    static Type IntType = Type {
      .name = "int",
      .typeClass = TypeClass::LITERAL,
      .typeDef = std::make_shared<IntDef>()
    };

    static Type Invalid = Type {
      .name = "invalid",
      .typeClass = TypeClass::NONE
    };

    static Type Reference = Type {
      .name = "reference",
      .typeClass = TypeClass::REFERENCE,
      .typeDef = nullptr
    };

  }

  // --

  struct ExpressionResult {

    Type type { .name = "__unset__" };
    Value value;

  };

  struct Expression {

    std::string debugName;
    bool isTerminator { false };

    ExpressionResult result;
    URef<Expression> leftChild;
    URef<Expression> rightChild;

    Expression() {}
    virtual ~Expression() {
      std::cout << "~Expression(" << debugName << ")\n";
    }

    Status split(Expression& left, Expression& right);

    Expression& left();

    Expression& right();

  };

  // * Locator *

  struct EntityLookup {

    SRef<Namespace> scope;
    std::string module;
    std::string name;

    EntityLookup() {}
    EntityLookup(const SRef<Namespace> p_scope, const std::string_view p_name) : scope(p_scope), name(p_name) {}

  };

  struct EntityMatch {

    SRef<Module> module { nullptr };
    std::vector<SRef<Namespace>> path;
    SRef<Entity> found { nullptr };

    inline bool notFound() {
      return found == nullptr;
    }

  };

  // --

  using ModuleMap = HMap<std::string, SRef<Module>>;

  /**********************************
   *
   **********************************/

  bool validIdentifier(const std::string_view ident);

  Symbol readSymbol();

  /* Tries to locate an entity following the scopes up to module level.
   * If no entity can be matched {found} will be empty.
   *
   * @param scope Scope to start with [required]
   * @param name Symbol name [required]
   * @return EntityMatch with statistics of query and result
   */
  EntityMatch lookup(EntityLookup);

  SRef<Module> createModule(const Source&);

  /* */

  Type combinedType(Type left, Type right);

  /* UTIL */

  inline SRef<Namespace> toNamespace(SRef<Entity> entity) {
    return std::dynamic_pointer_cast<Namespace>(entity);
  }

  template <typename T>
  inline SRef<T> castEntity(SRef<Entity> entity) {
    return std::dynamic_pointer_cast<T>(entity);
  }

  inline Type varType(SRef<Entity> entity) {
    if (entity && entity->type == EntityType::VAR) {
      return castEntity<Variable>(entity)->valueType;
    }

    return types::Invalid;
  }

  inline std::string entityName(SRef<Entity> entity) {
    if (entity != nullptr)
      return entity->symbol.value;

    return "???";
  }

}

#endif
