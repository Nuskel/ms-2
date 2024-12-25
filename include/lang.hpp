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
  struct Function;

}

namespace ms::lang {

  struct Error {

    Status status { Status::Fail };
    std::string message;
    SourceLocation location;

    Error() {}
    Error(const Status pStatus, const std::string& pMessage, const SourceLocation& pLocation) :
      status(pStatus), message(pMessage), location(pLocation) {}

  };

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

    const static Symbol NONE;

    Symbol() {}
    Symbol(const std::string_view name) : value(name) {}

    inline Symbol& operator=(const Symbol& s) {
      this->value = s.value;
      this->modifiers = s.modifiers;

      return *this;
    }

    friend inline bool operator==(const Symbol& l, const Symbol& r) {
      debug::printsf("CMP: %% == %%", l.value, r.value);
      return l.value == r.value;
    }

    friend inline std::ostream& operator<<(std::ostream& stream, const Symbol& symbol) {
      return stream << symbol.value;
    }

    static bool nameValid(const std::string& value) {
      return false;
    }

  };

  inline const Symbol Symbol::NONE = Symbol("__none__");

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

    inline friend bool operator ==(const Operation& a, const Operation& b) {
      return a.type == b.type;
    }
    
  };

  struct OperationHash {
    
    size_t operator()(const ms::Operation& op) const {
      return std::hash<char>{}((char) op.type);
    }

  };

  struct Operations {
    
    static constexpr inline Operation NOT { .type = OpClass::NOT, .rank = 1 };
    static constexpr inline Operation ADD { .type = OpClass::ADD };

  };

  /* ********** */

  struct Type;

  enum class TypeClass : int {

    NONE,
    LITERAL,
    REFERENCE,

    FUNCTION,
    PROTO,
    OBJECT,
    ARRAY,
    TUPEL, // like array with different types

    SIMPLE,
    ANY

  };

  struct TypeDef {

    /* Can be an integral type (inbuilt).
     * - Proto
     * - Function (functions are returnables types)
     */

    Symbol name { Symbol::NONE };
    TypeClass typeClass { TypeClass::NONE };

    virtual ~TypeDef() {}

    virtual SRef<Function> castfn(const Type target);

    virtual SRef<Function> opfn(const Operation op, const Type target);

    friend inline bool operator==(const TypeDef& left, const TypeDef& right) {
      return left.typeClass == right.typeClass && &left.name == &right.name;
    }

  };

  struct Type {

    SRef<TypeDef> def;

    Type() {}
    Type(const SRef<TypeDef>& typeDef) : def(typeDef) {}

    inline bool operator ==(const Type& another) {
      return def == another.def;
    }

    inline bool operator !=(const Type& another) {
      return def != another.def;
    }

    inline Type& operator =(const Type& another) {
      def = another.def;

      return *this;
    }

    inline Type& operator=(const SRef<TypeDef>& typeDef) {
      def = typeDef;

      return *this;
    }

    inline bool hasDef() const {
      return def ? true : false;
    }

    inline const Symbol name() const {
      return def ? def->name : Symbol::NONE;
    }

    inline const TypeClass typeClass() const {
      return def ? def->typeClass : TypeClass::NONE;
    }

    friend inline bool operator==(const Type& type, const TypeClass typeClass) {
      return type.typeClass() == typeClass;
    }

    friend inline bool operator==(const Type& left, const Type& right) {
      return left.def == right.def;
    }

  };

  /*
  struct Type {

    std::string name { "unnamed" };
    TypeClass typeClass { TypeClass::NONE };
    TypeDef* typeDef { nullptr };

    friend inline bool operator==(const Type& left, const Type& right) {
      return left.typeClass == right.typeClass && left.typeDef == right.typeDef;
    }

  };
  */

  struct TypeHash {
    
    size_t operator()(const ms::Type& type) const {
      const size_t th = std::hash<int>{}((int) type.typeClass());
      const size_t sh = SymbolHash{}(type.name());

      return th ^ (sh << 1);
    }

  };

  struct TypeMap {



  };

  /* ====================== */

  /* ********** */

  enum class EntityType : int {

    UNKNOWN,

    MODULE,
    NAMESPACE,
    OBJECT,
    PROTO,
    ARRAY,
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
      return (int) type >= (int) EntityType::MODULE && (int) type <= (int) EntityType::FUNCTION;
    }

  };

  struct Decl {

    // DeclSpec
    bool isConst {false};
    bool isAtomic {false};
    bool isDef {false};

    SRef<Entity> entity;

    Decl() {}
    Decl(SRef<Entity> p_entity) : entity(p_entity) {}

  };

  struct Namespace : public Entity {

    using DeclMap = HMap<Symbol, Decl, SymbolHash>;

    DeclMap entities;

    Namespace() {
      type = EntityType::NAMESPACE;
    }

    virtual Status add(SRef<Entity>);

    virtual SRef<Entity> find(const std::string_view name);

    virtual inline bool contains(const Symbol symbol) {
      return entities.find(symbol) != entities.end();
    }

  };

  struct Module : public Namespace {

    SRef<Source> source { nullptr };

    Module() {
      type = EntityType::MODULE;
    }

    Module(const Symbol& symbol);

  };

  struct Variable : public Entity {

    Type valueType;
    //msx::Address address; // TODO: public Adressable

    Variable() {}
    Variable(const Symbol& p_symbol) {
      symbol = p_symbol;
      type = EntityType::VAR;
    }

  };

  struct Function : public Namespace {

    Type returnType;
    std::vector<SRef<Variable>> params;
    


  };

  struct Block {};

  // --

  namespace json {

    struct Element {};

    struct Object : Element {

      void put(const std::string& key, Element* value);

    };

    struct Array : Element {};

    struct String : Element {};

    struct Double : Element {};

    struct Bool : Element {};

  }

  struct Object : public Namespace, public TypeDef {

    Object() {
      type = EntityType::OBJECT;
      typeClass = TypeClass::OBJECT;
      symbol = name = Symbol("Object");
    }

  };

  struct Proto : public Object {

    HMap<Type, SRef<Function>, TypeHash> castFunctions;
    HMap<Operation, SRef<Function>, OperationHash> opFunctions;

    Proto() {
      type = EntityType::PROTO;
      typeClass = TypeClass::PROTO;
      symbol = name = Symbol("Proto");
    }

    SRef<Function> castfn(const Type type);

    inline SRef<Function> findOpFunc(const Operation& op) {
      const auto& entry = opFunctions.find(op);

      return entry != opFunctions.end() ? entry->second : nullptr;
    }

  };

  struct Array : public Entity, public TypeDef {

    Type arrayType;

    Array() {
      type = EntityType::ARRAY;
      typeClass = TypeClass::ARRAY;
      symbol = name = Symbol("Array");
    }

  };

  struct Literal {

    using ID = long; // Literal ID
    using LiteralValue = void*;
    using Intermediate = std::variant<msx::Integral, msx::Decimal, std::string>;

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

  namespace stdtypes {

    struct AnyDef : public TypeDef {

      AnyDef() {
        typeClass = TypeClass::ANY;
        name = Symbol("any");
      }

    };

    struct IntDef : public Proto {

      IntDef() {
        typeClass = TypeClass::SIMPLE;
        symbol = name = Symbol("int"); // TODO: mod = LANG_TYPE
      }

    };

    struct DecimalDef : public Proto {

      DecimalDef() {
        typeClass = TypeClass::SIMPLE;
        symbol = name = Symbol("decimal");
      }

    };

    struct StringDef : public Proto {

      StringDef() {
        typeClass = TypeClass::SIMPLE;
        symbol = name = Symbol("string");
      }

    };

    struct UnknownDef : public TypeDef {

      UnknownDef() {
        typeClass = TypeClass::NONE;
        name = Symbol::NONE;
      }

    };

  }

  namespace types {

    inline const Type Int { std::make_shared<stdtypes::IntDef>() };
    inline const Type Decimal { std::make_shared<stdtypes::DecimalDef>() };
    inline const Type String { std::make_shared<stdtypes::StringDef>() };

    inline const Type Unknown { std::make_shared<stdtypes::UnknownDef>() };
    inline const Type Any { std::make_shared<stdtypes::AnyDef>() };

    inline Type makeObject() {
      return Type { std::make_shared<Object>() };
    }

    inline Type makeArray(Type arrayType) {
      SRef<Array> def = std::make_shared<Array>();

      // TODO:
      //  (a) -> Type::template ?
      //  (b) -> TypeRegistry lookup for Array with template type
      def->arrayType = arrayType;

      return Type { def };
    }

  }

  // --

  struct ExpressionResult {

    Type type;
    Value value;
    SRef<Entity> entity;
    bool reduced {false};

  };

  struct Expression {

    std::string debugName;
    bool isRoot { true };
    bool isTerminator { false };
    bool hasAssignment { false };
    bool pushHash { false };

    ExpressionResult result;
    URef<Expression> leftChild;
    URef<Expression> rightChild;

    size_t startToken {0};
    size_t endToken {0};

    SRef<Namespace> scope {nullptr};

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

    using MapEntry = HMapIt<Symbol, Decl>;

    SRef<Module> module { nullptr };
    std::vector<SRef<Namespace>> path;

    bool match;

    MapEntry entry; // only use if match!
    Symbol symbol;
    Decl decl;

    SRef<Entity> entity { nullptr };

    inline bool hasMatch() {
      return match;
    }

    inline bool hasDef() {
      return match && decl.entity;
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

  SRef<Module> createModule(Context& ctx, const Source&);

  /* UTIL */

  inline SRef<Namespace> toNamespace(SRef<Entity> entity) {
    return std::dynamic_pointer_cast<Namespace>(entity);
  }

  template <typename T>
  inline SRef<T> castEntity(SRef<Entity> entity) {
    return std::dynamic_pointer_cast<T>(entity);
  }

  inline void setVarType(SRef<Entity> var, Type type) {
    if (var && var->type == EntityType::VAR) {
      castEntity<Variable>(var)->valueType = type;
    }
  }

  inline Type varType(SRef<Entity> entity) {
    if (entity && entity->type == EntityType::VAR) {
      return castEntity<Variable>(entity)->valueType;
    }

    return types::Unknown;
  }

  inline std::string entityName(SRef<Entity> entity) {
    if (entity != nullptr)
      return entity->symbol.value;

    return "???";
  }

  inline bool isType(const SRef<Entity> entity, const EntityType type) {
    return entity && entity->type == type;
  }

}

#endif
