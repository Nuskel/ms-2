#ifndef MS_TYPE_HPP
#define MS_TYPE_HPP

#include "env.hpp"
#include "source.hpp"
#include "lang2.hpp"

namespace ms {

  class TypeDef {};

}

namespace ms {

  using TypeMods = unsigned int;

  enum class TypeClass : int {

    kNone = 0,
    kUndefined = 1,
    kNull = 2,

    kInt,
    kFloat,
    kString,

    kObject,
    kProto,

    kAny

  };

  enum class TypeMod : unsigned char {

    kNone = 0

  };

  class Type {

    TypeClass typeClass { TypeClass::kNone };
    TypeMods typeMods { (TypeMods) TypeMod::kNone };
    Symbol name { Symbol::kNone };
    Module* owner { nullptr };
    size_t size { 0 };
    SourceLocation sourceDefinition;

    TypeDef* def;
    // Todo: OperationMap <Operation op, Type other, Type result> -> bad?

  };

}

namespace ms {

  class ProtoDef : public TypeDef {

    Node* node;

  };

}

#endif