#ifndef MS_TYPES_HPP
#define MS_TYPES_HPP

namespace ms::types {

    enum class VarScope : unsigned char {

        None = 0,
        Global,
        Local,
        Param

    };

    using VarMods = unsigned char;

    enum class VarMod : VarMods {

        kNone       = 0,
        kConst      = 1,
        kTemp       = 1 << 1,
        kProtected  = 1 << 2

    };

    enum class Operator : unsigned int {

        kNone = 0,

        Add,
        Sub,
        Mul,
        Div,

        Comma,
        
        kPlus,
        kMinus,
        kMultiply,
        kDiv,

        kOpenParenthesis,
        kCloseParenthesis,

        kOpenBracket,
        kCloseBracket

    };

    enum class FnMod : int {

        None,

        Hidden,
        Internal,
        Atomic

    };

}

#endif