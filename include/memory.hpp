#ifndef MS_MEMORY_HPP
#define MS_MEMORY_HPP

#include <string>
#include <variant>

// msx - ms execution (runtime part)
namespace msx {

  using Integral = long long;
  using Decimal = long double;
  using Address = long int;
  using String = std::string;

  struct Proto {};

  struct Object {};

  struct Function {};

  enum class DataType {

    UNSET,

    INTEGRAL,
    DECIMAL,
    STRING,

    OBJECT,
    PROTO,
    FUNCTION

  };

  using ValueUnion = std::variant<Integral, Decimal, String, Proto, Object, Function>;

  struct CompValue {

    DataType type { DataType::UNSET };
    ValueUnion value;

    template <typename T>
    T get() {
      switch (type) {
        case DataType::INTEGRAL:
          return std::get<Integral>(value);

        default:
          return "";
      }
    }

  };

  static CompValue makeIntegral(Integral i) {
    CompValue val { .type = DataType::INTEGRAL, .value = i };

    return val;
  }

}

#endif