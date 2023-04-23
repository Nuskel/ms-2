#ifndef MS_ENV_HPP
#define MS_ENV_HPP

#include <memory>
#include <string>
#include <string_view>
#include <unordered_map>

// === Dynamic Tables ===
// Idea: https://www.youtube.com/watch?v=kDqS1xVWGMg

// Generates a vector containing pair literals filled out in the source code.
#define MS_VALUE_MAP(K, V) std::vector<std::pair<K, V>>

// Generates the lookup function for the 'table'-vector. Contains a static const
// map copy of the vector to be accessed at runtime.
#define MS_MAP_LOOKUP(func, table, K, V, fallback)                                  \
    V func(K key) {                                                                 \
        static const auto map = std::unordered_map<K, V> { table.begin(), table.end() };      \
        static V staticFallback = fallback;                                         \
                                                                                    \
        if (map.find(key) == map.end())                                             \
            return /*const_cast<V>(*/staticFallback/*)*/;                           \
                                                                                    \
        return map.at(key);                                                         \
    }

namespace ms {

  using ulong = unsigned long;
  using uint  = unsigned int;

  template <typename T>
  using URef = std::unique_ptr<T>;

  template <typename T>
  using SRef = std::shared_ptr<T>;

  template <typename T>
  using WRef = std::weak_ptr<T>;

  template <typename K, typename V, typename KeyHash = std::hash<K>>
  using HMap = std::unordered_map<K, V, KeyHash>;

  struct Context;

  enum Status {

    SUCCESS,
    FAIL,
    FAIL_DUPLICATE,
    INTERNAL,
    ERR_INVALID_TYPE

  };

}

#endif
