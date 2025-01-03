#ifndef MS_MISC_HPP
#define MS_MISC_HPP

#include <filesystem>
#include <string>
#include <sstream>
#include <vector>
#include <iostream>
#include <initializer_list>
#include <memory>

#define MS_REQUIRE_FIELD(name, type) requires (T t) { { t.name } -> std::same_as<type&>; }

namespace ms {

  /* ------- */

  template <typename T>
  struct LList {
    using value_type = T;

    struct Entry {

      T value;
      Entry* prev { nullptr };
      Entry* next { nullptr };

      Entry(T&& value) {}

    };

    private:
      Entry* head { nullptr };
      Entry* tail { nullptr };
      size_t size_m { 0 };

    public:
      LList() {}

      void add(T&& value) {
        Entry* e = new Entry(value);

        e->prev = this->tail;

        this->tail = e;
        this->size_m++;
      }

      void insert(size_t index, T&& value) {
        if (index > size_m) {
          throw "index out of bounds";
        }

        Entry* e = new Entry(value);
        Entry* c = head;

        for (size_t i = 0; i < index; i++) {
          c = c->next;
        }

        e->prev = c;

        // TODO: check that c !== nullptr
        e->next = c->next;

        if (c->next)
          c->next->prev = e;

        c->next = e;
      }

      T get(size_t index) {
        if (index > size_m) {
          throw "index out of bounds";
        }

        Entry* e = head;

        for (size_t i = 0; i < index; i++) {
          e = e->next;
        }

        return e->value;
      }

      size_t size() {
        return this->size_m;
      }
  };

  template <typename T, typename X = std::reference_wrapper<T>>
  struct RefList : public std::vector<X> {

    explicit RefList(std::initializer_list<X> l) :
      std::vector<X>(l) {}

    inline T& operator[](size_t index) const {
      return this->at(index).get();
    }

  };

  /* ------- */

  /* Creates a RefList using the varg params. */
  template<typename... Args, typename T = std::common_type_t<Args...>>
  RefList<T> reflist(Args&&... args) {
    return RefList<T> { std::forward<Args>(args)... };
  }

  template <typename B, typename D>
  inline std::shared_ptr<D> derive(std::shared_ptr<B> base) {
    return std::dynamic_pointer_cast<D>(base);
  }

  // https://stackoverflow.com/a/36120483
  template<typename T, typename F>
  std::unique_ptr<T> unique_ptr_cast(std::unique_ptr<F>&& ptr){
    return std::unique_ptr<T>{static_cast<T*>(ptr.release())};
  }

  template<typename T, typename F>
  std::unique_ptr<T> move_cast(std::unique_ptr<F>&& ptr) {
    return std::unique_ptr<T>({static_cast<T*>(ptr.release())});
  }

  inline bool fexists(const std::string& filename) {
    std::filesystem::path dpath { filename };

    return std::filesystem::exists(dpath);
  }

  inline bool fexists(const std::string& dir, const std::string& filename) {
    std::filesystem::path dpath { dir + "/" + filename };

    return std::filesystem::exists(dpath);
  }

  /* Returns the filename by stripping off the extension part. */
  std::string filename(std::string_view path, char separator = '.');

  /* True for 0..9. */
  constexpr inline bool isDigit(const char c) {
    return c >= '0' && c <= '9';
  }

  constexpr inline bool isNaturalNumber(const std::string_view str) {
    for (size_t i = 0; i < str.length(); i++) {
      if (!isDigit(str[i]))
        return false;
    }

    return true;
  }

  /* Parses an integer of given base from a given string.
    * The input must be null-terminated with the format of
    * [0-9]*
    */
  template <typename String = std::string_view>
  constexpr int parseInt(const String& s, const int base = 10, const int offset = 0) {
    int i = 0, off = offset, b = 1;

    while (s[off] && isDigit(s[off++]));

    for (--off; off >= offset; off--) {
      i += (s[off] - '0') * b;
      b *= base;
    }

    return i;
  }

  template <typename T> // TODO: is integral
  std::string toBinary(const T& data) {
    std::stringstream buf;

    for (long i = (sizeof(T) * 8) - 1; i >= 0; i--) {
      buf << ((data & (1 << i)) > 0 ? '1' : '0');

      if (i > 0 && (i % 8) == 0)
        buf << '\'';
    }

    return buf.str();
  }

}

#endif
