#include "misc.hpp"
#include <iostream>

namespace ms {

  std::string filename(std::string_view path, char separator) {
    size_t s = 0,
      e = path.length(),
      i = e - 1;

    for (; i > 0; i--) {
      // if an extension separator was found, set the end
      if (path[i] == separator)
        e = i;

      // set start on the first directory separator /
      if (i < e && path[i] == '/') {
        s = i + 1;
        break;
      }
    }

    if (e - s > 0)
      return std::string(path, s, e - s);

    return std::string(path);
  }

}