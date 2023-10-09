#include "misc.hpp"

namespace ms {

  std::string filename(std::string_view path, char separator) {
    std::stringstream buf;
    bool name {false};

    for (size_t i = path.length() - 1; i >= 0; i--) {
      if (!name && path[i] == separator)
        name = true;
      else if (name)
        buf << path[i];
    }

    return buf.str();
  }

}