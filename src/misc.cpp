#include "misc.hpp"

namespace ms {

  std::string filename(std::string_view path, char separator) {
    std::stringstream buf;

    for (size_t i = 0; i < path.length(); i++) {
      if (path[i] == separator)
        break;

      buf << path[i];
    }

    return buf.str();
  }

}