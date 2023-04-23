#ifndef MS_COMPILER_HPP
#define MS_COMPILER_HPP

#include <string>
#include <algorithm>

#include "env.hpp"
#include "context.hpp"
#include "source.hpp"
#include "lang.hpp"
#include "opcode.hpp"
#include "debug.hpp"

namespace ms {

  Status parse(Context& ctx, Instructions& i);

  Status parse(Context& ctx, Source& source, Instructions& i);

  /* Converts all instructions to the numeric value, all intermediates
   * to bytes and all other non-linkage stuff.
   */
  Status compile(Context& ctx, Instructions& instructions);

  /* Parses an expression as long as the tokens seem to belong the active one.
   *
   * @param ctx -
   * @param source
   * @param expr - Output of metadata
   */
  Status parseExpression(Context& ctx, Source& source, Expression& expr);

  size_t rightBound(Source& src, size_t pos);

}

#endif
