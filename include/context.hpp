#ifndef MS_CONTEXT_HPP
#define MS_CONTEXT_HPP

#include <string>
#include <string_view>
#include <vector>

#include "env.hpp"
#include "source.hpp"
#include "opcode.hpp"
#include "lang.hpp"
#include "misc.hpp"

namespace ms {

  struct LiteralRegistry {

    std::vector<Literal> literals;

  };

  struct TypeRegistry {



  };

  struct Context {

    std::vector<std::string> sourceLocations;
    SRef<Source> entrySource { nullptr };
    SRef<Source> currentSource { nullptr };
    SourceMap sources;

    Trace trace;

    ModuleMap modules;
    SRef<Module> module;
    SRef<Namespace> scope;

    TypeRegistry types;
    LiteralRegistry literals;
    Instructions instructions;

    //

    Context();

    //

    Status makeCurrent(SRef<Source> source);

    Status decl(Symbol);

    Status registerModule(SRef<Module>);

    Status bindModule();

    Status enterModule(const std::string_view name);

    Status createLiteral(Literal& result, const Token& token);

    //

    template <typename... Ts>
    Status throwd(Status s, const std::string& fmt, Ts... args) {
      std::string formatted { debug::sformat(fmt.c_str(), args...) };

      // if isWarning(s)
      debug::printsf_ignore_debug_mode("$1[Error@%%] <$r$1$b%%$r$1> %%", "Comp", s, formatted);

      // if logCode
      if (currentSource && currentSource->token < currentSource->tokenCount()) {
        const Token& token = currentSource->tokens[currentSource->token];

        std::cout << currentSource->getMarkedLine(currentSource->line, token.col + 1, token.col + 1) << '\n';
      }

      return s;
    }

    void logTrace();

  };

  Status registerSource(ms::Context& ctx, const std::string& file);

}

#endif
