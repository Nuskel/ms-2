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
    std::vector<lang::Error> errors;

    ModuleMap modules;
    SRef<Module> module;
    SRef<Namespace> scope;
    SRef<Namespace> globalScope;

    TypeRegistry types;
    LiteralRegistry literals;
    Instructions instructions;

    //

    Context();

    //

    Status makeCurrent(SRef<Source> source);

    Status decl(Symbol);

    Status decl(Symbol, SRef<Namespace> scope);

    Status defineType(SRef<Proto> proto, TypeClass typeClass = TypeClass::PROTO);

    Status registerModule(SRef<Module>);

    Status bindModule();

    Status enterModule(const std::string_view name);

    Status createLiteral(Literal& result, const Token& token);

    //

    template <typename... Ts>
    Status throwd(Status s, const std::string& fmt, Ts... args) {
      errors.emplace_back(s, debug::sformat(fmt.c_str(), args...), SourceLocation(currentSource.get()));

      return s;
    }

    template <typename... Ts>
    Status throwd(Status s, size_t pos, const std::string& fmt, Ts... args) {
      errors.emplace_back(s, debug::sformat(fmt.c_str(), args...), SourceLocation(currentSource.get(), pos));

      return s;
    }

    template <typename... Ts>
    Status throwd(Status s, size_t start, size_t end, const std::string& fmt, Ts... args) {
      errors.emplace_back(s, debug::sformat(fmt.c_str(), args...), SourceLocation(currentSource.get(), start, end));

      return s;
    }

    /*
    template <typename... Ts>
    Status throwd(Status s, size_t pos, const std::string& fmt, Ts... args) {
      std::string formatted { debug::sformat(fmt.c_str(), args...) };

      // if isWarning(s)
      debug::printsf_ignore_debug_mode("$1[Error@%%] <$r$1$b%%$r$1> %%", "Comp", s, formatted);

      // if logCode
      if (currentSource && pos < currentSource->tokenCount()) {
        const Token& token = currentSource->tokens[pos];
        
        std::cout << currentSource->getMarkedLine(token.line, token.col, token.col) << '\n';
      }

      return s;
    }

    template <typename... Ts>
    Status throwd(Status s, size_t start, size_t end, const std::string& fmt, Ts... args) {
      std::string formatted { debug::sformat(fmt.c_str(), args...) };

      // if isWarning(s)
      debug::printsf_ignore_debug_mode("$1[Error@%%] <$r$1$b%%$r$1> %%", "Comp", s, formatted);

      if (end >= currentSource->tokenCount())
        end = currentSource->tokenCount() - 1;

      if (start >= end)
        start = end;

      // if logCode
      if (currentSource && currentSource->tokenCount() > 0) {
        const Token& from = currentSource->tokens[start];
        const Token& to = currentSource->tokens[end];

        /*if (to.line > from.line)
          std::cout << currentSource->getMarkedLine(currentSource->line, from.col, to.col) << '\n';
        else*//*
          std::cout << currentSource->getMarkedLine(from.line, from.col, to.col) << '\n';
      }

      return s;
    }
    */

    void logErrors(bool onlyRoot = false);

    void logTrace();

    /* Accessors */

    inline bool inErrorState() const {
      return errors.size() > 0;
    }

  };

  Status registerSource(ms::Context& ctx, const std::string& file);

}

#endif
