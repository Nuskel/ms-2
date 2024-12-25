#ifndef MS_COMPILER_HPP
#define MS_COMPILER_HPP

#include <string>
#include <algorithm>
#include <variant>

#include "env.hpp"
#include "context.hpp"
#include "source.hpp"
#include "lang.hpp"
#include "ast.hpp"
#include "opcode.hpp"
#include "debug.hpp"

namespace ms {

  class CompUnit {

    public:
      ModuleMap imports;
      Instructions instructions;

  };

  class Compiler {

    public:
      Compiler(Context& context) : ctx(context) {
        ast.program = std::move(ast::makeNode<ast::ProgramNode>("__MAIN__"));

        // TODO: prepare somewhere else
        // INBUILT types
        ast::append(ast.program.get(), ast::makeNode<ast::TypeDecl>("int"));
        ast::append(ast.program.get(), ast::makeNode<ast::TypeDecl>("dec"));
        ast::append(ast.program.get(), ast::makeNode<ast::TypeDecl>("str"));
      }

      Status compile(Source&);

      Status generateAST(Source&);

      Status generateFnDefAST(Source& source, size_t i, size_t& end, UPtr<ast::FnDecl>& result);

      Status generateExprAST(Source& source, ast::Expression& expr);

      Status enterScope(ast::Tree);
      Status leaveScope();

    private:
      Context& ctx;
      ast::ASTContext ast;
      CompUnit unit; // current

    public:
      ast::Tree programTree() const {
        return ast.program.get();
      }

  };

}

namespace ms {

  Status parse(Context& ctx, Instructions& i);

  Status parse(Context& ctx, Source& source, Instructions& i);

  /* Converts all instructions to the numeric value, all intermediates
   * to bytes and all other non-linkage stuff.
   */
  Status compile(Context& ctx, Instructions& instructions);

  Status parseObject(Context& ctx, Source& s, Expression& e, size_t from, size_t to, SRef<Object>& result);

  /* Parses an expression as long as the tokens seem to belong the active one.
   *
   * @param ctx -
   * @param source
   * @param expr - Output of metadata
   */
  Status parseExpression(Context& ctx, Source& source, Expression& expr);

  Status parseExpression(Context& ctx, Source& source, Expression& expr, size_t from, size_t to);

  size_t rightBound(Source& src, size_t pos);

  size_t leftBound(Source& src, size_t pos);

  // --

  Status decl(Context& ctx, SRef<Namespace> scope, Symbol symbol, Decl decl = {});

  Status def(Context& ctx, SRef<Namespace> scope, Symbol symbol, SRef<Entity> member, bool requiresDecl = false);

}

#endif
