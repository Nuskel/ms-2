#include "compiler.hpp"
#include "lang.hpp"
#include "types.hpp"
#include "env.hpp"
#include "misc.hpp"
#include "sformats.hpp"

#define require(amount, token, msg) \
  if ((i + amount) >= source.tokenCount()) \
    return debug::errorsf(Status::FAIL, token, msg);

#define leave(what) { what; break; }
#define consume(n) i += n
#define skip(n) i += n

namespace ms {

  memloc createMemloc(const Value& value);

}

namespace ms {

  Status Compiler::enterScope(ast::Tree scope) {
    if (!scope)
      return Status::Fail;

    debug::printsf("$3{Scope}: %% > %%", ast::type(ast.scope), ast::type(scope));

    ast.layers.emplace(ast.scope);
    ast.scope = scope;

    /* if we are in a function, keep track of it */
    if (ast::NodeType::FnDecl == ast::type(scope))
      ast.function = static_cast<ast::FnDecl*>(scope);

    return Status::Success;
  }

  Status Compiler::leaveScope() {
    if (ast.layers.size() > 0) {
      ast::Tree scope { ast.layers.top() };

      debug::printsf("$3{Scope}: %% > %%", ast::type(ast.scope), ast::type(scope));

      /* cannot leave Program scope again */
      if (ast::NodeType::Program == ast::type(ast.scope))
        return Status::HighestScope;

      /* if the old scope is a function, keep track of it */
      if (ast::NodeType::FnDecl == ast::type(scope)) {
        ast.function = static_cast<ast::FnDecl*>(scope);
      } else {
        ast.function = nullptr;
      }

      ast.scope = scope;
      ast.layers.pop();

      return Status::Success;
    }

    return Status::HighestScope;
  }

  // TODO: SHOULD BE JUST FOR A SINGLE MODULE!
  Status Compiler::generateAST(Source& source) {
    using namespace ast;

    if (SourceState::LEXED != source.state)
      return ctx.throwd(Status::kFail, "source must be lexed to be compiled");

    Status s { Status::kSuccess };
    size_t i = 0; // token
    size_t l = 0; // line

    // TODO: outsource program
    ProgramNode* program = ast.program.get();

    if ((s = enterScope(program)) != Status::Success)
      return ctx.throwd(s, i, "could not enter program scope");

    UPtr<ModuleNode> modulePtr { makeNode<ModuleNode>(source.name, source) };
    ModuleNode* moduleNode = modulePtr.get(); 

    if ((s = append(program, std::move(modulePtr))) != Status::Success)
      return ctx.throwd(s, "could not begin module AST");

    ast.program->modules.emplace(source.name, moduleNode);

    if ((s = enterScope(moduleNode)) != Status::Success)
      return ctx.throwd(s, i, "could not enter module scope");
    
    Tree& scope { ast.scope };

    debug::printsf("[AST] Module: %%", moduleNode->name);
    debug::printsf("[AST] Source: %%", source.name);

    for (; i < source.tokenCount(); i++) {
      const Token& token = source.tokens[i];

      debug::printsf(" -- (%%) $6%% %%$r (%%:%%)", i, token.type, token.value, token.line, token.col);

      switch (token.type) {
        case Tok::UNKNOWN:
          break;

        case Tok::KW_IMPORT: {
          std::string ident;
          ++i;

          if ((s = source.readIdentifier(ident, i)) != Status::SUCCESS)
            return ctx.throwd(s, i + 1, "expected an identifier");

          UPtr<ImportStmt> node { makeNode<ImportStmt>(ident) };
          ImportStmt* stmt = node.get();

          if ((s = append(scope, std::move(node))) != Status::Success)
            return ctx.throwd(s, i, "failed to add import statement");

          debug::printsf("[import] %%", stmt->name);

          break;
        }

        case Tok::KW_DEF: {
          UPtr<FnDecl> declPtr;
          size_t end = i;

          if ((s = generateFnDefAST(source, i + 1, end, declPtr)) != Status::Success)
            return ctx.throwd(s, i, end, "failed to parse function declaration");

          FnDecl* decl { declPtr.get() };

          if ((s = append(scope, std::move(declPtr))) != Status::Success)
            return ctx.throwd(s, i, end, "failed to register function declaration");

          i = end - 1;

          debug::printsf("[def] declared: %%", decl->name);

          if ((s = enterScope(decl)) != Status::Success)
            return ctx.throwd(s, i, "could not enter function scope");

          break;
        }

        case Tok::KW_LET: {
          std::string ident;
          ++i;

          if ((s = source.readIdentifier(ident, i)) != Status::SUCCESS)
            return ctx.throwd(s, i, "expected an identifier");

          // TODO: outsource
          types::VarScope varScope;

          switch (ast::type(ast.block)) {
            case NodeType::Module:
              varScope = types::VarScope::Global;
              break;

            default:
              varScope = types::VarScope::None;
              break;
          }

          if ((s = append(scope, makeNode<VarDecl>(ident, varScope))) != Status::Success)
            return ctx.throwd(s, i, "failed to add var declaration");

          debug::printsf("[let] %%", ident);

          break;
        }

        case Tok::OP_ASSIGN: {
          /*
          std::string ident;

          if ((s = source.readIdentifier(ident, i - 1)) != Status::SUCCESS)
            return ctx.throwd(s, i - 1, "expected an identifier");

          ast::VarDecl* varDecl = findFirst<ast::VarDecl>(scope, ast::NodeType::VarDecl, [&](const ast::VarDecl& decl) { return decl.name == ident; });

          if (!varDecl)
            return ctx.throwd(Status::FAIL, i - 1, "unknown target for assignment: %%", ident);
          */

          size_t targetStart = leftBound(source, i);
          size_t targetEnd = i;
          ast::Expression target(scope, targetStart, targetEnd);

          if ((s = generateExprAST(source, target)) != Status::Success)
            return ctx.throwd(s, targetStart, targetEnd, "invalid target expression");

          size_t start = i + 1;
          size_t end = rightBound(source, start);
          ast::Expression expr(scope, start, end);

          /* next token is "def" -> assignment to an inline fn */
          if (source.peekType(i + 1, Tok::KW_DEF)) {
            UPtr<FnDecl> declPtr;
            size_t end = i + 2;

            generateFnDefAST(source, i + 2, end, declPtr);

            FnDecl* inlineDecl { declPtr.get() };

            ast.program->inlineFnDecls.push_back(std::move(declPtr));
            expr.node = std::move(makeNode<ast::InlineFnDeclExpr>(inlineDecl));
          } else {
            if ((s = generateExprAST(source, expr)) != Status::Success)
              return ctx.throwd(s, start, end, "failed to parse expression");

            if (expr.empty)
              return ctx.throwd(Status::Fail, start, end, "expected expression for assignment");
          }

          UPtr<AssignStmt> stmt = makeNode<AssignStmt>();

          stmt->target = std::move(target.node);
          stmt->expr = std::move(expr.node);
          stmt->location = SourceLocation(&source, i);

          if ((s = append(scope, std::move(stmt))) != Status::Success)
            return ctx.throwd(s, i, end, "failed to parse assignment");

          i = end - 1;

          debug::printsf("[assign] %% = %%", "??", "??");

          break;
        }

        case Tok::KW_END: {
          if (NodeType::Module == ast::type(scope))
            return ctx.throwd(Status::HighestScope, i, "already in module scope");

          if ((s = leaveScope()) != Status::Success)
            return ctx.throwd(s, i, "could not leave scope");

          break;
        }

        case Tok::KW_RETURN: {
          size_t start = i + 1;
          size_t end = rightBound(source, start);
          ast::Expression expr(scope, start, end);

          if ((s = generateExprAST(source, expr)) != Status::Success)
            return ctx.throwd(s, start, end, "unexpected expression");

          debug::printsf("Return: %%", expr.nodeType());
          // TODO: Node

          i = end - 1;
        }

        case Tok::IDENTIFIER: {
          size_t start = i;
          size_t end = rightBound(source, start);
          ast::Expression expr(scope, start, end);

          if ((s = generateExprAST(source, expr)) != Status::Success)
            return ctx.throwd(s, start, end, "unexpected expression");

          if (expr.node) {
            if ((s = append(scope, std::move(expr.node))) != Status::Success)
              return ctx.throwd(s, start, end, "failed to append to scope");
          }

          i = end - 1;

          break;
        }

        default: {
          return ctx.throwd(Status::Fail, i, "unexpected token: %%", token.value);
        }
      }
    }

    if (!scope || NodeType::Module != ast::type(scope))
      return ctx.throwd(s, i - 1, "unclosed scope: %%", ast::type(scope));

    if ((s = leaveScope()) != Status::Success)
      return ctx.throwd(s, i - 1, "could not leave module scope");

    // scope should be program again

    return s;
  }

  /* ----- FUNCTIONS ------ */

  Status appendFnNameAndMods(Context&, ast::ASTContext&, Source&, size_t&, ast::FnDecl*);
  Status appendFnParams(Compiler&, Context&, Source&, size_t&, ast::FnDecl*);
  Status appendFnSuffix(Context&, Source&, size_t&, ast::FnDecl*);

  Status Compiler::generateFnDefAST(Source& source, size_t i, size_t& end, UPtr<ast::FnDecl>& result) {
    using namespace ast;

    Status s { Status::Success };
    size_t start = i;

    UPtr<FnDecl> declPtr { makeNode<FnDecl>() };
    FnDecl* decl = declPtr.get();

    // TODO: better as param (scope)
    decl->parent = ast.scope;

    if ((s = appendFnNameAndMods(ctx, ast, source, i, decl)) != Status::Success)
      return s;

    if ((s = appendFnParams(*this, ctx, source, i, decl)) != Status::Success)
      return s;

    if ((s = appendFnSuffix(ctx, source, i, decl)) != Status::Success)
      return s;

    debug::printsf("[fn] %%, %% mods", decl->name, decl->mods.size());

    end = i + 1;
    result = std::move(declPtr);

    return Status::Success;
  }

  Status appendFnNameAndMods(Context& ctx, ast::ASTContext& ast, Source& source, size_t& i, ast::FnDecl* decl) {
    Status s { Status::Success};

    /* (1) Read modifiers and name: */
    for (; i < source.tokenCount(); i++) {
      const Token& token = source.tokens[i];
      const Tok tok = token.type;

      if (tok == Tok::OP_LEFT_PARENTHESIS) {
        break;
      }

      switch (tok) {
        case Tok::IDENTIFIER: {
          /* current block */
          if (ast.block) {

          }

          // TODO: valid name!

          decl->name = token.value;
          break;
        }

        case Tok::KW_INTERNAL: {
          decl->mods.push_back(types::FnMod::Internal);
          break;
        }

        default:
          return ctx.throwd(Status::Fail, i, "expected either the function name or a modifier: %%", token.value);
      }
    }

    return s;
  }

  Status appendFnParams(Compiler& c, Context& ctx, Source& source, size_t& i, ast::FnDecl* decl) {
    using namespace ast;

    Status s { Status::Success };

    /* empty param list */
    if ((i + 1) < source.tokenCount() && Tok::OP_RIGHT_PARENTHESIS == source.tokens[i + 1].type) {
      return s;
    }

    /* Param:
     *  [type] {name} [= initialValue]
     */
    UPtr<ParamDecl> param { makeNode<ParamDecl>() };

    for (++i; i < source.tokenCount(); i++) {
      const Token& token = source.tokens[i];
      const Tok tok = token.type;

      if (tok == Tok::OP_RIGHT_PARENTHESIS) {
        if ((s = append(&decl->params, std::move(param))) != Status::Success)
          return ctx.throwd(s, i, "could not add param");

        break;
      }

      switch (tok) {
        case Tok::IDENTIFIER: {
          if ((i + 1) < source.tokenCount()) {
            const Token& next = source.tokens[i + 1];

            switch (next.type) {
              case Tok::OP_ASSIGN:
              case Tok::OP_COMMA:
              case Tok::OP_RIGHT_PARENTHESIS: {
                // token must be the identifier

                // TODO: validate
                param->name = token.value;

                break;
              }

              case Tok::IDENTIFIER: {
                // token must be a type name

                if (param->typeGuard.isset())
                  return ctx.throwd(Status::Fail, i + 1, "excess guard type: %%", token.value);

                TypeDecl* type = ast::findFirst<TypeDecl>(decl, NodeType::TypeDecl, [&](const TypeDecl& decl) { return decl.name == token.value; });

                if (!type)
                  return ctx.throwd(Status::Fail, i + 1, "unknown type: %%", token.value);

                // TODO: real TypeDecl
                param->typeGuard.decl = type;

                break;
              }

              default:
                return ctx.throwd(s, i + 1, "unexpected token in parameter pack: %%", next.value);
            }
          } else {
            return ctx.throwd(s, i, "unexpected function declaration end");
          }

          param->name = token.value;
          break;
        }

        case Tok::OP_ELLIPSIS: {
          debug::printsf("[+param] ...");
          break;
        }

        case Tok::OP_ASSIGN: {
          size_t start = i + 1;
          size_t end = start;

          bool searching = true;
          int nesting = 0;

          for (; searching && end < source.tokenCount(); end++) {
            const Token& paramToken = source.tokens[end];

            /* Try to find the next comma or closing parenthesis of the function header.
             * In case of initial values, {} and other brackets may be used to initialize
             * objects, expressions or the user just uses () around some trivial stuff.
             * While the nesting is active, commas or closing parenthesis do not dictate
             * the end of the parameter list, but are part of the initial value.
             * This helper skips to the real end.
             */
            switch (paramToken.type) {
              case Tok::OP_LEFT_CURLY:
              case Tok::OP_LEFT_BRACKET:
              case Tok::OP_LEFT_PARENTHESIS:
                nesting++; break;

              case Tok::OP_RIGHT_CURLY:
              case Tok::OP_RIGHT_BRACKET:
                nesting--; break;

              case Tok::OP_RIGHT_PARENTHESIS: {
                if (!nesting) {
                  searching = false;
                  break;
                } else {
                  nesting--;
                  break;
                }
              }

              case Tok::OP_COMMA: {
                if (!nesting) {
                  searching = false;
                  break;
                }
              }

              default:
                break;
            }
          }

          --end;
          ast::Expression expr(decl, start, end);

          if ((s = c.generateExprAST(source, expr)) != Status::Success)
            return ctx.throwd(s, start, end, "failed to parse expression");

          debug::printsf("(param %%) initial value: %%", param->name, ast::type(expr.node.get()));

          i = end - 1;
          param->initialValue = std::move(expr.node);

          break;
        }

        case Tok::OP_COMMA: {
          if ((s = append(&decl->params, std::move(param))) != Status::Success)
            return ctx.throwd(s, i, "could not add param");

          // next possible param
          param = std::move(makeNode<ParamDecl>());

          break;
        }

        default:
          return ctx.throwd(Status::Fail, i, "unexpected token in param list: %%", token.value);
      }
    }

    return s;
  }

  Status appendFnSuffix(Context& ctx, Source& source, size_t& i, ast::FnDecl* decl) {
    ++i;

    if (i >= source.tokenCount())
      return Status::Fail; // source end

    if (source.tokens[i].type != Tok::OP_COLON)
      return ctx.throwd(Status::Fail, i, "expected :");

    return Status::Success;
  }

  /* ----------- */

  Status parseParenthesis(Context& ctx,
    const Tok tok, const Tok open, const Tok close,
    long type, long* ps, long* pe, long* pdepth, size_t i);

  Status Compiler::generateExprAST(Source& source, ast::Expression& expr) {
    using namespace ast;

    Status s { Status::Success };
    size_t start = expr.start;
    size_t end = expr.end;
    size_t len = end - start;

    debug::printsf("<EXPR %%-%%: %%> $5%%", start, end, expr.purpose, source.concat(start, end));

    if (len == 0) {
      expr.empty = true;
      return Status::Success; // ctx.throwd(Status::Fail, start, "expected a valid token");
    }

    if (len == 1) {
      const Token& token = source.tokens[start];

      switch (token.type) {
        case Tok::OP_COMMA: {
          /* Comma expansion with no sub expressions (,,) -> ("", ",") will split on comma
           * and call this method with just a single comma. If the expansion is used in a
           * function call, the behavior is allowed since this just assumes an undefined
           * param value at that (or any) position.
           */
          if (expr.purpose & FuncParams)
            return Status::Success;

          return ctx.throwd(Status::Fail, start, "unexpected single comma");
        }

        case Tok::OP_NOT: {
          // NotExpr

          return Status::Success;
        }

        case Tok::IDENTIFIER: {
          if (expr.purpose & MemberRef) {
            expr.node = std::move(makeNode<RefExpr>(Symbol(token.value)));

            debug::printsf(" $5%% $r%%", NodeType::RefExpr, token.value);
          } else if (expr.purpose & FuncName) {

            /* TODO: when used via referencing chain "x.y.z()" no check necessary -> mustt be deduced differently ..
             */

            /*
            const auto fnDecl = findFirst<FnDecl>(expr.scope, NodeType::FnDecl, [&](const FnDecl& decl) { return decl.name == token.value; });

            if (!fnDecl)
              return ctx.throwd(Status::Fail, start, "unknown function: %%", token.value);
            */

            expr.node = std::move(makeNode<FnCall>(Symbol(token.value)));

            debug::printsf(" $5%% $r%%", NodeType::FnCall, token.value);
          } else {
            // TODO: not just for var!
            if (!isVarDecl(expr.scope, token.value))
              return ctx.throwd(Status::Fail, start, "unknown symbol: %%", token.value);

            expr.node = std::move(makeNode<RefExpr>(Symbol(token.value)));

            debug::printsf(" $5%% $r%%", NodeType::RefExpr, token.value);
          }

          return Status::Success;
        }

        case Tok::L_INTEGRAL:
        case Tok::L_DECIMAL:
        case Tok::L_STRING: {
          expr.node = std::move(makeNode<LiteralExpr>(token.integral, nullptr));

          debug::printsf(" $5%% $r%% [%%]", NodeType::LiteralExpr, token.integral, nullptr);

          return Status::Success;
        }

        default:
          return ctx.throwd(Status::Fail, start, "unexpected token: %%", token.value);
      }
    }

    /* precedence */
    int p = 0;
    int mp = 3;

    /* parenthesis control */
    long ps[4] = {-1, -1, -1, -1}; // start index of first opening occurence
    long pe[4] = {-1, -1, -1, -1}; // end index of last closing occurence
    long pdepth[4] = {0, 0, 0, 0}; // amount of closing tokens needed to build pairs

    for (size_t i = start; i < end; i++) {
      const Token& token = source.tokens[i];
      const Tok tok = token.type;

      // -- parenthesis control

      /* NOTE: brackets cannot be parsed with a simple precedence.
        * When there are no arithmetic operators on the level of the bracket, the bracket
        * gets parsed first. But if otherwise, the arithmetic operator has the
        * higher precedence and must be the main anchor.
        * e.g.:
        *  a) a[0 + 1] -> first parse [], then 0 + 1
        *  b) a[0] + 1 -> first parse +, then []
        * Therefore, use the exclusing mechanic below instead of
        * simply checking for the precendence of brackets.
        */
      
      MS_ASSERT_STATE(s, parseParenthesis(ctx, tok, Tok::OP_LEFT_PARENTHESIS, Tok::OP_RIGHT_PARENTHESIS, 0, ps, pe, pdepth, i))
      MS_ASSERT_STATE(s, parseParenthesis(ctx, tok, Tok::OP_LEFT_BRACKET, Tok::OP_RIGHT_BRACKET, 1, ps, pe, pdepth, i))
      MS_ASSERT_STATE(s, parseParenthesis(ctx, tok, Tok::OP_LEFT_CURLY, Tok::OP_RIGHT_CURLY, 2, ps, pe, pdepth, i))

      /* Whenever a parenthesis is present in the current level,
       * the scoped content gets parsed later. All precedences
       * of normal operators are parsed on this level before.
       */
      if ((pdepth[0] | pdepth[1] | pdepth[2] | pdepth[3]) > 0)
        continue;

      // == operators ==

      /*
      
      TODO: + and - can be treated as unary operators as well -> decide by precedence!
      
      */

      if (p == 2 && tok == Tok::OP_ADD) {
        ast::Expression left(expr.scope, start, i);
        ast::Expression right(expr.scope, i + 1, end);

        if ((s = generateExprAST(source, left)) != Status::Success)
          return ctx.throwd(s, start, i, "invalid expression");

        if ((s = generateExprAST(source, right)) != Status::Success)
          return ctx.throwd(s, i + 1, end, "invalid expression");

        UPtr<BinaryExpr> binExpr = makeNode<BinaryExpr>();

        binExpr->op = types::Operator::Add;
        binExpr->left = std::move(left.node);
        binExpr->right = std::move(right.node);

        debug::printsf("== $6%% (%%)", binExpr->op, binExpr->base.type);

        expr.node = std::move(binExpr);

        break;
      }

      if (p == 2 && tok == Tok::OP_SUB) {
        ast::Expression left(expr.scope, start, i);
        ast::Expression right(expr.scope, i + 1, end);

        if ((s = generateExprAST(source, left)) != Status::Success)
          return ctx.throwd(s, start, i, "invalid expression");

        if ((s = generateExprAST(source, right)) != Status::Success)
          return ctx.throwd(s, i + 1, end, "invalid expression");

        UPtr<BinaryExpr> binExpr = makeNode<BinaryExpr>();

        binExpr->op = types::Operator::Sub;
        binExpr->left = std::move(left.node);
        binExpr->right = std::move(right.node);

        debug::printsf("== $6%% (%%)", binExpr->op, binExpr->base.type);

        expr.node = std::move(binExpr);

        break;
      }

      // array

      /* Comma Operator , */
      if (p == 1 && tok == Tok::OP_COMMA) {
        /* Parsing a comma list is done by recursively expanding to the right.
         * The left side of the operator is added to the current comma expression
         * which gets set up by the first occurance. While expanding to the right,
         * all left parts are getting added as well. The node of the right expression
         * is only added on the last expansion step.
         * 
         *  1, 2, 3 -> 1 | 2, 3 -> 2 | 3
         *             ^   ^
         *      (+) left   right
         *                      -> 2 | 3
         *                         ^   ^
         *                  (+) left   right (+)
         * 
         *  (+): sub expressions gets added to the comma expression vector
         * 
         * A side effect of this method is that higher level right side expressions
         * do not return a sub expression node (see the first right in the example).
         * This right expressions just serves as a structure element that will not
         * be used to be added to the result list (since it was it self parsed to
         * smaller parts before). This is relevant to prevent duplicate or empty
         * node additions!
         */

        UPtrList<ExprNode>* nodes = expr.nodes;
        bool leftMost = false;

        if (!nodes) {
          leftMost = true;
          expr.node = std::move(makeNode<CommaExpr>());
          nodes = &static_cast<CommaExpr*>(expr.node.get())->exprs;
        }

        ast::Expression left(expr.scope, start, i);
        ast::Expression right(expr.scope, i + 1, end);

        /* propagate the current list to the sub expressions */
        left.nodes = right.nodes = nodes;

        /* share the expr purpose as it applies to each element */
        left.purpose = right.purpose = expr.purpose;

        if ((s = generateExprAST(source, left)) != Status::Success)
          return ctx.throwd(s, start, i, "invalid expression");

        /* Always add the left side of the comma expansion. The check for the node is
         * still necessary since the left side could have been an empty expression and
         * thus we would add a null node. (e.g. k = (, 3))
         */
        if (left.hasNode())
          nodes->push_back(std::move(left.node));

        if ((s = generateExprAST(source, right)) != Status::Success)
          return ctx.throwd(s, i + 1, end, "invalid expression");

        /* Only add the element as the right-most expansion step. */
        if (right.hasNode())
          nodes->push_back(std::move(right.node));

        /* Optimization: comma expressions with only one real element will be substituted to the one element.  */
        if (leftMost && expr.nodeType() == NodeType::CommaExpr && nodes->size() == 1) {
          expr.node = std::move((*nodes)[0]);
          expr.nodes = nullptr;
        }

        debug::printsf("== $6, %% elem(s)", nodes->size());

        break;
      }

      /* Dot Operator . */
      if (p == 3 && tok == Tok::OP_DOT) {

        /* 2 Cases:
         *  1) parent is 
         *  2) __.system(void)[0]('copy')
         */

        ast::Expression left(expr.scope, start, i);
        ast::Expression right(expr.scope, i + 1, end);

        // left.purpose = right.purpose = 0;

        if (expr.purpose & MemberRef)
          left.purpose = MemberRef;
        
        right.purpose = MemberRef;

        generateExprAST(source, left);
        generateExprAST(source, right);

        if (right.hasNode()) {
          
        }

        expr.node = std::move(left.node);

        debug::printsf("REF: %%.%%", left.nodeType(), right.nodeType());

        break;
      }

      // == parenthesis ==

      /* Parenthesis, brackets, arrows, ... are not checked by the token type here
       * since the detection was already done. At this point, just check that we
       * have a beginning and end of a certain type (and the correct precedence).
       */

      /* Parenthesis () */
      if (p == 3 && ps[0] != -1 && pe[0] != -1) {
        /* Parsing () is done by first checking the left side which might be a function
         * name or unary operator. Depening of the result, the inner part has to be
         * interpreted differently. So, parse left first, then the inner expression.
         */

        size_t lstart = start;
        size_t lend = ps[0];
        ast::Expression left(expr.scope, lstart, lend); // left(inner)
        
        left.purpose = FuncName | OpNot; // unary in general

        if ((s = generateExprAST(source, left)) != Status::Success)
          return ctx.throwd(s, lstart, lend, "invalid expression");

        size_t pstart = ps[0] + 1;
        size_t pend = pe[0];
        ast::Expression inner(expr.scope, pstart, pend); // left(inner)

        inner.purpose = FuncParams;

        /* When we know that the inner part of the parenthesis is the parameter list
         * for a function call, we can simply pass the params vector to the expression.
         * The inner expression will add to the node list in case of a comma list.
         */
        FnCall* fnCall {nullptr};

        if (left.nodeType() == NodeType::FnCall) {
          fnCall = static_cast<FnCall*>(left.node.get());
          inner.nodes = &fnCall->params;

          /* No need to check for return type since runtime will decide (undefined on
           * void functions). This is needed since fn decls can be overwritten so we
           * do not know the exact runtime type yet.
           */
        }

        if ((s = generateExprAST(source, inner)) != Status::Success)
          return ctx.throwd(s, pstart, pend, "invalid expression");

        debug::printsf(" $4()");

        /* reset parenthesis status */
        pdepth[0] = 0;
        ps[0] = pe[0] = -1;

        /* Case: function call */
        if (fnCall) {
          /* exactly one param given -> comma expression did not append, so we have to do it here */
          if (inner.nodeCount() == 1)
            fnCall->params.push_back(std::move(inner.node));
          
          expr.node = std::move(left.node);
        } else {
         /* Just propagate the inner expression to the upper expression. This will also
          * prevent stacking expression creations in cases like '((1 + 2))' as the inner
          * most expressions gets passed through since () do not create a new Expr object.
          */
          expr.node = std::move(inner.node);
        }

        break;
      }

      /* reiterate to a higher precedence */
      if (i == (end - 1) && ++p <= mp)
        i = (start - 1);
    }

    if (p > mp)
      return ctx.throwd(Status::Fail, start, end, "no operator applies");

    return s;
  }

}

namespace ms {

  Status parse(Context& ctx, Instructions& ins) {
    if (!ctx.entrySource)
      return ctx.throwd(Status::FAIL, "requires at least one lexed source loaded");

    Status s { Status::SUCCESS };

    if ((s = ctx.makeCurrent(ctx.entrySource)) != Status::SUCCESS)
      return ctx.throwd(s, "cannot activate entry source");

    return parse(ctx, *ctx.currentSource, ins);
  }

  Status parse(Context& ctx, Source& source, Instructions& ins) {
    Status s { Status::SUCCESS };
    size_t& i = source.token;

    if (source.state != SourceState::LEXED)
      return ctx.throwd(Status::FAIL, "invalid source state: %%; must be 'LEXED'", source.state);

    // TODO: könnte auch ok sein; wäre damit die letzte
    if (!ctx.currentSource || ctx.currentSource.get() != &source)
      return ctx.throwd(Status::INTERNAL, "none or another source is set as current: %%",
        ctx.currentSource ? ctx.currentSource->name : "none");

    /* Each source represents a module if not otherwise stated through the "module" keyword.
     * The current module can be switched within one source file.
     */
    if (ctx.module)
      return ctx.throwd(Status::INTERNAL, "another module still has the context: %%", entityName(ctx.module));

    // TODO: more generic loadModule() function
    //  -> get already created module
    //  -> or createModule() + registerModule()
    ctx.module = createModule(ctx, source);
    ctx.scope = ctx.module;
    ctx.registerModule(ctx.module);

    // --

    debug::printsf("[$2Comp$r] Parsing source '%%' (%% token(s)) -> Module '%%'", source.name, source.tokenCount(), entityName(ctx.module));

    for (i = 0; i < source.tokenCount(); i++) {
      const Token& token = source.tokens[i];
      bool nl = source.line < token.line; // new line

      debug::printsf(" -- $6%% %%$r (%%:%%)", token.type, token.value, token.line, token.col);

      source.line = token.line;

      switch (token.type) {
        case Tok::UNKNOWN:
          break;

        case Tok::KW_MODULE: {
          SRef<Module> prev { ctx.module };
          std::string ident;

          if ((s = source.readIdentifier(ident)) != Status::SUCCESS)
            leave(ctx.throwd(s, "expected identifier"))

          if (!validIdentifier(ident))
            leave(ctx.throwd(Status::FAIL, "invalid identifier"))

          if ((s = ctx.enterModule(ident)) != Status::SUCCESS)
            leave(ctx.throwd(s, "could not enter module"))

          if (prev == ctx.module)
            debug::printsf("[module] staying in module '%%'", entityName(ctx.module));
          else
            debug::printsf("[module] switched module '%%' -> '%%'", entityName(prev), entityName(ctx.module));

          break;
        }

        case Tok::KW_LET: {
          std::string ident;
          Decl d;

          // TODO: readSpec(...i) -> Decl

          if ((s = source.readIdentifier(ident)) != Status::SUCCESS)
            leave(ctx.throwd(s, i + 1, "expected identifier"))

          if (!validIdentifier(ident))
            leave(ctx.throwd(Status::FAIL, i + 1, "invalid identifier"))

          if ((s = decl(ctx, ctx.scope, { ident }, d)) != Status::SUCCESS)
            leave(ctx.throwd(s, i + 1, "could not declare variable"))
          
          debug::printsf("[let] declared var '%%' in '%%'", ident, entityName(ctx.module));

          break;
        }

        case Tok::OP_ASSIGN: {
          std::string ident;
          size_t pos = i - 1;

          // TODO: parseExpression(... pos, )

          if ((s = source.readIdentifier(ident, pos)) != Status::SUCCESS)
            leave(ctx.throwd(s, pos, "expected a symbol"))

          if (!validIdentifier(ident))
            leave(ctx.throwd(Status::FAIL, pos, "invalid identifier"))

          EntityMatch em = lookup(EntityLookup(ctx.module, ident));

          if (!em.hasMatch())
            leave(ctx.throwd(Status::FAIL, pos, "unknown identifier '%%'", ident))
          
          if (em.decl.isConst)
            leave(ctx.throwd(Status::FAIL, pos, "cannot assign to const"))

          Expression expr;

          expr.scope = ctx.scope;

          if ((s = parseExpression(ctx, source, expr)) != Status::SUCCESS)
            leave(ctx.throwd(s, expr.startToken, expr.endToken, "failed to parse expression"))

          // TODO: PROPAGATE ANY!!

          if (expr.result.entity) {
            // assign decl symbol
            expr.result.entity->symbol = em.symbol;
            em.entry->second = expr.result.entity;
            em.decl.entity = expr.result.entity;
            em.decl.isDef = true;
          }

          if (expr.result.value.valueClass == ValueClass::LITERAL) {

          } else {
            SRef<Entity> ent = expr.result.entity;

            if (ent) {
              // TODO: check if
              //  a) types have to match (in most cases not since vars can change type)
              //  b) if a) && assignment is valid for type(found) = expression_type

              //em.entry->second.entity = ent;
            }
          }

          // TODO: write value
          memloc target;

          // decide if a pop is needed
          //  get info from expression if a ADD target value instruction was used and no pop is required!
          // ctx.instructions.append(Op::POP, target);

          debug::printsf("[=] assigning '%%' to %% (%%)", ident, expr.result.type, em.entry->first);

          std::cout << structureString(ctx.scope) << "\n";

          break;
        }

        default:
          break;
      }
    }

    source.state = SourceState::PARSED;

    return s;
  }

  // -- Function

  Status callFunction(Context& ctx, const SRef<Function> func, const std::vector<Value>& values) {
    Status s { Status::SUCCESS };
    Instructions& ins = ctx.instructions;

    for (size_t i = 0; i < values.size(); i++) {
      const Value& value = values[i];

      if (value.valueClass == ValueClass::LITERAL) {
        const Literal& lit = std::get<Literal>(value.content);

        if (lit.isIntermediate) {
          ins.append(Op::PUSH, value.asIntegral());
        }
      } else if (value.valueClass == ValueClass::REFERENCE) {
        const Reference& ref = std::get<Reference>(value.content);
        bool local = ref.referenced == ctx.scope;
        
        if (local)
          ins.append(Op::PUSH, OpArg::local(ref.referenced->address));
      }
    }

    ins.append(Op::CALL);

    return s;
  }

  // -- Expressions

  Status parseParenthesis(Context& ctx,
    const Tok tok, const Tok open, const Tok close,
    long type, long* ps, long* pe, long* pdepth, size_t i) {
    /* parsing left to right

    if (content == open) {
        if (ps[type] == -1)
            ps[type] = i;
            
        pdepth[type]++;
    } else if (content == close) {
        pdepth[type]--;

        if (!pdepth[type] && pe[type] == -1)
            pe[type] = i;

        if (pdepth[type] < 0)
            return ctx.throwd(MS_ERROR_INVALID_SYNTAX, i, "scope operator '%%' has no partner", close);
    }
    */

    //debug::printsf("%%: tok=%% ps=%% pe=%% pdepth=%%", i, tok, ps[type], pe[type], pdepth[type]);

    if (tok == open) {
      if (ps[type] == -1)
        ps[type] = i;

      pdepth[type]++;
    } else if (tok == close) {
      pdepth[type]--;

      if (!pdepth[type] && pe[type] == -1)
        pe[type] = i;

      if (pdepth[type] < 0)
        return ctx.throwd(Status::Fail, i, "scope operator '%%' has no partner", close);
    }

    /*
    // parsing right to left <- currently used
    if (tok == open) {
        pdepth[type]--;

        if (!pdepth[type] && ps[type] == -1)
            ps[type] = i;

        if (pdepth[type] < 0)
            return ctx.throwd(Status::FAIL, i, "scope operator '%%' has no partner", open);
    } else if (tok == close) {
        if (pe[type] == -1)
            pe[type] = i;

        pdepth[type]++;
    }
    */

    //debug::printsf(" '%%' %%%% - level %%", content, open, close, pdepth[type]);

    return Status::SUCCESS;
  }

  Status handleFnCall(Context& ctx, SRef<Function> fn, const std::vector<TypedValue> params) {
    Status s { Status::SUCCESS };

    return s;
  }

  // Just create instructions, do not 
  Status parseObject(Context& ctx, Source& source, Expression& expr, size_t from, size_t to, SRef<Object>& result) {
    Status s { Status::SUCCESS };
    size_t l { to - from };

    if (l < 2)
      return ctx.throwd(Status::FAIL, from, to, "invalid object syntax");

    if (l == 2)
      return parseExpression(ctx, source, expr, from, to);

    SRef<Object> obj = std::make_shared<Object>();
    char phase = 0; // 0 - identifier; 1 - colon; 2 - expression

    // current layer
    Namespace::DeclMap layer;
    uint level = 0;

    ctx.instructions.append(Op::NEW, 1 /* TYPE 1 = OBJECT */);

    for (size_t i = from; i < to; i++) {
      const Token& token = source.tokens[i];
      const Tok tok = token.type;

      if (phase == 0) {
        if (tok != Tok::IDENTIFIER)
          return ctx.throwd(Status::FAIL, i, "expected identifier");

        layer.emplace(std::make_pair(token.value, Decl {}));
      }
    }

    return s;
  }

  Status parseTerminator(Context& ctx, Source& src, Expression& expr, size_t pos) {
    Status s { Status::SUCCESS };

    if (pos < 0 || pos >= src.tokenCount())
      return ctx.throwd(Status::INTERNAL, "expected terminator");

    const Token& token = src.tokens[pos];

    switch (token.type) {
      
      /* References */

      case Tok::IDENTIFIER: {
        
        if (expr.pushHash) {
          size_t hash { SymbolHash()({ token.value }) };

          ctx.instructions.append(Op::LEA, hash);

          expr.result.type = types::Any;

          break;
        }

        SRef<Namespace> scope = expr.scope ? expr.scope : ctx.scope;
        EntityMatch em = lookup(EntityLookup(scope, token.value)); // Todo: search option -> only EntityType::VAR???
        Reference ref;

        if (em.hasDef()) {
          SRef<Entity> ent = em.entity;

          if (ent->type == EntityType::PROTO) {
            SRef<Proto> proto = castEntity<Proto>(ent);

            expr.result.type = proto;
          } else if (ent->type == EntityType::OBJECT) {
            ctx.instructions.append(Op::PUSH, ent->address);
          }

          // DEPRECATED
          /* References in expressions should always refer to previously declared variables.
           * Only declared and not defined variables hold the default value for the value type.
           */
          //ref.refType = varType();
          ref.constant = em.decl.isConst;
          ref.referenced = ent;
          expr.result.entity = ent;
        } else {
          /* Const Objects and Protos cannot create properties in place, thus requiring
           * the requested name to be a member of the scope. Simple objects will automatically
           * create the member if not existing.
           */

          if (scope) {
            /* When referencing members of objects, their adresses must be pushed to the stack.
             * Since this is not a proto and thus one cannot know if the member really exists
             * on compile time, only the hash value of the symbol is used. On runtime this hash
             * will be used to query the address of the actual object's member (which may be undefined).
             */
            if (scope->type == EntityType::OBJECT) {
              size_t hash;

              if (em.hasMatch())
                hash = SymbolHash{}(em.symbol);
              else
                hash = SymbolHash()({ token.value });

              ctx.instructions.append(Op::LEA, hash);

              break;
            }
          }

          return ctx.throwd(Status::FAIL, "<%%> has no member '%%'", entityName(scope), token.value);
        }
        
        //expr.result.type = types::Reference;
        expr.result.type = ref.refType;
        expr.result.value = Value { ValueClass::REFERENCE, ref };

        debug::printsf(" FOUND %% (%%) in %% referencing %% at %%",
          em.symbol, em.entity->type, entityName(scope), ref.refType, "address");

        break;
      }

      /* Literals are registered in {??} and referenced by an ID. */

      case Tok::L_INTEGRAL:
      case Tok::L_DECIMAL:
      case Tok::L_STRING: {
        Literal lit;

        if ((s = ctx.createLiteral(lit, token)) != Status::SUCCESS)
          return ctx.throwd(s, "failed to create literal");

        expr.result.type = lit.type;
        expr.result.value = Value { ValueClass::LITERAL, lit };

        debug::printsf(" LITERAL: %% -> %%", token.value, lit.id);

        break;
      }

      default:
        return ctx.throwd(Status::FAIL, "invalid token type <%%>; expected literal or identifier", token.type);
    }

    expr.isTerminator = true;
    expr.result.reduced = true;

    return s;
  }

  Status handleBinaryOp(Context& ctx, const TypedValue left, const TypedValue right, const Operation op, Type& result) {
    const Type& lt = left.type, rt = right.type;
    const Value& lv = left.value, rv = right.value;

    if (!lt.def)
      return ctx.throwd(Status::INTERNAL, "type <%%> has no definition", lt.name());

    if (!rt.def)
      return ctx.throwd(Status::INTERNAL, "type <%%> has no definition", rt.name());

    // for each rank ... do ...

    // 1) is type a simple type
    //  1.1) is left == right
    //   x := left + right
    // 2) can (left) right?
    //   x := left + (left) right
    // 3) is there an op function in left with params <right>?
    //   x := left.opfn<operation>(right)
    // 4) is there a generel op-function defined for those specific types?
    //   x := global_op_fn<operation>(left, right)

    debug::printsf("callOpFunc %% %% %%", left, (int) op.type, right);

    if (lt == TypeClass::SIMPLE) {
      if (rt == TypeClass::SIMPLE) {
        // TODO -> result type
        // lookup combiner:
        //  x := ((SimpleType*) type.typeDef)->ordinal()
        //  y := ((SimpleType*) param.typeDef)->ordinal()
        //  combinerfn := combfns[y * N_SIMPLE_TYPES + x]
        //  combinerfn.apply(token_left, token_right, operation, context, source, instructions)

        result = types::Int;

        if (lv.valueClass == ValueClass::LITERAL && rv.valueClass == ValueClass::LITERAL) {

          if (op.type == OpClass::ADD)
            ctx.instructions.append(Op::ADD, createMemloc(lv), createMemloc(rv));

          // TODO: more
          //  getOpCodeForArithmeticOp(op)
        }

        // ctx.instructions.append(Op::ADD);
      } else if (rt == TypeClass::PROTO) {
        SRef<Function> castfn = rt.def->castfn(lt);

        /* Only the direct castfn is needed. Other (indirect) conversions like int + a.b (with a.b as float)
         * are not supported inexplicitely. An explicit cast: int + (<type the int can work with>) a.b is needed then!
         */

        if (castfn) {
          return handleFnCall(ctx, castfn, { right });
        }

        // TODO: look for global opfn
        
        return ctx.throwd(Status::FAIL, "no suitable conversion of <%%> to operate with <%%> via %%", rt, lt, op);
      } else {
        return ctx.throwd(Status::FAIL, "simple types cannot operate with <%%>", rt.typeClass());
      }
    } else if (lt == TypeClass::OBJECT) {
      if (rt == TypeClass::OBJECT) {

      } else if (rt == TypeClass::PROTO) {

      }

      // result = lt

      return ctx.throwd(Status::FAIL, "can only add other objects or protos to an object; provided <%%>", rt);
    } else if (lt == TypeClass::ARRAY) {
      // check type
      // SRef<Array> array = lt.def;
      //  if (array->valueType != rt) --> ERROR

      SRef<Array> array = derive<TypeDef, Array>(lt.def);

      if (array->arrayType != rt)
        return ctx.throwd(Status::FAIL, "cannot add non matching array type: Array<%%> + <%%>", array->arrayType, rt);

      // TODO: ADD -> array_add
      ctx.instructions.append(Op::ADD, createMemloc(lv), createMemloc(rv));

      result = lt;
    } else if (lt == TypeClass::TUPEL) {
      // accept everything ?
    } else if (lt == TypeClass::PROTO) {
      SRef<Function> opfn = lt.def->opfn(op, rt);

      if (opfn) {
        return handleFnCall(ctx, opfn, { right });
      }

      // TODO: look for global opfn

      return ctx.throwd(Status::FAIL, "no operator function defined for type <%%>", lt.name());
    } else if (lt == TypeClass::ANY) {
      // any: check on runtime...

      ctx.instructions.append(Op::ADD);
    } else {
      return ctx.throwd(Status::FAIL, "cannot perform <%%> on <%%> and <%%>", op, lt, rt);
    }

    return Status::SUCCESS;
  }

  Status handleOperation(Context& ctx, Source& source, const RefList<Expression>& expressions, const Operation op, Type& result) {
    Status s { Status::SUCCESS };

    if (op.rank != expressions.size())
      return ctx.throwd(Status::INTERNAL, "operation <%%> expects %% expression(s); provided: %%", op.type, op.rank, expressions.size());
    
    if (std::any_of(expressions.begin(), expressions.end(), [](const auto& e) { return !e.get().result.reduced; }))
      return ctx.throwd(Status::INTERNAL, "cannot execute operation since not each operation side is fully reduced");

    /* Possible combinations: [int, dec]x[int, dec] |C| = 4
     */

    switch ((OpRank) op.rank) {
      case OpRank::UNARY: {
        break;
      }

      case OpRank::BINARY: {
        TypedValue lt { expressions[0].result.type, expressions[0].result.value };
        TypedValue rt { expressions[1].result.type, expressions[1].result.value };

        s = handleBinaryOp(ctx, lt, rt, op, result);

        break;
      }

      case OpRank::TERTIARY: {
        break;
      }
      
      default:
        return ctx.throwd(Status::INTERNAL, "operations of rank %% are currently not supported", op.rank);
    }

    return s;
  }

  Status parseExpression(Context& ctx, Source& source, Expression& expr, size_t from, size_t to) {
    Instructions& is = ctx.instructions;
    Status s { Status::SUCCESS };

    if (to < from || (to - from) == 0)
      return ctx.throwd(Status::INTERNAL, "invalid expression range [%%, %%]", from, to);

    if (to > source.tokenCount())
      return ctx.throwd(Status::INTERNAL, "expression range [%%, %%] out of source range [%%, %%]", from, to, 0, source.tokenCount());

    size_t d = to - from;

    debug::printsf("!(%%, %%):%% - $4%%$r", from, to, d, source.concat(from, to));
    expr.debugName = source.concat(from, to);
    expr.startToken = from;
    expr.endToken = to;

    // -- single token
    if (d == 1) {
      return parseTerminator(ctx, source, expr, from);
    }

    /* only possible (and allowed) combinations:
     *  (1) not <terminator>
     *  (2) array defintions: []
     */
    if (d == 2) {
      const Token& l = source.tokens[from];
      const Token& r = source.tokens[to - 1];

      // advance token index
      source.token += 2;

      // Create array
      if (l.type == Tok::OP_LEFT_BRACKET && r.type == Tok::OP_RIGHT_BRACKET) {
        is.append(Op::CARRAY);

        expr.result.type = types::makeArray(types::Any);

        debug::printsf(" CREATE Dynamic Array -> %%", expr.result.type);
      } else if (l.type == Tok::OP_LEFT_CURLY && r.type == Tok::OP_RIGHT_CURLY) {
        SRef<Object> obj = std::make_shared<Object>();

        expr.result.type = Type {obj};
        expr.result.entity = obj;

        debug::printsf(" CREATE Empty Object -> %%", expr.result.type);
      } else
        return ctx.throwd(Status::FAIL, from, to, "invalid expression; expected special type of length two");

      return Status::SUCCESS;
    }

    // =-=-=-=

    int minPrecedence = 0;
    int p {minPrecedence}, mp = 15; // precendence; max precedence
    bool ignore {false}; // used to ignore tokens on strings

    // Each type of parenthesis has a start, an end and a depth.
    //  [0]: normal parenthesis ()
    //  [1]: bracket []
    //  [2]: curly bracket {}
    //  [3]: angular bracket <>

    long ps[4] = {-1, -1, -1, -1}; // start index of first opening occurence
    long pe[4] = {-1, -1, -1, -1}; // end index of last closing occurence
    long pdepth[4] = {0, 0, 0, 0}; // amount of closing tokens needed to build pairs
    
    // States of the sub-algorithms
    Status sleft { Status::SUCCESS }, sright { Status::SUCCESS };

    size_t i = to - 1;

    for (; i >= from; i--) {
      const Token& token = source.tokens[i];
      const Tok tok = token.type;

      // -- parenthesis control

      /* NOTE: brackets cannot be parsed with a simple precedence.
        * When there are no arithmetic operators on the level of the bracket, the bracket
        * gets parsed first. But if otherwise, the arithmetic operator has the
        * higher precedence and must be the main anchor.
        * e.g.:
        *  a) a[0 + 1] -> first parse [], then 0 + 1
        *  b) a[0] + 1 -> first parse +, then []
        * Therefore, use the exclusing mechanic below instead of
        * simply checking for the precendence of brackets.
        */

      //if (p >= 7) {
          MS_ASSERT_STATE(s, parseParenthesis(ctx, tok, Tok::OP_LEFT_PARENTHESIS, Tok::OP_RIGHT_PARENTHESIS, 0, ps, pe, pdepth, i))
          MS_ASSERT_STATE(s, parseParenthesis(ctx, tok, Tok::OP_LEFT_BRACKET, Tok::OP_RIGHT_BRACKET, 1, ps, pe, pdepth, i))
          MS_ASSERT_STATE(s, parseParenthesis(ctx, tok, Tok::OP_LEFT_CURLY, Tok::OP_RIGHT_CURLY, 2, ps, pe, pdepth, i))
      //}
      //MS_ASSERT_STATE(s, parseParenthesis(ctx, content.content, "<", ">", 3, ps, pe, pdepth, i))

      // Whenever a parenthesis is present in the current level,
      // the scoped content gets parsed later. All precedences
      // of normal operators are parsed on this level before.
      if ((pdepth[0] | pdepth[1] | pdepth[2] | pdepth[3]) > 0)
          continue;

      // == operators ==

      // +
      if (p == 5 && tok == Tok::OP_ADD) {
        Expression& left = expr.left();
        Expression& right = expr.right();

        sleft = parseExpression(ctx, source, left, from, i);
        sright = parseExpression(ctx, source, right, i + 1, to);

        /*

        const Type resultType = combinedType(left.result.type, right.result.type);

        if (resultType == types::Invalid) {
          return ctx.throwd(Status::FAIL, "invalid type combination: operator +(%%, %%)", left.result.type, right.result.type);
        }

        debug::printsf("ADD %% (%%) + %% (%%)",
          left.result.value,
          left.result.type,
          right.result.value,
          right.result.type
        );

        expr.result.type = resultType;

        is.append(Op::ADD, memloc(left.result.value.asIntegral()), memloc(right.result.value.asIntegral()));
        */

        // Parent expression is root expression -> left could be an assignment target
        if (expr.isRoot && left.isTerminator) {
          // left is a reference <-> assignable
          if (left.result.value.valueClass == ValueClass::REFERENCE) {

          }
        }

        Type result;

        if ((s = handleOperation(ctx, source, reflist(left, right), Operations::ADD, result)) != Status::SUCCESS)
          return ctx.throwd(s, "op failed");

        // combine types

        expr.result.type = result;

        break;
      }

      if (p == 7 && tok == Tok::OP_DOT) {
        Expression& left = expr.left();
        Expression& right = expr.right();

        sleft = parseExpression(ctx, source, left, from, i);

        /* When the left tree is an object or runtime reference, propagate
         * this behavior to the next node.
         */
        if (isType(left.result.entity, EntityType::OBJECT) || left.pushHash) {
          right.pushHash = true;
          expr.pushHash = true;
        }

        // TODO: propagate ANY ...

        // Only for proto?
        //if (left.result.entity && left.result.entity->isNamespace())
        //  right.scope = toNamespace(left.result.entity);

        sright = parseExpression(ctx, source, right, i + 1, to);

        expr.result.type = right.result.type;
        expr.result.reduced = true;

        break;
      }

      // object {...}
      if (p == 1 && ps[2] != -1 && pe[2] != -1) {
        debug::printsf("{$4%%$r}", source.concat(ps[2] + 1, pe[2]));

        Expression objExpr;
        SRef<Object> object;

        if ((s = parseObject(ctx, source, objExpr, ps[2] + 1, pe[2], object)) != Status::SUCCESS)
          return ctx.throwd(s, ps[2] + 1, ps[2], "failed to parse expression");

        break;
      }

      // When reaching the end of given input range and not all
      // precedences are resolved, start at the beginning on
      // a higher precedence.
      if (i == from && ++p < mp)
        i = to;
    }

    return s;
  }

  Status parseExpression(Context& ctx, Source& src, Expression& expr) {
    Status s { Status::SUCCESS };
    size_t end = rightBound(src, src.token + 1);

    expr.startToken = src.token;
    expr.endToken = end;

    debug::printsf("BEGIN parseExpression");
    s = parseExpression(ctx, src, expr, src.token + 1, end);

    // Advance current token index
    src.token = end - 1; // -1 since parse-loop will i++ on end
    debug::printsf("END   parseExpression");

    return s;
  }

  size_t rightBound(Source& src, size_t pos) {
    int cohesion = 2;
    int r = 0;

    for (size_t i = pos; i < src.tokenCount(); i++) {
      const Token& token = src.tokens[i];
      const Tok tok = token.type;
      const TokenClass tc = classifyToken(tok);

      //debug::printsf(" - %% '%%' = %% .. %% --> %%", tok, token.value, (int) tc, r, i);

      if (tc == TokenClass::KEYWORD)
        return i;
      else if (tc == TokenClass::OPERATOR) {
        // a type of parenthesis MUST be followed by an operator // TODO: no -> 2 + (2)
        /* NO SPECIAL TREATMENT
        if (isParenthesis(tok))
          r = 0;
        else
        */
          r = 0;
      } else
        r++;

      if (r == cohesion)
        return i;
    }

    return src.tokens.size();
  }

  size_t leftBound(Source& src, size_t pos) {
    int cohesion = 2;
    int r = 0;

    if (pos == 0)
      return 0;

    for (size_t i = pos - 1; i >= 0; i--) {
      const Token& token = src.tokens[i];
      const Tok tok = token.type;
      const TokenClass tc = classifyToken(tok);

      // debug::printsf(" - %% '%%' = %% .. %% --> %%", tok, token.value, (int) tc, r, i);

      if (tc == TokenClass::KEYWORD)
        return i + 1;
      else if (tc == TokenClass::OPERATOR) {
        // a type of parenthesis MUST be followed by an operator // TODO: no -> 2 + (2)
        /* NO SPECIAL TREATMENT
        if (isParenthesis(tok))
          r = 0;
        else
        */
          r = 0;
      } else
        r++;

      if (r == cohesion)
        return i + 1;
    }

    return 0;
  }

  // --

  Status decl(Context& ctx, SRef<Namespace> scope, Symbol symbol, Decl decl) {
    if (!scope)
      return ctx.throwd(Status::INTERNAL, "missing scope");

    if (scope->contains(symbol))
      return ctx.throwd(Status::FAIL_DUPLICATE, "symbol '%%' was already declared in scope <%%>", symbol, scope->symbol);

    scope->entities.emplace(std::make_pair(symbol, decl));

    return Status::SUCCESS;
  }

  Status def(Context& ctx, SRef<Namespace> scope, Symbol symbol, SRef<Entity> member, bool requiresDecl) {
    if (!scope)
      return ctx.throwd(Status::INTERNAL, "missing scope");

    if (!member)
      return ctx.throwd(Status::FAIL, "cannot define null entity");

    auto it = scope->entities.find(symbol);

    if (it == scope->entities.end()) {
      if (requiresDecl)
        return ctx.throwd(Status::FAIL, "missing declaration for <%%> in <%%>", entityName(scope), symbol);

      scope->entities.emplace(std::make_pair(member->symbol, member));
    } else
      it->second.entity = member;

    member->parent = scope;

    return Status::SUCCESS;
  }

  // -- Util

  memloc createMemloc(const Value& value) {
    if (value.valueClass == ValueClass::UNKNOWN)
      return memloc(memloc::UNSET, 0); // INVALID

    if (value.valueClass == ValueClass::REFERENCE) {
      const Reference& ref = std::get<Reference>(value.content);

      return memloc::local(ref.asAddress());
    }

    const Literal& lit = std::get<Literal>(value.content); // TODO: check if LITERAL

    if (lit.isIntermediate) {
      if (lit.type == types::Int) {
        return memloc(std::get<msx::Integral>(lit.value));
      } else if (lit.type == types::Decimal) {
        return memloc(std::get<msx::Decimal>(lit.value));
      }
    } else {
      return memloc::local(lit.id); // TODO: Literal to real address (offset)
    }

    return memloc {};
  }

}

#undef require
#undef leave