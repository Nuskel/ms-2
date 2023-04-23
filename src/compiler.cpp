#include "compiler.hpp"
#include "sformats.hpp"

#define require(amount, token, msg) \
  if ((i + amount) >= source.tokenCount()) \
    return debug::errorsf(Status::FAIL, token, msg);

#define leave(what) { what; break; }
#define consume(n) i += n
#define skip(n) i += n

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

    ctx.module = createModule(source);
    ctx.scope = ctx.module;
    ctx.registerModule(ctx.module);

    // --

    debug::printsf("[$2Comp$r] Parsing source '%%' (%% token(s)) -> Module '%%'", source.name, source.tokenCount(), entityName(ctx.module));

    for (i = 0; i < source.tokenCount(); i++) {
      const Token& token = source.tokens[i];
      bool nl = source.line < token.line; // new line

      debug::printsf(" -- %% %%", token.type, token.value);

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

          if ((s = source.readIdentifier(ident)) != Status::SUCCESS)
            leave(ctx.throwd(s, "expected identifier"))

          if (!validIdentifier(ident))
            leave(ctx.throwd(Status::FAIL, "invalid identifier"))

          if ((s = ctx.decl({ ident })) != Status::SUCCESS)
            leave(ctx.throwd(s, "could not declare variable"))
          
          debug::printsf("[let] declared var '%%' in '%%'", ident, entityName(ctx.module));

          break;
        }

        case Tok::OP_ASSIGN: {
          std::string ident;

          if ((s = source.readIdentifier(ident, i - 1)) != Status::SUCCESS)
            leave(ctx.throwd(s, "expected target symbol"))

          if (!validIdentifier(ident))
            leave(ctx.throwd(Status::FAIL, "invalid identifier"))

          EntityMatch em = lookup(EntityLookup(ctx.module, ident));

          if (em.notFound())
            leave(ctx.throwd(Status::FAIL, "unknown identifier '%%'", ident))

          Expression expr;

          if ((s = parseExpression(ctx, source, expr)) != Status::SUCCESS)
            leave(ctx.throwd(s, "failed to parse expression"))

          debug::printsf("[=] assigning '%%' to %% (%%)", ident, expr.result.type, "-");

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
          ins.append(Op::PUSH, memloc(value.asIntegral()));
        }
      } else if (value.valueClass == ValueClass::REFERENCE) {
        const Reference& ref = std::get<Reference>(value.content);
        bool local = ref.referenced == ctx.scope;
        
        if (local)
          ins.append(Op::LEA, memloc::local(ref.referenced->address));
      }
    }

    ins.append(Op::CALL);

    return s;
  }

  // -- Expressions

  Status parseTerminator(Context& ctx, Source& src, Expression& expr, size_t pos) {
    Status s { Status::SUCCESS };

    if (pos < 0 || pos >= src.tokenCount())
      return ctx.throwd(Status::INTERNAL, "expected terminator");

    const Token& token = src.tokens[pos];

    switch (token.type) {
      
      /* References */

      case Tok::IDENTIFIER: {
        EntityMatch em = lookup(EntityLookup(ctx.scope, token.value)); // Todo: search option -> only EntityType::VAR???
        Reference ref;

        ref.constant = false;
        ref.referenced = em.found;

        if (em.found) {
          /* References in expressions should always refer to previously declared variables.
           * Only declared and not defined variables hold the default value for the value type.
           */
          ref.refType = varType(em.found);
        } else
          return ctx.throwd(Status::FAIL, "unknown identifier '%%'", token.value);
        
        expr.result.type = types::Reference;
        expr.result.value = Value { ValueClass::REFERENCE, ref };

        break;
      }

      /* Literals are registered in {??} and referenced by an ID. */

      case Tok::L_INTEGRAL: {
        Literal lit;

        if ((s = ctx.createLiteral(lit, token)) != Status::SUCCESS)
          return ctx.throwd(s, "failed to create literal");

        expr.result.type = lit.type;
        expr.result.value = Value { ValueClass::LITERAL, lit };

        break;
      }

      case Tok::L_STRING: {
        break;
      }

      default:
        return ctx.throwd(Status::FAIL, "invalid token type <%%>; expected literal or identifier", token.type);
    }

    expr.isTerminator = true;

    return s;
  }

  Status callOpFunction(Context& ctx, const Type type, const std::vector<Type> params, const Operation op) {
    if (!type.typeDef)
      return ctx.throwd(Status::INTERNAL, "type <%%> has no definition", type.name);

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

    return Status::SUCCESS;
  }

  Status handleOperation(Context& ctx, Source& source, const std::vector<Expression&>& expressions, const Operation op) {
    Status s { Status::SUCCESS };

    if (op.rank != expressions.size())
      return ctx.throwd(Status::INTERNAL, "operation <%%> expects %% expressions; provided: %%", op.type, op.rank, expressions.size());

    if (std::any_of(expressions.begin(), expressions.end(), [](Expression& e) { return !e.isTerminator; }))
      return ctx.throwd(Status::INTERNAL, "cannot execute operation since not each operation is fully reduced");

    /* Possible combinations: [int, dec]x[int, dec] |C| = 4
     */

    switch ((OpRank) op.rank) {
      case OpRank::UNARY: {
        break;
      }

      case OpRank::BINARY: {
        Type lt { expressions[0].result.type };
        Type rt { expressions[1].result.type };
        std::vector<Type> params {{ rt }};

        s = callOpFunction(ctx, lt, params, op);

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

    debug::printsf("!(%%, %%):%% - %%", from, to, d, source.concat(from, to));
    expr.debugName = source.concat(from, to);

    // -- single token
    if (d == 1) {
      return parseTerminator(ctx, source, expr, from);
    }

    /* only possible (and allowed) combinations:
     *  (1) not <terminator>
     */
    if (d == 2) {

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

    int ps[4] = {-1, -1, -1, -1}; // start index of first opening occurence
    int pe[4] = {-1, -1, -1, -1}; // end index of last closing occurence
    int pdepth[4] = {0, 0, 0, 0}; // amount of closing tokens needed to build pairs
    
    // States of the sub-algorithms
    Status sleft { Status::SUCCESS }, sright { Status::SUCCESS };

    size_t i = to - 1;

    for (; i >= from; i--) {
      const Token& token = source.tokens[i];
      const Tok tok = token.type;

      // +
      if (p == 5 && tok == Tok::OP_ADD) {
        Expression& left = expr.left();
        Expression& right = expr.right();

        sleft = parseExpression(ctx, source, left, from, i);
        sright = parseExpression(ctx, source, right, i + 1, to);

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
      }

      // When reaching the end of given input range and not all
      // precedences are resolved, start at the beginning on
      // a higher precedence.
      if (i == from && ++p < mp)
        i = to;
    }

    // '+'
    Expression left, right;
    
    //is.append(Op::ADD, memloc::local(left.result.value.asAddress()), memloc(left.result.value.asIntegral()));
    

    return s;
  }

  Status parseExpression(Context& ctx, Source& src, Expression& expr) {
    Status s { Status::SUCCESS };
    size_t end = rightBound(src, src.token + 1);

    debug::printsf("BEGIN parseExpression");
    s = parseExpression(ctx, src, expr, src.token + 1, end);
    debug::printsf("END   parseExpression");

    return s;
  }

  size_t rightBound(Source& src, size_t pos) {
    for (size_t i = pos; i < src.tokenCount(); i++) {

    }

    return pos + 3;
  }

}

#undef require
#undef leave