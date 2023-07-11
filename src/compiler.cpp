#include "compiler.hpp"
#include "lang.hpp"
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

      debug::printsf(" -- %% %% (%%:%%)", token.type, token.value, token.line, token.col);

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

          if ((s = parseExpression(ctx, source, expr)) != Status::SUCCESS)
            leave(ctx.throwd(s, expr.startToken, expr.endToken, "failed to parse expression"))

          std::cout << structureString(ctx.scope) << "\n";

          if (expr.result.entity) {
            debug::printsf("ENT = %%, %%", expr.result.entity->symbol, expr.result.entity);
            debug::printsf(" (1) %% = %%", em.entry->first, em.entry->second.entity);

            em.entry->second.entity = expr.result.entity;

            debug::printsf(" (2) %% = %%", em.entry->first, em.entry->second.entity);
          }

          for (const auto& e : ctx.scope->entities) {
            debug::printsf(" - %%: %%", e.first, e.second.entity);
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

  Status handleFnCall(Context& ctx, SRef<Function> fn, const std::vector<TypedValue> params) {
    Status s { Status::SUCCESS };

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
        SRef<Namespace> scope = expr.scope ? expr.scope : ctx.scope;
        EntityMatch em = lookup(EntityLookup(scope, token.value)); // Todo: search option -> only EntityType::VAR???
        Reference ref;

        if (em.hasDef()) {
          SRef<Entity> ent = em.entity;

          if (ent->type == EntityType::PROTO) {
            SRef<Proto> proto = castEntity<Proto>(ent);

            expr.result.type = proto;
          } else {

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
    } else {
      return ctx.throwd(Status::FAIL, "cannot perform <%%> on <%%> and <%%>", op, lt, rt);
    }

    return Status::SUCCESS;
  }

  Status handleOperation(Context& ctx, Source& source, const RefList<Expression>& expressions, const Operation op, Type& result) {
    Status s { Status::SUCCESS };

    if (op.rank != expressions.size())
      return ctx.throwd(Status::INTERNAL, "operation <%%> expects %% expression(s); provided: %%", op.type, op.rank, expressions.size());

    if (std::any_of(expressions.begin(), expressions.end(), [](const auto& e) { return !e.get().isTerminator; }))
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

    debug::printsf("!(%%, %%):%% - %%", from, to, d, source.concat(from, to));
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
      }

      if (p == 7 && tok == Tok::OP_DOT) {
        Expression& left = expr.left();
        Expression& right = expr.right();

        sleft = parseExpression(ctx, source, left, from, i);

        debug::printsf("left: %% in scope %%", entityName(left.result.entity), entityName(left.scope));

        if (left.result.entity && left.result.entity->isNamespace())
          right.scope = toNamespace(left.result.entity);

        debug::printsf("right: %% in scope %%", entityName(right.result.entity), entityName(right.scope));
        
        sright = parseExpression(ctx, source, right, i + 1, to);

        debug::printsf("DOT %% (%%).%% (%%)", entityName(left.result.entity), left.result.type,
          entityName(right.result.entity), right.result.type);

        expr.result.type = right.result.type;
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

      debug::printsf(" - %% '%%' = %% .. %% --> %%", tok, token.value, (int) tc, r, i);

      if (tc == TokenClass::KEYWORD)
        return i;
      else if (tc == TokenClass::OPERATOR) {
        // a type of parenthesis MUST be followed by an operator
        if (isParenthesis(tok))
          r = 1;
        else
          r = 0;
      } else
        r++;

      if (r == cohesion)
        return i;
    }

    return src.tokens.size();
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