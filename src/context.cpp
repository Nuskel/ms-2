#include "context.hpp"

namespace ms {

  Context::Context() {
    instructions.applyLabelOnNext("__main__");
  }

  //

  Status Context::makeCurrent(SRef<Source> source) {
    if (!source)
      return Status::FAIL;

    currentSource = source;

    return Status::SUCCESS;
  }

  Status Context::decl(Symbol symbol) {
    if (!scope)
      return throwd(Status::INTERNAL, "no active scope");

    SRef<Entity> var = std::make_shared<Entity>(symbol, EntityType::VAR);
    Status s { scope->add(var) };

    if (s == Status::FAIL_DUPLICATE)
      throwd(s, "symbol '%%' is already declared in this scope", symbol);

    var->parent = scope;

    return s;
  }

  Status Context::registerModule(SRef<Module> module) {
    return Status::SUCCESS;
  }

  Status Context::bindModule() {
    return Status::SUCCESS;
  }

  Status Context::enterModule(const std::string_view name) {
    return Status::SUCCESS;
  }

  Status Context::createLiteral(Literal& lit, const Token& token) {
    Status s { Status::SUCCESS };

    switch (token.type) {
      
      /* Those literals are stored as intermediate values in the instruction set. */

      case Tok::L_INTEGRAL: {
        lit.type = types::IntType;
        lit.isIntermediate = true;
        lit.value = token.integral;

        break;
      }

      case Tok::L_DECIMAL: {}

      /* As strings usually require a lot of bytes, they will not be stored
       * as intermediate values in the instructions. Rather an ID to the static
       * literal datapool is returned which will be resolved by the linker.
       */
      case Tok::L_STRING: {}

      default:
        return throwd(Status::FAIL, "expected literal token; got %%", token.type);
    }

    return s;
  }

  void Context::logTrace() {

  }

  //

  std::string findPath(Context& ctx, const std::string& filename) {
    if (ms::fexists(filename)) {
      return filename;
    }

    for (const std::string& dir : ctx.sourceLocations) {
      if (ms::fexists(dir, filename)) {
        return dir + "/" + filename;
      }
    }

    return "";
  }

  Status registerSource(ms::Context& ctx, const std::string& file) {
    SRef<Source> source = std::make_shared<Source>();
    Status s { Status::SUCCESS };

    if (ctx.sources.find(file) != ctx.sources.end())
      return debug::errorsf(Status::FAIL, "!", "a file named '%%' was already loaded", file);

    // find correct filepath (local or any in given source locations)
    std::string path = findPath(ctx, file);

    if (path == "") {
      return debug::errorsf(Status::FAIL, "!", "could not find file '%%' in any source location", file);
    }

    if ((s = readFile(*source, path)) != Status::SUCCESS)
      return debug::errorsf(s, "!", "failed to read file");

    if ((s = lex(*source)) != Status::SUCCESS)
      return debug::errorsf(s, "!", "failed to lex file");

    // set source meta
    source->name = ms::filename(file); // w/o extension
    source->file = path;

    // the first source is the entry source
    if (!ctx.entrySource) {
      ctx.entrySource = source;

      debug::printsf("[$2Entry Source$r] will be set as '%%'", path);
    }
    
    ctx.sources.emplace(file, std::move(source));

    return s;
  }

}