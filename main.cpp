#include <string>
#include <iostream>
#include <sstream>
#include <typeinfo>
#include <vector>
#include <functional>

#include "ms.hpp"
#include "console.hpp"

namespace ms {

  void printBanner() {
    debug::applyColor(std::cout, debug::ChatColor::FG_CYAN);

    std::cout << '\n';
    std::cout << "______  ___      _______________    ________            _____        _____  \n";
    std::cout << "___   |/  /___  ____  /_  /___(_)   __  ___/_______________(_)_________  /_ \n";
    std::cout << "__  /|_/ /_  / / /_  /_  __/_  /    _____ \\_  ___/_  ___/_  /___  __ \\  __/ \n";
    std::cout << "_  /  / / / /_/ /_  / / /_ _  /     ____/ // /__ _  /   _  / __  /_/ / /_   \n";
    std::cout << "/_/  /_/  \\__,_/ /_/  \\__/ /_/      /____/ \\___/ /_/    /_/  _  .___/\\__/     V 3.1\n";
    std::cout << "                                                             /_/            \n";
    std::cout << '\n';

    debug::resetStream(std::cout);
  }

}

void test();

void abc() {
  using namespace ms;
}

template <typename T, typename... Rest>
inline void hashCombine(std::size_t &seed, T const &v, Rest &&... rest) {
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    (int[]){0, (hashCombine(seed, std::forward<Rest>(rest)), 0)...};
}

struct OpFn {

  size_t ltype;
  size_t rtype;
  size_t op;

  bool operator ==(const OpFn& other) const {
    return ltype == other.ltype && rtype == other.rtype && op == other.op;
  }

};

struct HASHER {
  auto operator ()(const OpFn& fn) const -> size_t {
    size_t hash = 0;
    hashCombine(hash, fn.ltype, fn.rtype, fn.op);
    return hash;
  }
};

int main(int argc, char* argv[]) {
  using namespace ms;
  using namespace ast;

  // test();

  if (false) {
    size_t i1 = 123;
    size_t i2 = 932;
    size_t i3 = 555;

    std::unordered_map<OpFn, char, HASHER> map;

    map.emplace(std::make_pair<OpFn, char>({1, 2, 3}, 'A'));

    debug::printsf("%%", map.find({1, 2, 3})->second);

    struct IType {

      long id; // type class: int, str, obj, proto
      long M;  // proto id: proto.X

    };

    struct IData {

      IType type;
      long data;

    };

    /*
    Runtime-Bsp: add $1, $2

    t1 := $1.type.id
    t2 := $2.type.id
    fn := op_fn[OpFn{t1,t2,OP_ADD}]

    call fn
    
    */

   // map.emplace(std::make_pair(OpFn{1, 2, 3}, 'A'));
   // map.emplace(std::make_pair(OpFn{8, 7, 6}, 'B'));

    //debug::printsf("Hash: %% : %%", hash, hash % 7);

    return 0;
  }

  if (false) {
    ProgramNode program {"test"};

    Status s1 = append(&program.block, makeNode<VarDecl>("i1", types::VarScope::Local));
    Status s2 = append(&program, makeNode<VarDecl>("i2", types::VarScope::Local));
    Status s3 = append(&program.block, makeNode<VarDecl>("i3", types::VarScope::Global));

    std::vector<VarDecl*> decls = nodesByType<VarDecl>(program.block.nodes);
    debug::printsf("DECLS %% of %%", decls.size(), (int) TypedNode<VarDecl>::DEF_TYPE);

    debug::printsf("Tree: %%", (int) program.base.type);
    debug::printsf(" .decl: %%", (int) program.block.nodes[0]->base.type);

    debug::printsf("%%, %%", s1, s2);

    ast::print(&program);

    return 0;
  }

  console::Options opts ({
    { .name = "file", .symbol = 'f', .reqval = true },
    { .name = "compile", .symbol = 'c' }
  });

  std::vector<std::string> args;
  int status = console::ConsoleOptions::read(opts, args, argc, argv);

  debug::printsf("Status: %%", status);
  debug::printsf("Compile? %%", opts.isset("compile"));

  for (const auto& opt : opts.options) {
    debug::printsf(" * %% (%%): %%", opt.name, opt.reqval, opt.value);
  }

  for (const auto& arg : args) {
    debug::printsf(" - %%", arg);
  }

  /**/

  std::string entryFile;

  if (args.size() < 2) {
    debug::printsf("$0`1Error: expected file argument.");
    debug::printsf("$0`1Usage: ms [-cf] $uFILE");

    return -1;
  } else if (args.size() == 2) {
    entryFile = args[1];
  }

  // if (true) {
  //   return 0;
  // }

  //ms::printBanner();
  debug::printsf("$b<$2!$9> MultiScript 2.1");
  debug::printsf("Default types: %%, %%, %%", types::Int, types::Decimal, types::String);

  ms::Context ctx;

  ctx.sourceLocations.push_back("scripts");

  // ms::registerSource(ctx, "ex.ms");
  // ms::registerSource(ctx, "ex.ms");
  // ms::registerSource(ctx, "test.ms");
  // ms::registerSource(ctx, "test2.ms");
  ms::registerSource(ctx, entryFile);

  ms::printSources(ctx);

  if (!opts.isset("compile")) {
    debug::printsf("Running compiled file ... %%", ctx.entrySource ? ctx.entrySource->name : "<missing source>");
    return 0;
  }

  if (true) {
    ms::Compiler compiler(ctx);

    for (size_t i = 0; i < ctx.entrySource->tokenCount(); i++) {
      const Token& token = ctx.entrySource->tokens[i];

      debug::printsf(" -- (%%) $6%% %%$r (%%:%%)", i, token.type, token.value, token.line, token.col);
    }

    ctx.makeCurrent(ctx.entrySource);

    Status s { Status::Success };

    if ((s = compiler.generateAST(*ctx.entrySource)) != Status::Success) {
      ctx.throwd(s, "failed to generate AST");
      //return 0;
    }

    if (ctx.inErrorState()) {
      ctx.logErrors(true);
    } else {
      ast::print(compiler.programTree());
    }

    return 0;
  }

  // --

  ms::Instructions ins;
  ms::parse(ctx, ins);

  debug::printsf("====");
  debug::printsf("");
  debug::printsf("%%", structureString(ctx.globalScope));
  debug::printsf("%%", structureString(ctx.module));

  debug::printsf("Literals:");

  for (size_t i = 0; i < ctx.literals.literals.size(); i++) {
    debug::printsf(" %%: %% @ %%", i, ctx.literals.literals[0].type,
      SFmt<Literal>{}.toVerboseString(ctx.literals.literals[0]));
  }
  
  debug::printsf("");
  printInstructions(ctx.instructions);

  // --

  // ...

  return 0;
}

/*

void printMem(const ms::memory::Memory<char>& mem) {
  for (size_t i = 0; i < mem.capacity; i++) {
    std::cout << i << ": " << ms::toBinary(mem.data[i]) << '\n';
  }

  std::cout << std::endl;
}

void test() {
  using namespace ms;

  // TODO: test something
  msx::Integral i1 = 0;
  ms::memory::CellMemory mem(8);

  printMem(mem.container);
  mem.write<msx::Integral>(0, 15, 256);
  printMem(mem.container);
  
  ms::memory::DataCell<msx::Integral> i = mem.read<msx::Integral>(0);
  void* raw = (void*) (mem.container.data + 1);
  int rawValue = *reinterpret_cast<int*>(raw);
  std::cout << "RAW; base=" << (void*) mem.container.data << " .. raw=" << raw << " .. value=" << rawValue << '\n';
  std::cout << "READ " << i.data << '\n';
}
*/