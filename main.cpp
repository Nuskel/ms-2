#include <string>
#include <iostream>
#include <sstream>
#include <typeinfo>
#include <vector>

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

int main(int argc, char* argv[]) {
  using namespace ms;

  // test();

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