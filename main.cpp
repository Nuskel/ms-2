#include <string>
#include <iostream>
#include <typeinfo>

#include "ms.hpp"

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

int main(int argc, char* argv[]) {
  using namespace ms;

  test();

  //ms::printBanner();
  debug::printsf("$b<$2!$9> MultiScript 2.1");
  debug::printsf("Default types: %%, %%, %%", types::Int, types::Decimal, types::String);

  ms::Context ctx;

  ctx.sourceLocations.push_back("scripts");

  ms::registerSource(ctx, "ex.ms");
  ms::registerSource(ctx, "ex.ms");
  ms::registerSource(ctx, "test.ms");
  ms::registerSource(ctx, "test2.ms");

  ms::printSources(ctx);

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

void test() {
  using namespace ms;

  // TODO: test something
}