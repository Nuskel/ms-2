#ifndef MS_RUNTIME_HPP
#define MS_RUNTIME_HPP

#include <vector>

#include "env.hpp"
#include "opcode.hpp"
#include "memory.hpp"
#include "lang.hpp"

/* First decl */
namespace ms {

  struct Runtime;
  
  using OpCallback = Status (*)(Runtime&, const Instruction& i);

}

namespace ms {

  struct EvalMap {

    std::vector<OpCallback> callbacks;

    EvalMap() : callbacks((int) Op::__count__) {}

    Status put(const Op, OpCallback);

    Status eval(Runtime&, const Instruction&);

  };

  struct Runtime {

    EvalMap evalMap;
    memory::ProgramStack stack;

    memory::Register<memory::Int> intRegister {0};
    memory::Register<memory::Dec> decRegister {0.0};

    Runtime() {
      registerOpCallbacks();
    }

    void registerOpCallbacks();

    memory::Data find(const OpArg&);

    memory::IData load(const OpArg&); // from intermediate, address (global, local), stack or register; will manipulate stack

    memory::DataPos locateTarget(const Instruction&);

  };

  Status eval(Runtime&, const Instructions&);

  Status eval(Runtime&, const Instruction&);

}

#endif