#include "opcode.hpp"
#include "debug.hpp"

namespace ms {

  Status InstructionBlock::append(Op op) {
    return Status::SUCCESS;
  }

  Status InstructionBlock::append(Op op, memloc source) {
    return Status::SUCCESS;
  }

  Status InstructionBlock::append(Op op, memloc source, memloc target) {
    Instruction i { op };

    i.source = source;
    i.target = target;

    if (nlabel.length() > 0) {
      i.label = nlabel;
      nlabel = "";
    }

    instructions.push_back(i);

    debug::printsf("$%%", i);

    return Status::SUCCESS;
  }

  Status InstructionBlock::append(Op op, memloc source1, memloc source2, memloc target) {
    return Status::SUCCESS;
  }

}