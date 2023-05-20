#ifndef MS_OPCODE_HPP
#define MS_OPCODE_HPP

#include <iostream>
#include <vector>

#include "env.hpp"
#include "memory.hpp"

namespace ms {

  enum class Op {

    NOP,

    LEA,
    MOV,
    PUSH,

    /* POP <target> -- writes highest stack value to a memory cell */
    POP,
    
    CALL,
    RET,

    ADD

  };

  struct OpInfo {

    Op operation;
    std::string name;
    size_t params;

    OpInfo() : operation(Op::NOP), name("unset"), params(0) {}
    OpInfo(Op p_op, std::string p_name, size_t p_paramCount) : operation(p_op), name(p_name), params(p_paramCount) {}

  };

  static MS_VALUE_MAP(Op, OpInfo) operations {
    { Op::ADD, OpInfo { Op::ADD, "ADD", 2 } },
    { Op::POP, OpInfo { Op::POP, "POP", 1 } },
  };
  static MS_MAP_LOOKUP(getOpInfo, operations, Op, OpInfo, OpInfo {});

  struct memloc {

    const static inline char UNSET = 0;
    const static inline char STACK = 1;
    const static inline char INTERMEDIATE = 2;
    const static inline char ADDR_LOCAL = 3;
    const static inline char ADDR_CONST = 4;
    const static inline char ADDR_STATIC = 5;

    char type { UNSET };
    long offset { 0 };

    union {

      msx::Integral integral;
      //msx::Decimal decimal;

    } intermediate;

    memloc(char p_type = UNSET) : type(p_type) {}
    memloc(char p_type, long p_offset) : type(p_type), offset(p_offset) {}
    explicit memloc(msx::Integral p_int) : type(INTERMEDIATE) { intermediate.integral = p_int; }
    explicit memloc(msx::Decimal p_dec) : type(INTERMEDIATE) { }

    static memloc stack(long offset) {
      return memloc { STACK, offset };
    }

    static memloc local(long offset) {
      return memloc { ADDR_LOCAL, offset };
    }

  };

  struct Instruction {

    Op op;
    memloc source;
    memloc target;
    std::string label;

    Instruction(Op operation) : op(operation) {}

    friend std::ostream& operator <<(std::ostream& stream, const Instruction& i) {
      return stream;
    }

  };

  struct InstructionBlock {

    std::string nlabel;
    std::vector<Instruction> instructions;

    virtual Status append(Op);
    virtual Status append(Op, memloc src);
    virtual Status append(Op, memloc src, memloc target);
    virtual Status append(Op, memloc src1, memloc src2, memloc target);
    
    virtual inline void applyLabelOnNext(const std::string_view label) {
      nlabel = label;
    }

  };

  struct Instructions : public InstructionBlock {

    size_t baseOffset { 0 };

    /* Lifecycle:
     *
     *  Instructions from parsing > compiled > linked (final and executable)
     */

  };

}

#endif