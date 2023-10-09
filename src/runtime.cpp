#include "runtime.hpp"
#include "opcode.hpp"

#define MS_ASSERT_ARGC(i, count) if (i.args.size() != count) return Status::FAIL;

/* Op Callbacks */
namespace ms {

    Status eval__ADD(Runtime& r, const Instruction& i);

}

namespace ms {

    void Runtime::registerOpCallbacks() {
        evalMap.put(Op::ADD, &eval__ADD);
    }

    memory::Data Runtime::find(const OpArg& arg) {
        switch (arg.type)
        {
        case OpArg::Type::STACK:
            return stack.pop<memory::Data>();
        
        default:
            return memory::Data {};
        }
    }

    memory::DataPos Runtime::locateTarget(const Instruction& i) {
        if (i.target.type == memloc::STACK) {
            return memory::DataPos { stack.data, stack.ptr };
        }
    }

    /* ******** */

    Status eval(Runtime& r, const Instruction& i) {
        Status s { Status::SUCCESS };
        
        s = r.evalMap.eval(r, i);

        return s;
    }

    /* Instruction Handlers */

    Status eval__ADD(Runtime& r, const Instruction& i) {
        MS_ASSERT_ARGC(i, 2)

        /* NOTE:
         *  Operations with protos only work with defined
         *  operator functions. These will be expressed
         *  as a "call->push result->add with stack" execution
         *  order. The intrinsic "add(IData, IData, IData)"
         *  function must not handle higher operations and
         *  operates only on the bit level.
         * 
         * Example:
         *  proto Number { member x; def op+(y): x = x + y return self }
         * 
         *  let x = Number(10)
         *  x = x + 10
         *  print(x.x)
         * 
         * Will generate:
         *  ... code to setup proto object with init (local x = 10)
         *  PUSH INT(10)  -- push intermediate
         *  CALL proto::Number::op::+  -- call function; see insructions below
         *   ADD local:var(x), stack:pop, local:var(x)  -- pop (10), add with self.x => 20; save result to self.x
         *   PUSH self  -- push return value (self)
         *   RET  -- return to call instruction + 1
         *  ASSIGN local:var(x), stack:pop  -- pop the object ref (self) and write to x (self = self) -> expression is now of type <proto:Number>
         */

        memory::Data d0 = r.find(i.args[0]);
        memory::Data d1 = r.find(i.args[1]);
        memory::DataPos target = r.locateTarget(i);

        // *-- NEW:
        memory::IData _d0 = r.load(i.args[0]);

        memory::add(_d0, _d0, _d0);
        // --*

        size_t adv = memory::add(d0, d1, target);

        // if target.type == STACK
        r.stack.advance(adv);

        // r.stack.push(r);
        r.intRegister.store(20);
        r.intRegister.storeFrom(static_cast<memory::Int*>(r.stack.data) + 10);

        return Status::SUCCESS;
    }

}

#undef MS_ASSERT_ARGC