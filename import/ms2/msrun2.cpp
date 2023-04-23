#define MS_ENABLE_GLOBAL_NAMESPACE // enable all 'using namespace's
#define MS_DEBUG true
#define MS_VM_ANALYSIS
#define MS_TRACK_IDATA

#include "fast2.hpp"
#include <chrono>
#include <tuple>
#include <utility>

using namespace ms;
using namespace std::chrono;

/* TODO:
*

== CRUCIAL ==

- when parsing 'extern def' -> in cpp-code assign ms-function result type to be expected
    ctx.ext().bind("a", DataType::VOID, [] ...) -> assigns result type of 'a' to VOID

- Expressions must be generic in a form like:
    DataType resultType = expression.getResultType({DataType... for each ref used});

- make Expression an Entity
- expression count on functions calls is invalid (get_info('op-info', 1 + 2) => 4 instead of 2 ???)

- Function::containsReturn must consider ALL return statements!

- memory::LocalMemory::write() -> on override: could delete data twice?
  AND is check for isProtected() sufficient???

- operator < should be connected with the jge!!

- check if Data<>::assign() works inside the Data<> class!


*
* --- General TODOs ---
*
// Feature Reflections
** - generate Meta of everything and store it
** - create protos for all language entities (functions, expressions, ...)
** - allow direct feedback of compilation data and runtime data => printable objects / serializable
*
// Feature "FunctionRegistry"
** - save function meta data (instruction index, params given, local count) and pass to VM (in .cms file or directly) -> FunctionRegistry
*
** - make all 'stringifys' to const maps
*
** - findExpressionStart - does it work?
** - parseExpression(), VARG function calls
** - memory::getDataTypeForIdentifier()
** - default parameters should be equal to type guards
     => read '0' as INT not decimal
*
// Feature "Nested Functions" -- needed?
** - allow nested functions? see 'parseFunction()' .. owner is always the module? OR owner = ctx.currentNamespace
    -> BUG: a.b() will pgref 0, pattr 'b' INSTEAD of call 'function a.b' .. MUST BE COMPILE-TIME
*
** 'self' keyword -> see parseExpression@len=1
** remove MS_ERROR_UNFINISHED_LABEL_APPLICATION (in Context::leaveBlock() and InstructionSet::leaveLayer())
** remove parseEntity -> use parseExpression always?
** numbers like '1.1abc2bba' are possible -> in Tokenizer check if we are still in a number
** FParam::guard -> TypeSpec to check for proto types
** Expression::isLogical() should refer to lang::isLogical(Operation op)
*
* --- Improvements ---
*
** - tokenize from file stream instead from string (allow both but prefer stream)
** - in parseFunction(): (stage 0) check only once for the function name, either in '(' or in else part
** - stop 'parseExpression' if everything is parsed, aka not restart 'maxPrecedence'-times
** - lang::Function::getSignature(): add mods
** - XMods::stringMap should be a pointer and not be stored in the object
*
* --- Ideas ---
*
** memory::assignData() -> should update owner?
*
* --- BUGS ---
*
** - Functions can be owners of functions -> parseFunction@dotOperator
** - toMarkedString(): show only range of tokens
** - a[0][0] are handled as two expressions: a[0] and [0] -> change!
*
** - Error: {a} -> <unknown data type (8021)> with code:
        let i = 0
        let a = a(1 + 2)#1

    -> a is still saved as Entity(type=Variable) and NOT Variable
    -> that's why Variable::dataType in Entity is not existing and returns garbage
*
* -- Error Throwing Code:

    ==== MS code ====

    def extern size(x)

    def sum(x...):
    let sum = 0
    let i = 0

    while i < size(x) do
        sum = sum + x[i, i * 2] /// <--- location of interest -> no error on 'x[i]' || xenum allocation? - why error on fcall sum()?
        i = i + 1
    end

    return sum
    end

    debug sum(1, 2, 3, '@')

    ==== output ====

    ...
    [51..52 -> 1] sum
    REQUEST getEntity(name: sum, namespace: test22)
    RESULT -> {name: sum, mods: [], start: 0xffffffff, isVoid: false, containsReturn: true, returnType: undefined, paramCount: 0, params: [{index: 0, name: x, guard: undefined, literal: ffffffff, varg: true}]}
     invoke sum(x...) with pcount=2
      x (undefined) := 1 , 2 , 3 (undefined) -> 1
    f(2011,0x11b0f85c0) malloc: can't allocate region
    *** mach_vm_map(size=140447288668160) failed (error code=3)
    f(2011,0x11b0f85c0) malloc: *** set a breakpoint in malloc_error_break to debug

    Caught Exception:
    ! Compilation failed due to a std::exception:
    ! std::bad_alloc

*
*
*
* --- Proposals ---
*
// Compiler; Hybrid; Interpreter
** - well defined program structure for compiler
** - JAVA like hybrid
** - direct expression interpreter
*
// Requirements & Function DECLarations (decl)
** - function call requirements -> compile and runtime
    a) Determined at compile-time:
        decl add(A a, B b) requires implements(A, +) and implements(B, +) end
        decl sub(A a, B b) requires is_local(A::value) and {is_unsigned & is_numeric}(A::value) end

    b) Determined at runtime: (type of a and b might differ through control strutures)
        decl add(a, b) requires typeof(a) is typeof(b) end
        decl sub(a, b) requires is_numeric(a) and is_numeric(b) end
** - function forwarding
    a) def add(a, b): return a + b end
       decl add_secure(a, b) requires numeric(a, b) -> add(a, b) end

    b) def X::add(b): val = val + b return self end
       decl X::+(X y) -> X::add(y.value) end
*
* --- Docs ---
*
** @not
    -> not negates an expression
    -> can be used in following cases:
        a) in front a logical operator (is, <>, <, >, <=, >=)
            if i is not 0
        b) in front of parenthesis to negate a full expression
           (Note: 'not' will be treated as a reference even though it is a COMPILE TIME key word,
                   so the expression parse accepts it as a len=1 input but only called using the ()-operator)
            not (i is 0)
*
** @Varg-function-call
    -> when count of x... is 0..1 -> no or the single IData* will be passed
    -> when count of x... is  >1  -> the 'collect'-operator creates a Container first and pushed this
    => This must be considered when handling with VARG on extern functions.
       (see invalid casting from IData* to Container* without checking the type!)
    => When using in the script it-self, a size check of the VARG parameter must be done at first:
        def a(x...):
          if size(x) is 0 then return end
          else if size(x) is 1 then <do something with x> end
          else <use x[index] for the entries up to size(x)-1> end
        end
*
**/

template <typename T>
class BasicAllocator {
    private:
    public:
        BasicAllocator() {}
        virtual ~BasicAllocator() {}

        T* allocate(size_t size) {
            if (size > 0)
                return new T[size];

            return nullptr;
        }

        T* realloc(const T* data, size_t oldSize, size_t newSize) {
            if (newSize >= 0 && newSize > oldSize) {
                T* ndata = new T[newSize];

                if (!data)
                    return ndata;

                for (size_t i = 0; i < oldSize; i++) {
                    ndata[i] = data[i];
                }

                return ndata;
            }

            return nullptr;
        }
};

template <typename T>
class PrimitveDestructor {
    public:
        PrimitveDestructor() {}
        virtual ~PrimitveDestructor() {}

        void destruct(const T& element) {}
};

template <typename T>
class BasicDestructor {
    public:
        BasicDestructor() {}
        virtual ~BasicDestructor() {}

        void destruct(const T& element) {
            element.~T();
        }
};

template <typename T>
class Destructor : public std::conditional_t<std::is_trivially_destructible_v<T>, PrimitveDestructor<T>, BasicDestructor<T>> {};

template <typename T, typename Alloc = BasicAllocator<T>, typename Destruct = Destructor<T>>
// requires T::operator=
class List {
    private:
        Alloc alloc;
        Destruct destruct;

    protected:
        size_t capacity {0}, top {0};
        T* data {nullptr};

        void allocate(size_t size) {
            T* ndata;
            
            if (top == 0)
                ndata = alloc.allocate(size);
            else {
                ndata = alloc.realloc(data, top, size);

                for (size_t i = 0; i < top; i++) {
                    destruct.destruct(data[i]);
                }

                delete[] data;
                data = nullptr;
            }

            if (!ndata)
                throw std::bad_alloc();

            data = ndata;
            capacity = size;
        }

    public:
        List(size_t initSize = 16, Alloc allocator = BasicAllocator<T>(), Destruct destructor = Destructor<T>()) :
            capacity(initSize), alloc(allocator), destruct(destructor) {

            if (capacity == 0)
                capacity = 2;
                
            allocate(capacity);
        }

        virtual ~List() {
            if (data) {
                clear();

                delete[] data;
                data = nullptr;
            }
        }

        T* operator [](size_t index) const {
            if (index >= 0 && index < top)
                return &data[index];

            return nullptr;
        }

        T& get(size_t index, const T& fallback) const {
            if (index >= 0 && index < top)
                return data[index];

            return const_cast<T&>(fallback);
        }

        bool insert(T element) {
            if (top == capacity) {
                try {
                    allocate(capacity + capacity / 2);
                } catch (...) {
                    return false;
                }
            }

            data[top++] = element;

            return true;
        }

        bool add(const T& element) {
            if (top == capacity) {
                try {
                    allocate(capacity + capacity / 2);
                } catch (...) {
                    return false;
                }
            }

            data[top++] = element;

            return true;
        }

        bool remove(size_t index) {
            if (index >= 0 && index < top) {
                destruct.destruct(*data[index]);

                for (size_t i = index; i < top; i++) {
                    data[i] = data[i + 1];
                }

                top--;

                return true;
            }

            return false;
        }

        void clear() {
            for (size_t i = 0; i < top; i++) {
                destruct.destruct(data[i]);
            }

            top = 0;
        }

        T* begin() const {
            return data;
        }

        T* end() const {
            return data + top;
        }

        bool empty() const {
            return top == 0;
        }

        size_t size() const {
            return top;
        }
};

#define MODULE_PATH "/users/nuskel/documents/vs-code/ms"

/*

 DO NOT CHECK vm.checkStack(params) SINCE THE ExtHandler ALREADY POPPED THE DATA AND PUT
 IT INTO THE CALL OBJECT. TESTING FOR THE STACK IS NOT NECESSARY!

*/

state ms_math_pow(Context& ctx, VM& vm, ext::Call& call) {
    IData* b = call.getParam("b");
    IData* e = call.getParam("e");

    if (checkType(b, DataType::INTEGRAL) && checkType(e, DataType::INTEGRAL)) {
        vm.push(new Integral(pow(extractInt(b), extractInt(e))));
    }

    return MS_SUCCESS;
}

state push_info_object(Context& ctx, VM& vm, ext::Call& call) {
    if (!checkType(call.getParam("registry"), DataType::STRING) || !checkType(call.getParam("id"), DataType::STRING))
        return vm.throwException(MS_EXCEPT, "extern call", "registry and id must be a string");

    std::string registry = call.getParam("registry")->get<std::string>();
    std::string id = call.getParam("id")->get<std::string>();
    info::LangInfoObject* info = info::get_info(registry.c_str(), id.c_str());

    if (info) {
        std::stringstream buf;
        info->toString(buf);
        vm.push(new String(buf.str()));
    } else {
        vm.push(new String("Error {not found}"));
    }

    return MS_SUCCESS;
}

//

class StringData {

    int calc(int a, int b) {
        const auto add = [](int x, int y) { return x + y; };
        const auto add_and_mul = [&](int x, int y, int r) { return r * add(x, y); };

        return add_and_mul(a, b, 2);
    }

};

//

template <char... str>
struct const_string {
    static constexpr inline const char data[sizeof...(str) + 1] {str..., '\0'};
    static constexpr inline size_t size { sizeof...(str) + 1 };
};

//

template <size_t X>
struct Fib {
    static constexpr inline size_t value = Fib<X - 1>::value + Fib<X - 2>::value;
};

template <>
struct Fib<0> {
    static inline const size_t value = 0;
};

template <>
struct Fib<1> {
    static inline const size_t value = 1;
};

//

// fib(i - 1) + fib(i - 2)
// fib(0, 1) = i

namespace immutable {

    struct string
    {
        template<std::size_t N>
        constexpr string(const char (&s)[N])
        : _data(s)
        , _size(N-1)
        {}

        constexpr const char* data() const { return _data; }
        constexpr std::size_t size() const { return _size; }

        constexpr char operator [](size_t index) const {
            return _data[index];
        }

        const char* const _data;
        const std::size_t _size;
    };
    std::ostream& operator<<(std::ostream& os, string s)
    {
        return os.write(s.data(), s.size());
    }
}

//

int main(int argc, char ** argv) {
    ms::allow_non_assigning_non_void_func = true;
    ms::debug_translate_state_codes = true;
    ms::debug_level = 5;

    Context ctx {MODULE_PATH};
    ProgramArgs args {argc, argv};

    args.directExecution = true;

    //ms::experimental::test();

    // extern calls
    ctx.ext().bind("size", DataType::INTEGRAL, {"x"}, [](Context& ctx, VM& vm, ext::Call& call) {
        int pcount = call.getParamCountOnCall();
        int vargs = call.getVargCount();
        int size = pcount;

        if (!checkType(call.getParam("x"), DataType::CONTAINER)) {
            debug::printsf("x is not a container ....");
        }

        vm.registry.integral.assign(size);
        vm.push(vm.registry.integral.value());

        //vm.push(new Integral(size));

        return MS_SUCCESS;
    });

    ctx.ext().bind("pow", DataType::UNDEFINED, {"b", "e"}, &ms_math_pow);
    ctx.ext().bind("get_info", DataType::OBJECT, {"registry", "id"}, &push_info_object);

    state s = execute(ctx, "examples/test22.ms", args);

    /*

    IData* custom = new Data<DataType::DECIMAL, double64>(200.0f);
    IData* source = new Integral(101);

    std::cout << "target = " << toString(custom) << "; source = " << toString(source) << "\n";
    
    state x = assignData(custom, source);

    std::cout << "after assignment ... x => " << x << "\n";
    std::cout << "target = " << toString(custom) << "; source = " << toString(source) << "\n";

    delete custom;
    delete source;

    */

    // -- SDATA //

    //

    //std::cout << "end of main()\n highest IData count at one time: " << memory::highestCount << "\n";

    // --

    return 0;
}