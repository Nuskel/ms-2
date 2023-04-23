/**/

#ifndef MS_FAST_HPP
#define MS_FAST_HPP

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <functional> // hash?
#include <iomanip>
#include <chrono>
#include <map>
#include <algorithm>
#include <unordered_map>
#include <initializer_list>
#include <typeinfo>
#include <memory>
#include <variant>

// --- Pre-Compiler ---

// Global information
#define MS_FILE_NAME "fast2.hpp"
#define MS_SUPPORTED_CHARSET "ASCII"

// Global variables
#define MS_NIL 0
#define MS_NULL 0
#define MS_DEFAULT_INTEGRAL_VALUE 0
#define MS_DEFAULT_DECIMAL_VALUE 0.0
#define MS_DEFAULT_STRING_VALUE ""

#define MS_THIS_KEYWORD "self"

#define MS_INSTRUCTION_BLOCK_SIZE 32
#define MS_MAX_NAMESPACE_DEPTH 32
#define MS_MAX_STACK_FRAMES 64

#define MS_CMD_EXTRACT_META 0x12

// General States
#define MS_NO_OP -3
#define MS_FATAL -2
#define MS_ERROR -1
#define MS_SUCCESS 0
#define MS_FAIL 1
#define MS_ERROR_INTERNAL 2
#define MS_WARNING 300

// General Errors
#define MS_ERROR_NULL 500
#define MS_ERROR_DUPLICATE 501
#define MS_ERROR_UNSUPPORTED_LANG_CONSTRUCT 502
#define MS_ERROR_OUT_OF_BOUNDS 505
#define MS_ERROR_HIGHER_INSTRUCTION_LEVEL_EXISTS 506
#define MS_ERROR_ALREADY_BASE_INSTRUCTION_LEVEL 507
#define MS_ERROR_UNFINISHED_LABEL_APPLICATION 508

// File, Code and Module
#define MS_ERROR_MISSING_FILE 520
#define MS_ERROR_CODE_ALREADY_READ 521
#define MS_ERROR_INVALID_CODE_STATE 522

// Tokenizing and Syntax
#define MS_ERROR_INVALID_SYNTAX 530
#define MS_ERROR_INVALID_DECIMAL_FORMAT 531
#define MS_ERROR_EMPTY_STRING_LITERAL 532
#define MS_ERROR_INVALID_EXPRESSION_STATE 540
#define MS_ERROR_INSUFFICIENT_PARAM_COUNT 541
#define MS_ERROR_MALFORMED_FUNCTION_DECLARTION 542
#define MS_ERROR_INVALID_OPERAND_TYPES 543

// Entities
#define MS_ERROR_NO_ENTITY_FOR_NAME 550
#define MS_ERROR_ENTITY_ALREADY_REGISTERED 551
#define MS_ERROR_INVALID_ENTITY_NAME 552

// Extern
#define MS_ERROR_NO_EXT_LINKING 560
#define MS_ERROR_INVALID_EXT_LINK 561

// == RUNTIME ==

#define MS_GLOBAL_MEMORY_SIZE 64

// Rutime control codes
#define MS_EXIT_LIS 0
#define MS_EXIT_IIS -1

#define MS_EXCEPT 1
#define MS_EXCEPT_NULL 2
#define MS_EXCEPT_EMPTY 3
#define MS_EXCEPT_INVALID_STATE 4
#define MS_EXCEPT_INVALID_DATA_TYPE 5
#define MS_EXCEPT_OUT_OF_BOUNDS 9
#define MS_EXCEPT_10 10
#define MS_EXCEPT_STACK_UNDERFLOW 11
#define MS_EXCEPT_STACK_OVERFLOW 12
#define MS_EXCEPT_ARTITHMETIC 20

#define MS_EXCEPT_TYPE_MISMATCH 50
#define MS_EXCEPT_CONST_ASSIGN 51
#define MS_EXCEPT_PROTECTED_ASSIGN 52
#define MS_EXCEPT_INTERNAL_ASSIGNMENT_FAIL 53

// -- MAKROS  --

/* Define any of these:
 *  - MS_DEBUG {true | false}
 *  - MS_TRACK_IDATA
 */

namespace ms {
    
    #ifdef MS_DEBUG
        constexpr static bool ms_is_debug { MS_DEBUG };
    #else
        constexpr static bool ms_is_debug { false };
    #endif

    // Define MS_TRACK_IDATA to log each initialized IData* object and
    // its deconstruction. Will alert the system if on Context destruction
    // any IData* objects are not freed properly.
    #ifdef MS_TRACK_IDATA
        constexpr static bool is_track_idata { true };
    #else
        constexpr static bool is_track_idata { false };
    #endif

    #ifdef MS_VM_ANALYSIS
        static bool is_vm_analysis { true };
    #else
        static bool is_vm_analysis { false };
    #endif

    static bool ms_auto_create = true; // TODO: rename to 'ms_auto_create_var' ??
    static bool allow_non_assigning_non_void_func = false;
    static bool assign_only_on_defined_types = true; // IData::assign() only valid in (x : public Data<>) and not in Data<> itself

    static bool debug_translate_state_codes = false;
    static int debug_level = 0; // 0 = nothing; 4 - all

}

// === Debug Guards ===

#define MS_DEBUG_START if (ms::ms_is_debug) {
#define MS_DEBUG_END }
#define MS_IF_DEBUG if (ms::ms_is_debug)
#define MS_ON_DEBUG(x) MS_IF_DEBUG { x; }
#define ms_debug(...) if constexpr (ms::ms_is_debug) { debug::printsf(__VA_ARGS__); }

// === State Checks ===

#define MS_ASSERT_STATE(s, call) if ((s = call) != MS_SUCCESS) return s;
#define MS_ASSERT_EXPR(ctx, expr, code, from, to, s, op)                                \
    if ((s = parseExpression(ctx, expr, code, from, to)) != MS_SUCCESS)                 \
        return ctx.throwd(s, from, to - 1, "failed to parse expression for '%%'", op);

// === Expression Control ===

// ... coming soon

// === memory ... ===

// Creates a struct containing information about the Data<.., ..> implementation.
#define MS_DATA_INFO_STRUCT(type, wrapper_name, enum_data_type)         \
    template <>                                                         \
    struct DataInfo<type> {                                             \
        using wrapper = wrapper_name;                                   \
        using value_type = type;                                        \
        static constexpr inline DataType data_type { enum_data_type };  \
    };

// === Dynamic Tables ===
// Idea: https://www.youtube.com/watch?v=kDqS1xVWGMg

// Generates a vector containing pair literals filled out in the source code.
#define MS_VALUE_MAP(K, V) std::vector<std::pair<K, V>>

// Generates the lookup function for the 'table'-vector. Contains a static const
// map copy of the vector to be accessed at runtime.
#define MS_MAP_LOOKUP(func, table, K, V, fallback)                                  \
    V func(K key) {                                                                 \
        static const auto map = std::map<K, V> { table.begin(), table.end() };      \
        static V staticFallback = fallback;                                         \
                                                                                    \
        if (map.find(key) == map.end())                                             \
            return /*const_cast<V>(*/staticFallback/*)*/;                                   \
                                                                                    \
        return map.at(key);                                                         \
    }

// Generates a vector for a certain container type
#define MS_VALUE_LIST(T) std::vector<T>

// Generates a lookup for a vector of containers where each attribute can be
// queried using the 'query' statement. The given parameter is named 'input'.
//
//  - Example: MS_LIST_QUERY_LOOKUP(get_int, integers, IntInfo, value < param, -1)
//     .. will search for the FIRST IntInfo with the value attribute (IntInfo::value) less than the parameter
//
#define MS_LIST_QUERY_LOOKUP(func, list, T, query, fallback)    \
    template <typename V>                                       \
    T& func(const V& input) {                                   \
        static T staticFallback = fallback;                     \
                                                                \
        for (auto& val : list) {                                \
            if (val.query)                                      \
                return val;                                     \
        }                                                       \
                                                                \
        return staticFallback;                                  \
    }

// --- MS Source Code ---

namespace ms {

    // -- Generic, Miscellaneous, Utility

    namespace types {

        using index = int;
        using uindex = unsigned int;
        using usize = unsigned int;
        using state = int;

        using operation = unsigned char; // -> 256
        using param = int;

        using Address = long;
        using int64 = long int;
        using double64 = long double;

        enum class State : state {
            ERRROR = -1,
            SUCCESS = 0,
            WARN = 1,
        };

        enum class DataType : short {

            // Non-Type values
            UNDEFINED   = 0,
            INVALID     = 1,
            NIL         = 2,

            // Primitives
            INTEGRAL    = 3,
            DECIMAL     = 4,
            STRING      = 5,
            
            // Containers
            CONTAINER   = 6,
            ARRAY       = 7,
            OBJECT      = 8,
            PROTO       = 9,

            // References
            REF         = 10,
            FCALL       = 11,
            VOID        = 12,

            __count__
        };

        enum class EntityType : int {
            UNKNOWN     = 0,
            NAMESPACE   = 1,

            MODULE      = 2,
            FUNCTION    = 3,
            OBJECT      = 4,

            IF          = 5,
            ELSE        = 6,
            ELSE_IF     = 7,

            LOOP        = 8,
            //FOR         = 8
            WHILE       = 9,

            TRY         = 10,
            CATCH       = 11,

            VARIABLE, // is var v
            PARAM,
            VARARG_PARAM,
            INTEGRAL,
            DECIMAL,
            STRING, // is var ^

            COMP_FUNC
        };

        enum class VariableType : int {
            UNDEFINED = -1,
            GLOBAL = 0,
            LOCAL,
            PARAM,
            VARG_PARAM,
            EXTERN // always global
        };

        enum class ClauseType : int {
            NONE        = 0,

            MODULE      = 1,
            FUNCTION    = 2,

            IF          = 3,
            ELSE        = 4,
            ELSE_IF     = 5,

            FOR         = 6,
            WHILE       = 7,

            TRY         = 8,
            CATCH       = 9
        };

        enum class ModuleState {
            UNREAD,
            READ,
            TOKENIZED,
            FAILED
        };

        enum class ProgramState : int {
            INVALID = -1,
            IDLE = 0,
            INIT,
            SETUP,
            FILE_READING,
            TOKENIZING,
            COMPILATION,
            RUNTIME,
            TERMINATED
        };

        // -- lang:: ... Instances

        enum class ProtoCallbackId : int {
            UNKNOWN = 0,

            CONSTRUCT, // called upon constructor call of a proto, should create the object instance
            DESTRUCT, // called upon deconstruction of a proto-object

            ASSIGN, // called if a proto-object gets assigned to something
            RETRIEVE, // called whenever something uses the proto-object

            MEMBER_ADD, // called when the proto-objects gets a new member after construction (only on non-const)
        };

        // -- Operation

        enum class Operation : int {
            NONE        = 0,

            ADD         = 1,
            SUBSTRACT   = 2,
            MULTIPLY    = 3,
            DIVIDE      = 4,
            MODULO      = 5,

            EQUALS, //is
            NOT_EQUALS,
            NOT,

            LESS_THAN,
            MORE_THAN,

            LESS_THAN_EQUALS,
            MAORE_THAN_EQUALS,

            AND,
            OR
        };

        enum class OperationType {
            UNDEFINED,
            ARITHMETIC,
            LOGICAL
        };

        enum class OperationDirection {
            LEFT_TO_RIGHT,
            RIGHT_TO_LEFT
        };

        //

        enum class Modifier : short {
            NONE        = 0,

            TEMP        = 1,
            EXTERN      = 2,
            CONST       = 4,
            NIL         = 8,
            PROTECTED   = 16,
            LINK        = 32, // for temporary references

            __count__ = 7 // current modifier count
        };

        // Mapped types
        const static std::unordered_map<int, std::string> ERR_CODES;

        // Exceptions

        class MSException {
            private:
                int id { 0 };
                std::string reason;

            public:
                MSException(int code, const std::string& description = "ms failed execution") : id(code), reason(description) {}

                int getCode() const {
                    return id;
                }

                const std::string& getReason() const {
                    return reason;
                }

                friend std::ostream& operator <<(std::ostream& stream, const MSException& exc) {
                    return stream << MS_FILE_NAME << "; ERROR (" << exc.id << "): " << exc.reason << std::endl;
                }
        };
        
    }

    namespace traits {

        using types::Modifier;

        namespace stringify {

            using namespace types;

            // -- Definitions

            static std::string FALLBACK_UNSUPPORTED { "<unsupported>" };

            template <typename T>
            struct string_format {
                std::string operator ()(const T& fallback) { return FALLBACK_UNSUPPORTED; }
            };

            template <>
            struct string_format<std::string> {
                std::string operator ()(const std::string& elem) { return elem; }
            };

            template <>
            struct string_format<const char*> {
                std::string operator ()(const char* elem) { return std::string(elem); }
            };

            template <>
            struct string_format<ProgramState> {
                std::string operator ()(const ProgramState ps) {
                    switch (ps) {
                        case ProgramState::INVALID:
                            return "invalid";
                        case ProgramState::IDLE:
                            return "idle";
                        case ProgramState::INIT:
                            return "init";
                        case ProgramState::SETUP:
                            return "setup";
                        case ProgramState::COMPILATION:
                            return "comp";

                        default:
                            return "<system>";
                    }
                }
            };

            template <>
            struct string_format<VariableType> {
                std::string operator ()(const VariableType op) {
                    switch (op) {
                        case VariableType::UNDEFINED:
                            return "undefined";
                        
                        case VariableType::GLOBAL:
                            return "global";
                        case VariableType::LOCAL:
                            return "local";
                        case VariableType::PARAM:
                            return "param";
                        case VariableType::EXTERN:
                            return "extern";

                        default:
                            return "<unknown type>";
                    }
                }
            };

            template <>
            struct string_format<ClauseType> {
                std::string operator ()(const ClauseType ct) {
                    switch (ct) {
                        case ClauseType::NONE:
                            return "none";
                        case ClauseType::MODULE:
                            return "module";
                        case ClauseType::IF:
                            return "if";
                        case ClauseType::ELSE:
                            return "else";
                        case ClauseType::ELSE_IF:
                            return "else_if";
                        case ClauseType::FOR:
                            return "for";
                        case ClauseType::WHILE:
                            return "while";
                        case ClauseType::FUNCTION:
                            return "function";
                        case ClauseType::TRY:
                            return "try";
                        case ClauseType::CATCH:
                            return "catch";

                        default:
                            return "<unknown clause type>";
                    }
                }
            };

            template <>
            struct string_format<Operation> {
                std::string operator ()(const Operation op) {
                    switch (op) {
                        case Operation::NONE:
                            return "<no op>";
                        
                        case Operation::ADD:
                            return "ADD";
                        case Operation::SUBSTRACT:
                            return "-";
                        case Operation::MULTIPLY:
                            return "*";
                        case Operation::DIVIDE:
                            return "/";
                        case Operation::MODULO:
                            return "%";

                        default:
                            return "<unknown operation>";
                    }
                }
            };

            template <>
            struct string_format<EntityType> {
                std::string operator ()(const EntityType et) {
                    switch (et) {
                        case EntityType::UNKNOWN:
                            return std::string("unknown");
                        case EntityType::MODULE:
                            return std::string("module");
                        case EntityType::FUNCTION:
                            return std::string("function");
                        case EntityType::LOOP:
                            return std::string("loop");
                        case EntityType::VARIABLE:
                            return std::string("variable");

                        default:
                            return std::string("<unknown entity type>");
                    }
                }
            };

            template <>
            struct string_format<Modifier> {
                std::string operator ()(const Modifier mod) {
                    switch (mod) {
                        case Modifier::NONE: return std::string("none");
                        case Modifier::NIL: return std::string("nil");
                        case Modifier::TEMP: return std::string("temp");
                        case Modifier::CONST: return std::string("const");
                        case Modifier::EXTERN: return std::string("extern");
                        case Modifier::LINK: return std::string("link");
                        case Modifier::PROTECTED: return std::string("protected");

                        default:
                            return std::string("<unknown modifier>");
                    }
                }
            };

            template <typename T, typename = void>
            struct has_default : std::false_type {};

            // see: is_stream_writable
            // https://stackoverflow.com/questions/22758291/how-can-i-detect-if-a-type-can-be-streamed-to-an-stdostream
            template <typename T>
            struct has_default<T, std::void_t<decltype(std::declval<std::ostream&>() << std::declval<T>())>> : std::true_type {};

            template <typename T>
            std::string toString(const T& value, const std::string& fallback = "-") {
                const std::string fmt { string_format<T> {}(value) };

                /* Note: not the most efficient way, may be changed later...
                * If *value* has no custom string_format but can be
                * streamed to a std::ostream, this is chosen over the
                * fallback.
                */
                if (fmt == FALLBACK_UNSUPPORTED) {
                    if constexpr (has_default<T>::value) {
                        std::stringstream buf;

                        buf << value;

                        return buf.str();
                    }

                    return fallback;
                }
                
                return fmt;
            }

        }

        struct Modifiers {
            int data {0};

            Modifiers() {}
            explicit Modifiers(int idata) : data(idata) {}
            Modifiers(Modifier mod) { data = (int) mod; }
            Modifiers(std::initializer_list<Modifier> modifiers) {
                for (const Modifier& mod : modifiers)
                    data |= (int) mod;
            }

            Modifiers& operator =(const Modifiers& m) {
                if (this != &m)
                    data = m.data;
                
                return *this;
            }

            // -- Static Access

            template <typename Primitive>
            static bool ispresent(Primitive data, Modifier m) {
                return (data & (int) m) == (int) m;
            }

            template <typename Primitive>
            static void apply(Primitive& data, Modifiers m) {
                data = m.data;
            }

            template <typename Primitive>
            static int count(Primitive data) {
                int count = 0;

                for (int i = 0, x = 1; i < (sizeof(Primitive) * 8); i++) {
                    if ((data & x) != 0)
                        count++;

                    x = x << 1;
                }

                return count;
            }

            static std::string modToString(Modifier mod) {
                switch (mod) {
                    case Modifier::NONE:
                        return std::string("none");
                    
                    case Modifier::TEMP:
                        return std::string("temp");

                    case Modifier::EXTERN:
                        return std::string("extern");

                    case Modifier::CONST:
                        return std::string("const");

                    case Modifier::NIL:
                        return std::string("nil");

                    case Modifier::PROTECTED:
                        return std::string("protected");

                    case Modifier::LINK:
                        return std::string("link");

                    default:
                        return std::string("<unknwon>");
                }
            }

            // -- Object Access

            bool isset(Modifier mod) const {
                return (data & (int) mod) == (int) mod;
            }

            void set(Modifier mod, bool state) {
                if (state)
                    data |= (int) mod;
                else
                    data &= ~((int) mod);
            }

            void enable(Modifier m) {
                set(m, true);
            }

            void disable(Modifier m) {
                set(m, false);
            }

            void toggle(Modifier mod) {
                set(mod, !isset(mod));
            }

            int activeCount() const {
                int count = 0;

                for (int i = 0, x = 1; i < (sizeof(int) * 8); i++) {
                    if ((data & x) != 0)
                        count++;

                    x = x << 1;
                }

                return count;
            }

            std::ostream& toString(std::ostream& stream) const {
                stream << "[";

                if (data == 0) {
                    return stream << "]";
                }

                for (int i = 0, c = 0, x = 1, max = activeCount(); (i < (sizeof(int) * 8)) && c < static_cast<int>(Modifier::__count__); i++) {
                    if ((data & x) == x)
                        stream << modToString((Modifier) x) << (++c < max ? ", " : "");
                    
                    x *= 2;
                }
                
                return stream << "]";
            }
        };

        // -- Functions

        template <typename A, typename B>
        constexpr bool are_same_type(A a, B b) {
            return std::is_same<A, B>::value;
        }

        template <typename A, typename B, typename... O>
        constexpr bool are_same_type(A a, B b, O... others) {
            return are_same_type<A, B>(a, b) && are_same_type(a, others...);
        }

    }

    namespace util {

        using namespace types;

        // Base of all Tuple structs used for any conversion
        template <int elems>
        struct Tuple {
            int elementCount = elems;
        };

        template <typename A, typename B>
        struct Pair : public Tuple<2> {
            A a;
            B b;

            Pair (const A& initA, const B& initB) : a(initA), b(initB) {}
        };

        template <typename A, typename B, typename C>
        struct Triplet : public Tuple<3> {
            A a;
            B b;
            C c;

            Triplet (const A& initA, const B& initB, const C& initC) : a(initA), b(initB), c(initC) {}
        };

        template <typename T>
        struct Array {
            T * m_elements = nullptr; // ALWAYS set to nullptr, otherwise would point to trash -> if (m_element) = true -> malloc error on delete
            usize m_size;

            Array(usize size) : m_size(size) {
                if (size > 0)
                    m_elements = new T[size];
            }

            virtual ~Array() {
                if (m_elements)
                    delete[] m_elements;
            }

            void resize(usize newSize) {
                if (newSize == 0) {
                    *this.~Array();

                    return;
                }

                T * m_new = new T[newSize];

                for (uindex i = 0; i < newSize; i++) {
                    if (i < m_size)
                        m_new[i] = m_elements[i];
                }

                delete[] m_elements;

                m_elements = m_new;
            }

            bool validIndex(uindex index) {
                return index < m_size;
            }

            void put(uindex index, const T& element) {
                if (validIndex(index))
                    m_elements[index] = element;
            }

            T operator [](uindex index) {
                return m_elements[index];
            }

            T* operator ()(uindex index) {
                if (validIndex(index))
                    return &m_elements[index];

                return nullptr;
            }

            T& operator ()(uindex index, const T& fallback) {
                if (validIndex(index))
                    return m_elements[index];

                return fallback;
            }
        };

        template <typename T>
        struct List {
            protected:
                T * data { nullptr };
                usize capacity {0}, inicap {0};
                uindex index {0};

            public:
                List(usize capacity = 16) : data(nullptr), inicap(capacity), index(0) {
                    std::cout << "------- new list: " << capacity << std::endl;
                    grow();
                    std::cout << "------- \\new list" << std::endl;
                }
                /*
                List(std::initializer_list<T>& values) : inicap(values.size()), index(0) {
                    grow();

                    for (const T& v : values) {
                        data[index++] = v;
                    }
                }
                */
                virtual ~List() {
                    std::cout << "1)" << std::endl;
                    
                    if (data) {
                        std::cout << "2) data: " << data << std::endl;

                        for (int i = 0; i < index; i++) {
                            data[i].~T();
                        }

                        std::cout << "3)" << std::endl;

                        delete[] data;
                    }

                    std::cout << "4)" << std::endl;

                    data = nullptr;
                }

                bool grow() {
                    try {
                        T * ndata = new T[capacity + inicap];

                        for (uindex i = 0; i < index; i++) {
                            data[i].~T();
                            std::cout << "called ~T() for index " << i << std::endl;

                            ndata[i] = data[i];
                        }

                        std::cout << "data? " << (data != nullptr) << " -> " << data << std::endl;

                        // free old data
                        if (data)
                            delete[] data;

                        capacity = capacity + inicap;
                        data = ndata;

                        std::cout << "data=" << data << " cap=" << capacity << " index=" << index << std::endl;
                    } catch (...) {
                        data = nullptr;
                        throw MSException(MS_FATAL, "failed to grow List");
                    }

                    return true;
                }

                void clear() {
                    for (int i = 0; i < index; i++)
                        data[i].~T();

                    index = 0;
                }

                bool set(int index, const T& value) {
                    if (index >= 0 && index < capacity) {
                        data[index].~T();
                        data[index] = value;

                        return true;
                    }
                        
                    return false;
                }

                bool add(const T& value) {
                    if (isFull() && !grow())
                        return false;

                    data[index++] = value;
                    
                    return true;
                }

                bool insert(int index, const T& value) {
                    if (index < 0 || index >= capacity)
                        return false;
                    
                    if (isFull() && !grow())
                        return false;
                    
                    for (int i = this->index; i > index; i--)
                        data[i] = data[i - 1];

                    data[index] = value;
                    this->index++;

                    return true;
                }

                bool remove(int index, state (*removeOp)(T*) = MS_SUCCESS) {
                    if (index < 0 || index > this->index)
                        return false;

                    state r = removeOp(&data[index]);
                    
                    for (int i = index; i < this->index - 1; i++)
                        data[i] = data[i + 1];

                    this->index--;

                    return (r == MS_SUCCESS);
                }

                T * get(int index) const {
                    if (index >= 0 && index < this->index)
                        return &data[index];

                    return nullptr;
                }

                T& get(int index, const T& fallback) const {
                    if (index >= 0 && index < this->index)
                        return data[index];

                    return const_cast<T&>(fallback);
                }

                T * begin() const {
                    return data;
                }

                T * end() const {
                    return data + index;
                }

                friend std::ostream& operator <<(std::ostream& stream, const List<T>& l) {
                    stream << '[';

                    for (int i = 0; i < l.index; i++) {
                        if (l.get(i)) {
                            stream << *l.get(i);

                            if ((i + 1) != l.index)
                                stream << ',';
                        }
                    }

                    return stream << ']';
                }

                bool isFull() const {
                    return index == capacity;
                }

                bool isEmpty() const {
                    return index == 0;
                }

                usize getCapacity() const {
                    return capacity;
                }

                usize size() const {
                    return index;
                }

                T * getData() const {
                    return data;
                }
        };

        template <typename T>
        struct Container {
            protected:
                usize size_;
                usize capacity_;

            public:
                Container(usize size = 0, usize capacity = 0) : size_(size), capacity_(capacity) {}

                //virtual void fill(const T& value) = 0;

                usize getSize() const {
                    return size_;
                }

                usize getCapactity() const {
                    return capacity_;
                }
        };

        template <typename T>
        struct Stack {
            private:
                T * data { nullptr };
                int index;
                usize capacity;
                usize inicap;

            public:
                Stack(usize capacity = 16) : data(nullptr), index(-1) {
                    if (capacity < 16)
                        this->capacity = capacity;
                    else
                        this->capacity = 16;

                    init();
                }
                Stack(const Stack<T>& another) { *this = another; }
                ~Stack() {
                    if (data)
                        delete[] data;

                    data = nullptr;
                }

                Stack& operator =(const Stack<T>& another) {
                    if (this != &another) {
                        if (data)
                            delete[] data;

                        data = new T[another.capacity];

                        for (int i = 0; i <= another.index; i++)
                            data[i] = another.data[i];

                        capacity = another.capacity;
                        inicap = another.inicap;
                        index = another.index;
                    }

                    return *this;
                }

                bool init() {
                    inicap = capacity;

                    try {
                        if (data)
                            delete[] data;
                        
                        data = new T[capacity];
                    } catch(...) {
                        return false;
                    }

                    return true;
                }

                bool mustGrow() const {
                    return index == (capacity - 1);
                }

                bool grow() {
                    usize ncap = capacity + inicap;
                    
                    try {
                        T * ndata = new T[ncap];

                        for (int i = 0; i <= index; i++)
                            ndata[i] = data[i];

                        // free old data
                        delete[] data;

                        capacity = ncap;
                        data = ndata;
                    } catch(...) {
                        return false;
                    }

                    return true;
                }

                bool canPop() const {
                    return index > -1;
                }

                bool push(const T& element) {
                    if (mustGrow() && !grow())
                        return false;

                    data[++index] = element;

                    return true;
                }

                bool pushValue(T element) {
                    if (mustGrow() && !grow())
                        return false;

                    data[++index] = element;

                    return true;
                }

                bool operator <<(const T& element) {
                    return push(element);
                }

                T * get(uindex i) const {
                    if (i != -1 && i <= index)
                        return &data[i];

                    return nullptr;
                }

                T * operator [](uindex i) const {
                    return get(i);
                }

                T * top() const {
                    if (index == -1)
                        return nullptr;

                    return &data[index];
                }

                T * peek() const {
                    return top();
                }

                T& pop(const T& fallback) {
                    if (index == -1)
                        return const_cast<T&>(fallback);

                    return data[index--];
                }

                T popAndClear(const T& clear, const T& fallback) {
                    if (index == -1)
                        return const_cast<T>(fallback);
                    
                    T val = data[index];
                    data[index--] = clear;

                    return val;
                }

                T * popSafe() {
                    if (index == -1)
                        return nullptr;

                    return &data[index--];
                }

                T* begin() const {
                    return data;
                }

                T* end() const {
                    return data + capacity;
                }

                usize size() const {
                    return index + 1;
                }

                usize getCapacity() const {
                    return capacity;
                }

                uindex currentIndex() const {
                    return index;
                }
                
        };

        template <typename T>
        class Queue {
            protected:
                T * data {nullptr};

                int capacity {0}, inicap {0};
                int in {0}, out {-1};

            public:
                Queue(usize initialSize = 16) : inicap(initialSize) {
                    capacity = inicap;
                    data = new T[capacity];
                }
                virtual ~Queue() {
                    cleanUp([](int i, T& d) -> void {});

                    if (data)
                        delete[] data;

                    data = nullptr;
                }

                void resize(int nsize = 0) {
                    if (nsize == 0)
                        nsize = capacity + inicap;
                    
                    if (nsize <= capacity || !data)
                        return;

                    try {
                        T * ndata = new T[nsize];

                        for (int i = 0; i < capacity; i++)
                            ndata[i] = data[i];

                        delete[] data;

                        data = ndata;
                        capacity = nsize;

                        /* if the free space is between in and out,
                         * a resized array has more space on the end
                         * and thus the 'out' bounding must adjust too
                         */
                        if (in < out)
                            out += nsize;
                    } catch (...) {
                        throw MSException(MS_FATAL, "failed to resize queue");
                    }
                }

                bool push(const T& value) {
                    if (!canPush())
                        return false;

                    if (in == capacity)
                        in = 0;

                    data[in++] = value;

                    return true;
                }

                T * pop() {
                    if (!canPop())
                        return nullptr;

                    if (out == capacity)
                        out = -1;

                    return &data[++out];
                }

                void forAll(void (*f)(int, T&)) {
                    if (!data)
                        return;

                    for (int i = 0; i < capacity; i++) {
                        f(i, data[i]);
                    }
                }

                void cleanUp(void (*c)(int, T&)) {
                    if (!data)
                        return;

                    for (int i = 0; i < capacity; i++) {
                        c(i, data[i]);
                    }

                    in = 0;
                    out = -1;
                }

                bool canPush() const {
                    if (in > out)
                        return (in < capacity) || (out > 0);

                    return in != out;
                }

                bool canPop() const {
                    if (out > in)
                        return (out < capacity) || (in > 0);

                    return out < (in - 1);
                }

                int size() const {
                    if (in > out)
                        return in - out - 1;

                    return in + (capacity - out);
                }

                int getCapacity() const {
                    return capacity;
                }

                bool isIndexSet(int index) const {
                    if (in < out)
                        return index < in || index > out;

                    return index > out && index < in;
                }

                friend std::ostream& operator <<(std::ostream& stream, const Queue<T>& queue) {
                    stream << "{capacity: " << queue.capacity << ", size: " << queue.size() << ", in: " << queue.in << ", out: " << queue.out << ", data: [";

                    for (int i = 0; i < queue.capacity; i++) {
                        if (queue.isIndexSet(i))
                            stream << queue.data[i] << ',';
                    }

                    return stream << "]}";
                }

        };

        /*
        template <typename K, typename V>
        struct Map {
            struct MapPair {
                size_t key;
                V value;

                MapPair (size_t hash, const V& init) : key(hash), value(init) {}

                MapPair& operator =(const MapPair& p) {
                    if (this != &p) {
                        key = p;
                        value = p;
                    }

                    return *this;
                }
            };

            private:
                MapPair * data;
                usize capacity;
                usize size;
                index offset;

            public:
                Map (usize capacity = 16) : data(nullptr), size(0), offset(-1) {
                    this->capacity = capacity;
                    setup(capacity >= 16 ? 16 : capacity);
                }
                Map (usize capacity, const std::vector<Pair<K, V>>& init) {
                    usize counter = 0;

                    for (Pair<K, V>& pair : init) {
                        if (counter == capacity)
                            break;

                        put(pair);

                        counter++;
                    }

                    size = counter;
                }

                bool setup(int size) {
                    if (size >= capacity)
                        return false;
                    
                    if (data)
                        delete[] data;

                    try {
                        data = new V[size];
                    } catch (...) {
                        return false;
                    }

                    return true;
                }

                bool grow(usize newSize) {
                    if (newSize <= capacity) {
                        V * alloc = nullptr;
                        try {
                            alloc = new V[newSize];
                        } catch (...) {
                            return false;
                        }

                        for (uindex i = 0; offset != 0 && i < offset; i++)
                            alloc[i] = data[i];

                        delete[] data;
                        data = alloc;

                        return true;
                    }

                    return false;
                }

                bool grow() {
                    return grow(size / 2); // add half the size to the size
                }

                bool canPut() const {
                    return offset < size;
                }

                size_t keyHash(const K& key) const {
                    return std::hash<K>{}(key);
                }

                index indexOf(const K& key) const {
                    return keyHash(key) % size;
                }

                MapPair * put(const Pair<K, V>& data) {
                    if (!canPut() && !grow())
                        return nullptr;

                    size_t hash = keyHash(data.a);
                    data[hash % size] = {hash, data.b};
                    
                    return &data[offset];
                }

                V * put(const K& key, const V& value) {
                    if (!canPut() && !grow())
                        return nullptr;

                    size_t hash = keyHash(key);
                    data[hash % size] = {keyHash(key), value};

                    return &data[offset].value;
                }

                void remove(const K& key) {
                    size_t hash = keyHash(key);

                    for (size_t i = 0; i < data.size(); i++) {
                        MapPair& pair = data[i];

                        if (pair.key == hash) {
                            data.erase(data.begin() + i);

                            return;
                        }
                    }
                }

                MapPair * getPair(const K& key) {
                    size_t hash = keyHash(key);

                    for (MapPair& pair : data) {
                        if (pair.key == hash)
                            return &pair;
                    }

                    return nullptr;
                }

                bool containsKey(const K& key) {
                    return getPair(key) != nullptr;
                }

                V * get(const K& key) {
                    MapPair * pair = getPair(key);

                    if (pair)
                        return &(pair->value);

                    return nullptr;
                }

                V getUnsafe(const K& key) {
                    MapPair * pair = getPair(key);

                    if (pair)
                        return pair->value;

                    return V();
                }

                V& get(const K& key, const V& fallback) {
                    MapPair * pair = getPair(key);

                    if (pair)
                        return pair->value;

                    return const_cast<V&>(fallback);
                }

                index indexOf(const K& key) {
                    size_t hash = keyHash(key);

                    //TODO: use direct index, since hash

                    for (uindex i = 0; i < data.size(); i++) {
                        if (data[i].key == hash)
                            return i;
                    }

                    return -1;
                }

                std::string toString() const {
                    std::stringstream buffer;

                    std::cout << "Map::toString() @ size=" << data.size() << std::endl;

                    for (int i = 0; i < data.size(); i++) {
                        buffer << data[i].key << ": " << data[i].value;
                        
                        if (i < (data.size() - 1))
                            buffer << ", ";
                    }

                    return buffer.str();
                }

                usize capacity() const {
                    return capacity;
                }

                usize size() const {
                    return size;
                }

                index offset() const {
                    return offset;
                }

                bool empty() const {
                    return offset == -1;
                }
        };
        */

        template <typename K, typename V>
        struct Map {
            struct MapPair {
                size_t key;
                V value;
                bool unset { true };

                MapPair() : key(0) {}
                MapPair(size_t hash, const V &init) : key(hash), value(init), unset(false) {
                }

                MapPair &operator=(const MapPair &p) {
                    if (this != &p) {
                        key = p.key;
                        value = p.value;
                        unset = p.unset;
                    }

                    return *this;
                }
            };

        private:
            MapPair * data { nullptr };
            usize capacity, inicap;
            int count;

        public:
            Map(usize initSize = 16) : data(nullptr), inicap(initSize), capacity(0), count(0) {
                grow(inicap);
            }
            Map(const std::vector<Pair<K, V>> &init) : data(nullptr), capacity(0), inicap(init.size()), count(0) {
                grow(inicap);

                for (const Pair<K, V>& pair : init) {
                    putPair(pair);
                }
            }
            ~Map() {
                if (data)
                    delete[] data;
                
                data = nullptr;
            }

            Map<K, V>& operator =(const Map<K, V>& another) = default; /*{
                if (this != &another) {
                    if (data)
                        delete[] data;

                    capacity = another.capacity;
                    inicap = another.inicap;
                    count = another.count;
                    data = new MapPair[capacity];

                    for (int i = 0; i < capacity; i++)
                        this->data[i] = another.data[i];
                }

                return *this;
            }*/

            bool grow(usize size) {
                if (size <= capacity)
                    return false;

                try {
                    MapPair * data = new MapPair[size];

                    for (int i = 0; i < size; i++) {
                        if (i < capacity)
                            data[i] = this->data[i];
                        else
                            data[i] = {};
                    }

                    if (this->data)
                        delete[] this->data;

                    capacity = size;
                    this->data = data;
                } catch (...) {
                    throw MSException(MS_FATAL, "failed to grow map to size " + std::to_string(size));
                }

                return true;
            }

            bool grow() {
                return grow(capacity + inicap);
            }

            bool canPut() const {
                return count < capacity;
            }

            size_t keyHash(const K &key) const {
                return std::hash<K>{}(key);
            }

            int indexOf(size_t hash, bool nextFree = false) const {
                if (capacity == 0)
                    return -1;
                
                int pos = hash % capacity;
                int it = 0;

                while (data[pos].key != hash) {
                    if (nextFree && data[pos].unset)
                        return pos;

                    if (it++ == capacity)
                        return -1;

                    pos = (pos + 1) % capacity;
                }

                // ignored removed values
                // (since they are only marked as 'unset' and not deleted)
                if (data[pos].unset && !nextFree)
                    return -1;

                return pos;
            }

            // search for existing entry
            int nextEmpty(size_t hash) const {
                return indexOf(hash, true);
            }

            bool containsKey(const K &key) const {
                return indexOf(keyHash(key)) != -1;
            }

            MapPair * putPair(const Pair<K, V> &data) {
                if (!canPut() && !grow())
                    return nullptr;

                size_t hash = keyHash(data.a);
                int pos = nextEmpty(hash);

                // if the key is unknown
                if (pos == -1)
                    pos = indexOf(hash, true);

                // if there is new new free index
                if (pos == -1)
                    return nullptr;

                data[pos] = MapPair(hash, data.b);
                count++;

                return &data[pos];
            }

            V * put(const K &key, const V &value) {
                if (!canPut() && !grow())
                    return nullptr;

                size_t hash = keyHash(key);
                int pos = indexOf(hash);

                // if the key is unknown
                if (pos == -1)
                    pos = indexOf(hash, true);

                // if there is new new free index
                if (pos == -1)
                    return nullptr;

                data[pos] = MapPair(hash, value);
                count++;

                return &data[pos].value;
            }

            MapPair * putx(const K& key, const V& value) {
                if (!canPut() && !grow())
                    return nullptr;

                size_t hash = keyHash(key);
                int pos = indexOf(hash);

                // if the key is unknown
                if (pos == -1)
                    pos = indexOf(hash, true);

                // if there is new new free index
                if (pos == -1)
                    return nullptr;

                data[pos] = MapPair(hash, value);
                count++;

                return &data[pos];
            }

            bool remove(const K& key) {
                int pos = indexOf(keyHash(key));

                if (pos != -1) {
                    data[pos].unset = true;
                    count--;

                    return true;
                }

                return false;
            }

            MapPair * getPair(int index) const {
                if (index < 0 || index >= capacity)
                    return nullptr;

                return &data[index];
            }

            MapPair * getPairByValue(const V& value) {
                for (int i = 0; i < capacity; i++) {
                    if (!data[i]->unset && data[i]->value == value)
                        return &data[i];
                }

                return nullptr;
            }

            V& get(const K& key, const V& fallback) const {
                int pos = indexOf(keyHash(key));

                if (pos == -1)
                    return const_cast<V&>(fallback);

                return data[pos].value;
            }

            V * getSafe(const K& key) const {
                int pos = indexOf(keyHash(key));

                if (pos == -1)
                    return nullptr;

                return &data[pos].value;
            }

            friend std::ostream& operator <<(std::ostream& stream, const Map<K, V>& map) {
                stream << "{size: " << map.size() << ", capacity: " << map.getCapacity();
                stream << ", elements: {";

                for (int i = 0; i < map.getCapacity(); i++) {
                    if (!map.data[i].unset)
                        stream << map.data[i].key % map.size() << ": " << traits::stringify::toString(map.data[i].value) << ", ";
                }

                return stream << "}}";
            }

            std::string toString() const {
                std::stringstream buf;

                buf << *this;

                return buf.str();
            }

            MapPair * begin() const {
                return data;
            }

            MapPair * end() const {
                return data + capacity;
            }

            usize getCapacity() const {
                return capacity;
            }

            usize size() const {
                return count;
            }

            bool empty() const {
                return count == 0;
            }
        };

        template <typename K, typename V>
        class XMap {
            using const_iterator = typename std::map<K, V>::const_iterator;

            protected:
                std::map<K, V> map;

            public:
                XMap() {}

                void put(const K& key, V&& value) {
                    map.insert(std::make_pair(key, value));
                }

                void put(const K& key, const V& value) {
                    map.insert(std::make_pair(key, value));
                }

                void remove(const K& key) {
                    map.erase(map.find(key));
                }

                void clear() {
                    map.clear();
                }

                V& get(const K& key) {
                    return map.at(key);
                }

                V& get(const K& key, const V& fallback) const {
                    if (contains(key))
                        return map.at(key);

                    return const_cast<V>(fallback);
                }

                int indexOf(const K& key) const {
                    int index = map.find(key);

                    if (index == map.end())
                        return -1;

                    return index;
                }

                const_iterator begin() const {
                    return map.begin();
                }

                const_iterator end() const {
                    return map.end();
                }

                bool contains(const K& key) const {
                    return map.find(key) != map.end();
                }

                int size() const {
                    return map.size();
                }
        };

        // TODO: max attribute
        template <typename T>
        struct Sequence {
            private:
                usize inicap;
                usize capacity;

                T * data;
                
                int index;
                int read;

                bool init() {
                    try {
                        if (data)
                            delete[] data;

                        data = new T[capacity];
                    } catch(...) {
                        throw "failed to init sequence";
                        return false;
                    }

                    return true;
                }

            public:
                Sequence(usize capacity = 16) : data(nullptr), index(-1), read(-1) {
                    this->capacity = capacity;

                    // set this for gradual growth factor
                    inicap = this->capacity;
                    
                    init();
                }
                Sequence(const Sequence<T>& another) { *this = another; }
                ~Sequence() {
                    if (data)
                        delete[] data;
                    
                    data = nullptr;
                    capacity = inicap;
                    index = -1;
                    read = -1;
                }

                Sequence& operator =(const Sequence<T>& another) {
                    if (this != &another) {
                        try {
                            if (data)
                                delete[] data;

                            data = new T[another.capacity];

                            for (int i = 0; i < another.size(); i++)
                                data[i] = another.data[i];

                            inicap = another.inicap;
                            capacity = another.capacity;
                            index = another.index;
                            read = another.read;
                        } catch(...) {}
                    }

                    return *this;
                }

                bool grow() {
                    int ncap = capacity + inicap;
                    
                    try {
                        T * ndata = new T[ncap];

                        for (int i = 0; i <= index; i++)
                            ndata[i] = data[i];

                        // free old data
                        delete[] data;

                        data = ndata;
                        capacity = ncap;
                    } catch (...) {
                        return false;
                    }

                    return true;
                }

                bool insert(int index, const T& element) {
                    if (validIndex(index)) {
                        if (mustGrow() && !grow())
                            return false;

                        for (int i = index; i <= this->index; i++)
                            data[i + 1] = data[i];

                        data[index] = element;

                        return true;
                    }

                    return false;
                }

                bool putFirst(const T& element) {
                    return insert(0, element);
                }

                bool putLast(const T& element) {
                    if (mustGrow() && !grow())
                        return false;
                    
                    data[++index] = element;

                    return true;
                }

                bool append(const T& element) {
                    return putLast(element);
                }

                bool operator <<(const T& element) {
                    return putLast(element);
                }

                T * get(uindex i) const {
                    if (!validIndex(i) || i > index)
                        return nullptr;

                    return &data[i];
                }

                T * operator [](uindex i) const {
                    return get(i);
                }

                T * current() const {
                    if (read == -1)
                        return nullptr;
                    
                    return &data[read];
                }

                T * next() {
                    if (validIndex(read + 1) && (read + 1) < index)
                        return &data[read++];

                    return nullptr;
                }

                T * operator ++() {
                    return next();
                }

                T * previous() {
                    if (validIndex(read - 1) && read >= 0)
                        return &data[read--];

                    return nullptr;
                }

                T * begin() const {
                    return data;
                }

                T * end() const {
                    return data + index + 1;
                }

                bool mustGrow() const {
                    return index == (capacity - 1);
                }

                bool isEmpty() const {
                    return index == -1;
                }

                bool canRead() const {
                    return read >= -1 && read <= index;
                }

                bool validIndex(int pos) const {
                    return pos >= 0 && pos < capacity;
                }

                usize getCapacity() const {
                    return capacity;
                }

                usize size() const {
                    return index + 1;
                }

                int currentIndex() const {
                    return index;
                }

                usize leftRead() const {
                    return index - read;
                }
        };

        class StringBuffer {
            public:
                StringBuffer() : internalBuffer() {}
                StringBuffer(const std::string& base) {
                    for (size_t i = 0; i < base.length(); i++)
                        internalBuffer.push_back(base[i]);
                }

                inline std::string fuse() {
                    return std::string(internalBuffer.begin(), internalBuffer.end());
                }

                inline const char * fuse_c() {
                    return fuse().c_str();
                }

                void push(char c) {
                    internalBuffer.push_back(c);
                }

                void push(const std::string& s) {
                    for (char c : s)
                        internalBuffer.push_back(c);
                }

                inline void reset() {
                    internalBuffer.clear();
                }

                inline usize len() {
                    return internalBuffer.size();
                }

            private:
                std::vector<char> internalBuffer;
        };

        struct Range {
            long long from;
            long long to;
            
            Range(long long start, long long end) : from(start), to(end) {}
            Range(long long pos) : Range(pos, pos) {}
            Range() : Range(0, 0) {}

            bool contains(long long index) {
                return index >= from && index <= to;
            }

            long long length() {
                return to < from ? (from - to) : to - from;
            }
        };

        enum class Direction {
            FORWARD,
            BACKWARD
        };

        template <typename T>
        struct Optional {
            public:
                using remove = bool (*)(T&);

            private:
                constexpr static remove defaultRemoveOp = [](T& elem) -> bool { return true; };

            protected:
                T val;
                bool active {false};
                const remove removeOp = [](T& elem) -> bool { return true; };

            public:
                Optional() : active(false) {}
                Optional(const T& value, remove defaultRemove = defaultRemoveOp) : val(value), active(true) {}
                ~Optional() {
                    removeOp(val);

                    active = false;
                }

                bool set(const T& value, remove clearOp = true) {
                    if (isset() && !clearOp(val))
                        return false;

                    active = true;
                    val = value;

                    return true;
                }

                void clear(remove clearOp = true) {
                    if (isset() && !clearOp(val))
                        return;

                    active = false;
                }

                T& value(const T& fallback) const {
                    if (isempty())
                        return const_cast<T&>(fallback);

                    return const_cast<T&>(val);
                }

                bool isset() const {
                    return active;
                }

                bool isempty() const {
                    return !active;
                }
        };

        template <typename container, typename entry, typename X = std::enable_if_t<std::is_enum_v<entry>>>
        struct XMods {
            container state {0};
            Map<entry, std::string> stringMap;

            XMods() {}
            XMods(container initState) : state(initState) {}
            XMods(std::initializer_list<entry> enabled) {
                for (const entry& mod : enabled) {
                    enable(mod);
                }
            }

            XMods<container, entry>& operator =(const XMods<container, entry>& another) {
                if (&another != this) {
                    state = another.state;
                }

                return *this;
            }

            bool operator ==(container s) const {
                return state == s;
            }

            bool operator ==(XMods& another) const {
                return state == another.state;
            }

            void enable(entry mod) {
                state |= static_cast<container>(mod);
            }

            void disable(entry mod) {
                state &= ~static_cast<container>(mod);
            }

            void toggle(entry mod) {
                if (isset(mod))
                    disable(mod);
                else
                    enable(mod);
            }

            void clear() {
                state = 0;
            }

            bool isset(entry mod) const {
                return (state & static_cast<container>(mod)) == static_cast<container>(mod);
            }

            int enabledCount() const {
                int c {0};

                for (int i = 1, s = 1; i < sizeof(container) * 8; i++, s <<= 1) {
                    if ((state & s) == s)
                        c++;
                }

                return c;
            }

            friend std::ostream& operator <<(std::ostream& stream, const XMods<container, entry>& mods) {
                for (int i = 1, s = 1; i < sizeof(container) * 8; i++, s <<= 1) {
                    if ((mods.state & s) != s)
                        continue;
                    
                    stream << s << " (" << mods.stringMap.get((entry) i, "-") << ")";

                    if (i < sizeof(container) * 8)
                        stream << ", ";
                }

                return stream;
            }
            
        };

        template <typename T>
        struct OnErrorDeleted {

            T* value {nullptr};
            bool success {false};

            OnErrorDeleted(T* init) : value(init) {}
            virtual ~OnErrorDeleted() {
                if (!success)
                    delete value;
            }

            T* operator ->() {
                return value;
            }

            T* release() {
                success = true;

                return value;
            }
        };

        // --- Util functions

        /* True for 0..9. */
        constexpr bool isDigit(const char c) {
            return c >= '0' && c <= '9';
        }

        /* Parses an integer of given base from a given string.
         * The input must be null-terminated with the format of
         * [0-9]*
         */
        template <size_t StringOffset = 0, typename StringContainer = std::string_view>
        constexpr int parseInt(const StringContainer& s, const int base = 10) {
            int i = 0, off = StringOffset, b = 1;

            while (s[off] && isDigit(s[off++]));

            for (--off; off >= 0; off--) {
                i += (s[off] - '0') * b;
                b *= base;
            }

            return i;
        }

        // returns true for a number of format [0-9]*.?[0-9]*
        // will always be false on '.' if not set decimal or more than one occurence
        bool isNumber(const std::string& str, bool decimal = false) {
            bool dotFound = false;
            bool negative = false;

            if (str.length() > 0 && str[0] == '-')
                negative = true;

            for (size_t i = 0; i < str.length(); i++) {
                if (str[i] == '.') {
                    if (!decimal)
                        return false;
                    
                    if (decimal && dotFound)
                        return false;

                    dotFound = true;
                } else if (!isDigit(str[i]) && !negative)
                    return false;
            }

            return true;
        }

        bool containsOnly(const std::string& str, std::vector<char> validCharacters) {
            for (size_t i = 0; i < str.length(); i++) {
                size_t miss = 0;

                for (char c : validCharacters) {
                    if (str[i] != c) {
                        miss++;

                        if (miss == validCharacters.size())
                            return false;
                    } else break;
                }
            }

            return true;
        }

        bool containsASCIIRangesOnly(const std::string& str, const std::vector<Range>& ranges) {
            for (size_t i = 0; i < str.length(); i++) {
                char c = str[i];

                for (Range r : ranges) {
                    if (!r.contains(c))
                        return false;
                }
            }

            return true;
        }

        std::vector<std::string> * split(const std::string& str, char splitToken) {
            std::vector<std::string> * split = new std::vector<std::string>();
            size_t anchor = 0;

            for (size_t i = 0; i < str.size(); i++) {
                char c = str.at(i);

                if (c == splitToken) {
                    split->push_back(str.substr(anchor, i - anchor));

                    anchor = i;
                }
            }

            return split;
        }

        std::string getFileName(const std::string& fullPath) {
            uindex lastSlash = fullPath.find_last_of('/');
            uindex dot = fullPath.find_first_of('.');

            return fullPath.substr(lastSlash + 1, dot - lastSlash - 1);
        }

        std::string toBinary(int in, int bits = -1) {
            std::stringstream buf;

            bits = (bits == - 1) ? sizeof(int) : bits;

            for (int i = bits - 1; i >= 0; i--) {
                if (((in >> i) & 1) == 1)
                    buf << "1";
                else buf << "0";
            }

            return buf.str();
        }

        std::string toUpperCase(const std::string_view str) {
            static constexpr int min {(int) 'a'};
            static constexpr int max {(int) 'z'};
            static constexpr int dist { (int) 'A' - min };

            std::string val(str.length(), '?');

            for (int i = 0; i < str.length(); i++) {
                if (max >= str[i] && min <= str[i])
                    val[i] = str[i] + dist;
                else val[i] = str[i];
            }

            return val;
        }

    }

    MS_VALUE_MAP(types::state, std::string) stateCodes {
        {MS_FATAL, "Fatal"},
        {MS_ERROR, "Error"},
        {MS_SUCCESS, "Success"},
        {MS_FAIL, "Fail"},
        {MS_ERROR_INTERNAL, "Internal"},

        {MS_ERROR_NULL, "Null"},
        {MS_ERROR_INVALID_DECIMAL_FORMAT, "Invalid Decimal Format"},
        {MS_ERROR_EMPTY_STRING_LITERAL, "Empty String"},
        {MS_ERROR_INVALID_OPERAND_TYPES, "Invalid Operand Types"}
    };
    MS_MAP_LOOKUP(getStateCode, stateCodes, types::state, std::string, std::to_string(key))

    // -- Declarations

    namespace lang {

        bool isValidIdentifier(const std::string& identifier);

        class TypeSpec;

        class Entity;
        class Namespace;
        class Module;
        
        struct Object;
        struct Proto;

    }

    namespace ext {
    
        class Call;
        class ExtLink;
        class ExtHandler;

    }

    namespace comp {

        class Context;
        
    }

    namespace runtime {

        class VM;

    }

    // -- Definition

    namespace opcode {

        using namespace types;
        using namespace util;
        using namespace lang;

        // -- OP CODE Specification

        /* Note:
         *  Some operations have multiple variations. By passing
         *  a variable amount of parameters activates different
         *  instructions. See the definition below:
         * 
         *  -> op [target] [left] [right]
         * 
         *  Operations which require more parameters than given
         *  will probably use pop() or other sources.
         *  
         */

        enum Op : operation {

            NOP = 0, // no operation

            // UTIL & MISC
            debug,
            cmd, // easy command transmission; 1+ params: 0 - command, ... for the command arguments

            // ARITHMETIC
            add, // $stack -> a; $stack -> b; $stack <- a + b
            add_t, // add + save target; [param_0] := $-0 + $-1
            sub,
            mul,
            div,

            // LOGICAL
            cmp, // compares two values; pop a, pop b -> C::compare(a, b) -> push result
            negate, // negates peek stack value

            jmp,
            jnz, // jump to operation if result is not 0; 1 param: 0 - target operation; -> pop for cmp result
            jz, // jump to operation if result is 0; 1 param: 0 - target operation; -> pop for cmp result
            jgz, // jump greater than: jump when cmp result is > 0; 1 param: 0 - target operation; -> pop for cmp result
            jgez, // jump greater or equals: jump when cmp result is >= 0; 1 param: 0 - target operation; -> pop for cmp result
            jlz, // jump less than: jump when cmp result is < 0; 1 param: 0 - target operation; -> pop for cmp result
            jlez, // jump less or equals: jump when cmp result is <= 0; 1 param: 0 - target operation; -> pop for cmp result

            // Path and special push operations
            pattr, // PUSH ATTRIBUTE: pattr <STRING name> - pushes the reference to an attribute inside an object
            pindx, // PUSH INDEX: pindx - (POP index) > pushes the reference to a data cell inside a container (arrays)
            pgv, // PUSH global REFERENCE: pref <INT address> - pushes an IData-Object onto the stack to be used for further path operattions
            plref, // PUSH local REFERENCE: (see pref) using addresses in current stack frame
            plit, // PUSH LITERAL: plit <index> - 

            as, // ASSIGN STACK: assigns value from stack to object reference on stack: val = POP, obj = POP; obj := val
            alv, // ASSIGN local VALUE: alv <address>: val = POP; $frame$.mem[address] = val
            agv, // ASSIGN global VALUE: agn <address>: val = POP; mem[address] = val
            agv_t,

            // ARRAYS
            collect, // collect <count> -- Creates a Container of 'count' size. Gets filled with top 'count' op-stack values. Then the Container gets pushed.

            acreate, // array create; 2 params: 0 - size, 1 - element type id; push reference
            aset, // sets an element in an array; 3 params: 0 - array reference, 1 - index, 2 - element reference OR copy
            anil, // sets an element to nil in array; 2 params: 0 - array reference, 1 - index
            adel, // deletes an array; 1 param: 0 - array reference

            // DATA
            paste, // assigns data to a memory address without popping; 1 param: 0 - target cell; top -> cell
            move, // directly copies data from one memory cell to another; 2 params: 0 - source addres, 1 - destination address

            // REGISTERS & LOOP

            pop, // pop <register>; popps highest stack value into register
            push, // push <register>; pushes register value onto stack
            mov, // mov <register from> <register to>; moves register content
            load, // <register> := <address>
            store, // <address> := <register>

            repeat, // UNUSED -- repeat <index>; Sets loop_info[index] = current_op_index
            loop, // UNUSED -- jumps back to ...
            
            // FUNCTIONS
            extcall, // extern function call; 1 param "param count"
            call, // call function + USES 'putparam' + SAVES current operation index for 'ret'; 3 params: 0 - operation index, 1 - local memory size, 2 - param count
            fcall, // calls an RFunction: 1 param: id
            ret, // return to last address
            retc, // return to last address, copy last stack value

            // CONTROL
            END, // end of file
            LIS, // last operation
            IIS // invalid operation

        };

        // Name, Op-id, max parameters
        using OpInfo = Triplet<std::string, Op, usize>;

        static std::vector<OpInfo> operations {
            {"NOP", Op::NOP, 0},

            {"debug", Op::debug, 0},
            {"cmd", Op::cmd, 1},

            {"push", Op::push, 1},
            {"pop", Op::pop, 0},

            {"add", Op::add, 0},
            {"add_t", Op::add_t, 1},
            {"sub", Op::sub, 0},
            {"mul", Op::mul, 0},

            {"cmp", Op::cmp, 0},
            {"negate", Op::negate, 0},

            {"jmp", Op::jmp, 1},
            {"jnz", Op::jnz, 1},
            {"jz", Op::jz, 1},
            {"jgz", Op::jgz, 1},
            {"jgez", Op::jgez, 1},
            {"jlz", Op::jlz, 1},
            {"jlez", Op::jlez, 1},

            //{"assign", Op::assign, 1},
            {"paste", Op::paste, 1},
            {"move", Op::move, 2},

            {"extcall", Op::extcall, 2},
            {"call", Op::call, 3},
            {"ret", Op::ret, 0},
            {"retc", Op::retc, 0},

            {"pattr", Op::pattr, 0}, // no param is needed since only the var-arg is important
            {"pindx", Op::pindx, 0},
            {"pgv", Op::pgv, 1},
            {"plref", Op::plref, 1},
            {"plit", Op::plit, 1},

            {"as", Op::as, 0},
            {"alv", Op::alv, 1},
            {"agv", Op::agv, 1},
            {"agv_t", Op::agv_t, 2},

            {"collect", Op::collect, 1},

            {"END", Op::END, 0},
            {"LIS", Op::LIS, 0},
            {"IIS", Op::IIS, 0}
        };

        // -- Declarations

        static Op getOpByCode(const std::string& operation);
        static OpInfo getOpInfo(Op operation);
        static OpInfo getOpInfo(const std::string& operation);

        // -- Definitions

        struct Instruction {
            const static param INVALID_PARAM = 0xFFFF;

            private:
                /* Current ID of a usable label. */
                static int labelID;

                /* Label of this instruction serves as a placeholder when
                * absolute parameters are not known on first compile step. */
                int label {-1};

                /* Operation code interprted by the virtual machine. */
                operation op;

                /* Variable string argument. */
                std::string varg;

                /* A set of numeric values needed for certain operation codes.
                *
                *  NOTE: some operations may rely on placeholders (aka labels)
                *        which are encoded as numric values too. This value
                *        refers to the global label ID located in front of
                *        some instruction (see label).
                */
                std::vector<param> params;

                /* This field holds info which param is a label.
                * Each position is marked as a 1 at the respective bit position.
                */
                int labelData {0};

            private:
                void enableLabel(int position) {
                    labelData = labelData | (0x1 << position);
                }

            public:
                Instruction(operation code = Op::NOP) : op(code) {}
                Instruction(operation code, const std::vector<param>& params) : op(code), params(params) {}

                static int currentLabel() {
                    return labelID;
                }

                static int nextLabel() {
                    return ++labelID;
                }

                operator Op() const {
                    return (Op) op;
                }

                bool operator ==(Op code) const {
                    return op == code;
                }

                bool validParam(uindex i) const {
                    return i < params.size();
                }

                bool append(param p, bool isLabel = false) {
                    if (isLabel)
                        enableLabel(params.size());
                    
                    params.push_back(p);

                    return true;
                }

                param getParam(uindex i) const {
                    if (validParam(i))
                        return params.at(i);

                    return INVALID_PARAM;
                }

                bool setParam(uindex i, param p, bool isLabel = false) {
                    if (validParam(i)) {
                        if (isLabel)
                            enableLabel(i);
                        
                        params[i] = p;

                        return true;
                    }

                    return false;
                }

                void setLabel(int id) {
                    label = id;
                }

                void requestLabel() {
                    label = labelID++;
                }

                // determines if a certain param is a label
                bool isLabel(int position) const {
                    return (labelData & (0x1 << position)) > 0;
                }

                bool hasLabels() const {
                    return labelData > 0;
                }

                bool hasVARG() const {
                    return varg.length() > 0;
                }

                const std::string& getVARG() const {
                    return varg;
                }
                
                void setVARG(const std::string& arg) {
                    varg = arg;
                }

                operation getOp() const {
                    return op;
                }

                int getLabel() const {
                    return label;
                }

                size_t paramCount() const {
                    return params.size();
                }
        };

        /* InstructionLayers order instructions in way that nested functions won't
         * fill instructions for their parent functions. The same applies to module
         * level operations which will be put in the end.
         * 
         * Generally this functions puts the highest layers in the front and
         * lowest (module level) are in the end.
         * 
         * This effect is achieved through the 'appendLayerInstructions'-method in
         * the InstructionSet which serves as a layer-0 operator. Whenever the highest
         * layer is completed, its content gets written into the main instruction list.
         */
        struct InstructionLayer {
            InstructionLayer* higher {nullptr};
            InstructionLayer* lower {nullptr};

            std::vector<Instruction> layerInstructions;

            int level {0};
            bool module {false};
            Namespace* parent;
            std::string identifier {""};

            int index {0};
            int nextLabel {-1};
            bool applyLabel {false};

            InstructionLayer(Namespace* parentNamespace, const std::string& name = "") : parent(parentNamespace), identifier(name) {}

            // -- append

            virtual state append(const Instruction& instruction) {
                if (applyLabel) {
                    const_cast<Instruction&>(instruction).setLabel(nextLabel);

                    applyLabel = false;
                    nextLabel = -1;
                }

                MS_IF_DEBUG {
                    std::cout << "\033[35m<";
                    std::cout << identifier << "|" << level;
                    std::cout << ", " << layerInstructions.size();
                    std::cout << "> << " << getOpInfo((Op) instruction).a;

                    for (int i = 0; i < instruction.paramCount(); i++) {
                        std::cout << " ";

                        if (instruction.isLabel(i))
                            std::cout << "$";

                        std::cout << instruction.getParam(i);
                    }

                    if (instruction.hasVARG()) 
                        std::cout << "\033[32m '" << instruction.getVARG() << '\'';

                    if (instruction.getLabel() != -1)
                        std::cout << "\033[31m $" << instruction.getLabel();

                    std::cout << "\033[0m\n";
                }

                layerInstructions.push_back(instruction);
                
                return MS_SUCCESS;
            }

            virtual inline state append(operation op) {
                return append(static_cast<Instruction>(op));
            }

            virtual inline state append(operation op, param p0) {
                return append({op, {p0}});
            }

            virtual inline state append(operation op, param p0, param p1) {
                return append({op, {p0, p1}});
            }

            virtual inline state append(operation op, param p0, param p1, param p2) {
                return append({op, {p0, p1, p2}});
            }

            // -- label control

            virtual void placeLabelOnNextInstruction(int label) {
                nextLabel = label;
                applyLabel = true;
            }

        };

        struct InstructionSet : public InstructionLayer {
            InstructionLayer* currentLayer {nullptr};
            
            std::vector<Instruction> finalInstructions;
            std::map<int, int> resolvedLabels;
            int baseOffset {0};

            InstructionSet() : InstructionLayer(nullptr, "#") {}

            void resolveLabels() {
                for (int i = 0; i < finalInstructions.size(); i++) {
                    if (finalInstructions[i].getLabel() != -1)
                        resolvedLabels.insert(std::make_pair(finalInstructions[i].getLabel(), i));
                }
            }

            void resolveLabel(Instruction& i, int param) {
                int pos = -1;

                if (!i.isLabel(param))
                    return;

                if (resolvedLabels.find(i.getParam(param)) == resolvedLabels.end())
                    pos = finalInstructions.size();
                else
                    pos = resolvedLabels[i.getParam(param)];

                MS_IF_DEBUG {
                    std::cout << "resolved label for instruction (";
                    std::cout << getOpInfo((Op) i.getOp()).a;
                    std::cout << "): ";
                    std::cout << i.getParam(param);
                    std::cout << " -> ";
                    std::cout << pos;
                    std::cout << "\n";
                }

                i.setParam(param, pos);
            }

            void complete() {
                baseOffset = finalInstructions.size();

                // add the layer-0 instructions
                for (const Instruction& i : layerInstructions) {
                    finalInstructions.push_back(i);
                }

                resolveLabels();

                for (int i = 0; i < finalInstructions.size(); i++) {
                    if (finalInstructions[i].hasLabels()) {
                        for (int j = 0; j < finalInstructions[i].paramCount(); j++)
                            resolveLabel(finalInstructions[i], j);
                    }
                }

                finalInstructions.push_back(Op::LIS);
            }

            state enterLayer(Namespace* parent, const std::string& ownerName = "", bool module = false) {
                if (!currentLayer) {
                    currentLayer = new InstructionLayer(parent, ownerName);
                    currentLayer->level = 1;
                    currentLayer->module = module;
                    
                    return MS_SUCCESS;
                }

                InstructionLayer* higher = new InstructionLayer(parent, ownerName);
                higher->lower = currentLayer;
                higher->level = currentLayer->level + 1;
                higher->module = module;

                currentLayer->higher = higher;
                currentLayer = higher;

                return MS_SUCCESS;
            }

            state leaveLayer() {
                if (!currentLayer)
                    return MS_ERROR_ALREADY_BASE_INSTRUCTION_LEVEL;

                if (currentLayer->module) { // module level
                    appendLayerInstructions(finalInstructions);

                    MS_IF_DEBUG {
                        std::cout << "=> module layer: appending to final instructions -> size = " << finalInstructions.size() << '\n';
                    }
                } else {
                    // When we are in the main module, all layer instructions
                    // can be added to the final instruction set.
                    if (!currentLayer->lower)
                        appendLayerInstructions(finalInstructions);
                    else
                        appendLayerInstructions(currentLayer->lower->layerInstructions);
                }

                InstructionLayer* old = currentLayer;

                if (currentLayer->applyLabel) {
                    delete old;

                    return MS_ERROR_UNFINISHED_LABEL_APPLICATION;
                }

                // Unregister current layer as the higher from its parent (lower layer)
                if (currentLayer->lower)
                    currentLayer->lower->higher = nullptr;

                currentLayer = currentLayer->lower;
                delete old;

                if (currentLayer && currentLayer->higher)
                    return MS_ERROR_HIGHER_INSTRUCTION_LEVEL_EXISTS;

                return MS_SUCCESS;
            }

            void appendLayerInstructions(std::vector<Instruction>& target) {
                if (currentLayer) {
                    for (const Instruction& i : currentLayer->layerInstructions)
                        target.push_back(i);
                }
            }

            state appendJmp(Op whichJmp) {
                Instruction jmp(whichJmp);
                jmp.append(Instruction::nextLabel(), true);

                return append(jmp);
            }

            state append(const Instruction& instruction) {
                if (currentLayer)
                    return currentLayer->append(instruction);

                return InstructionLayer::append(instruction);
            }

            inline state append(operation op) {
                return append(static_cast<Instruction>(op));
            }

            inline state append(operation op, param p0) {
                return append({op, {p0}});
            }

            inline state append(operation op, param p0, param p1) {
                return append({op, {p0, p1}});
            }

            inline state append(operation op, param p0, param p1, param p2) {
                return append({op, {p0, p1, p2}});
            }

            void placeLabelOnNextInstruction(int label) {
                if (currentLayer)
                    currentLayer->placeLabelOnNextInstruction(label);
                else
                    InstructionLayer::placeLabelOnNextInstruction(label);
            }

        };

        // -- Static Variable Initializers

        int Instruction::labelID = -1;

        // -- Function

        static Op getOpByCode(const std::string& operation) {
            for (Triplet<std::string, Op, usize>& t : operations) {
                if (t.a == operation) {
                    return t.b;
                }
            }

            return Op::IIS;
        }

        static OpInfo getOpInfo(Op operation) {
            for (Triplet<std::string, Op, usize>& t : operations) {
                if (t.b == operation) {
                    return t;
                }
            }

            return Triplet<std::string, Op, usize>(std::string("N/A"), Op::IIS, (usize) 0);
        }

        static OpInfo getOpInfo(const std::string& operation) {
            for (Triplet<std::string, Op, usize>& t : operations) {
                if (t.a == operation) {
                    return t;
                }
            }

            return Triplet<std::string, Op, usize>(std::string("N/A"), Op::IIS, (usize) 0);
        }

    }

    namespace memory {

        using namespace types;
        using namespace traits;

        // -- Declarations

        class MemoryContainer;
        class IData;
        //struct ObjectData;
        
        state assignData(IData *& target, IData * source, bool copyModifiers = false, bool initTargetOnNull = true);
        void freeData(IData*& data, const MemoryContainer* owner = nullptr);
        std::string toString(IData * data, bool detailed = false); // calls traits::stringify:: ...

        // -- Static IDs

        static int objectCount {0};

        // -- Definitions

        class Allocator {

        };

        class MemoryContainer {
            public:
                std::string containerName;
                bool transient {false}; // True, if the container is only a temporary container (like registers)

            public:
                MemoryContainer(const std::string& name = "MemoryContainer") : containerName(name) {}

        };

        // -- Basic Type Definition

        struct IData {
            using ref_count = unsigned char;

            short type : 6;
            short modifiers : 10;

            MemoryContainer* owner {nullptr};
            ref_count references {0};

            #ifdef MS_TRACK_IDATA
                static inline int IDCNT {0};
                static inline int TOTAL {0};
                static inline int MAXCNT {0};

                int ID {0};
            #endif

            IData() : type(static_cast<short>(DataType::UNDEFINED)), modifiers(Modifiers(Modifier::NONE).data) {
                #ifdef MS_TRACK_IDATA
                    ID = IDCNT++;

                    if (++TOTAL > MAXCNT) MAXCNT = TOTAL;

                    MS_IF_DEBUG {
                        std::cout << "IData() -> " << ID << " (total " << TOTAL << ")\n";
                    }
                #endif
            }
            IData(const IData&) = delete;
            IData(const IData&&) = delete;
            virtual ~IData() {
                #ifdef MS_TRACK_IDATA
                    TOTAL--;

                    MS_IF_DEBUG {
                        std::cout << "~IData(";
                        std::cout << toString(this);
                        std::cout << ") \tin " << (owner ? owner->containerName : "-") << " \t| " << ID << " / " << (IDCNT - 1) << " (total " << TOTAL << ")\n";
                    }
                #endif
            }

            template <typename T>
            T& get();

            virtual IData* clone(bool copyModifiers = false) = 0;
            virtual IData* assign(IData*, bool copyModifiers = false) = 0;

            template <typename T>
            void operator =(const T&) = delete;

            template <typename T>
            void operator =(const T&&) = delete;

            IData* applyMods(Modifiers mods) {
                if (!isProtected())
                    modifiers |= mods.data;

                return this;
            }

            IData* disableMod(Modifier mod) {
                if (!isProtected())
                    modifiers &= static_cast<short>(~static_cast<int>(mod) & 0xFF);

                return this; 
            }

            void clearMods() {
                if (!isProtected())
                    modifiers = Modifiers(Modifier::NONE).data;
            }

            inline bool hasModifier(Modifier mod) const {
                return Modifiers::ispresent(modifiers, mod);
            }
            
            inline bool isConstant() const {
                return Modifiers::ispresent(modifiers, Modifier::CONST);
            }
            
            inline bool isProtected() const {
                return Modifiers::ispresent(modifiers, Modifier::PROTECTED);
            }

            inline bool isType(DataType type) const {
                return this->type == static_cast<short>(type);
            }

            DataType getType() const {
                return static_cast<DataType>(type);
            }
        };

        template <typename T, DataType D>
        struct Data : IData {
            using IData::type;
            using IData::modifiers;
            using type_name = Data<T, D>;

            T value {};

            explicit Data(T&& init) : value(init) { type = static_cast<short>(D); }
            Data(const T& init) : value(init) { type = static_cast<short>(D); }
            Data(const T& init, Modifiers mods) : value(init) { type = static_cast<short>(D); modifiers(mods.data); }
            ~Data() {}

            IData* clone(bool copyModifiers) {
                IData* data {nullptr};
                    
                try {
                    data = new Data<T, D> {value};

                    if (copyModifiers)
                        data->modifiers = modifiers;
                } catch (...) {
                    throw MSException(MS_ERROR_INTERNAL, "failed to clone Data");
                }

                return data;
            }

            IData* assign(IData* other, bool copyModifiers) {
                if (other && other->type == type) {
                    assign(static_cast<type_name*>(other)->value);

                    if (copyModifiers)
                        modifiers = other->modifiers;
                }

                return this;
            }

            // Overridden by Object, Proto, ... for handling internal heap objects
            virtual void assign(const T& value) {
                this->value = value;
            }
        };

        struct Integral : Data<int64, DataType::INTEGRAL> {
            Integral(int64 i) : Data(i) {}
        };

        struct Decimal : Data<double64, DataType::DECIMAL> {
            Decimal(double64 d) : Data(d) {}
        };

        struct String : Data<std::string, DataType::STRING> {
            String(const std::string& s) : Data(s) {}
        };

        struct Object : Data<usize, DataType::OBJECT> {
        };

        struct ProtoObject : Data<long long, DataType::PROTO> {
            // proto - id
        };

        struct NilData : Data<IData*, DataType::NIL> {
            NilData() : Data(nullptr) {}
            NilData(IData* nil);
            NilData(const NilData& other) : Data(other.value) {}
        };

        struct DataRef {
            private:
                IData* value {nullptr};

            public:
                explicit DataRef(IData* alloc) : value(alloc) {}

                IData* operator ->() {
                    return value;
                }

                DataType type() const {
                    return (value ? value->getType() : DataType::NIL);
                }
        };

        // -- Static Data Type Info

        template <typename T>
        struct DataInfo {
            using wrapper = T; // per default T for better info on compile-error
            using value_type = T;

            static constexpr inline DataType data_type { DataType::INVALID };
        };

        template <typename T>
        struct DataWrapper {
            using type = typename DataInfo<T>::wrapper;
        };

        MS_DATA_INFO_STRUCT(int64, Integral, DataType::INTEGRAL)
        MS_DATA_INFO_STRUCT(double64, Decimal, DataType::DECIMAL)
        MS_DATA_INFO_STRUCT(std::string, String, DataType::STRING)

        // -- Runtime Memory --

        // Memory for stack frames
        class LocalMemory : public MemoryContainer {
            protected:
                IData** data {nullptr};
                int capacity {0};
                int offset {0};

            public:
                LocalMemory(int size, const std::string& name = "LocalMemory") : MemoryContainer(name) {
                    allocate(size);
                }
                LocalMemory(const LocalMemory&) = delete;
                LocalMemory(const LocalMemory&&) = delete;

                virtual ~LocalMemory() {
                    free();
                }

                LocalMemory& operator =(const LocalMemory& mem) {
                    if (this != &mem) {
                        free();

                        offset = mem.offset;
                        capacity = mem.capacity;

                        try {
                            data = new IData*[capacity];
                        } catch (std::exception& e) {
                            throw e;
                        }

                        /* Copy all objects using 'assignData' to avoid illegal memory access
                         * if the data array in 'mem' gets deleted (do not just copy the pointers).
                         */
                        for (int i = 0; i < capacity; i++)
                            assignData(data[i], mem.data[i], true);
                    }

                    return *this;
                }

                void allocate(int size) {
                    if (size > capacity) {
                        try {
                            IData** ndata = new IData*[size];
                            int off {capacity};

                            if (capacity > 0) {
                                for (int i = 0; i < capacity; i++)
                                    ndata[i] = data[i];

                                delete[] data;
                            }

                            data = ndata;
                            capacity = size;

                            // Only init new cells to nullptr
                            for (int i = off; i < capacity; i++)
                                data[i] = nullptr;
                        } catch (std::exception& e) {
                            throw e;
                        }
                    }
                }

                bool allocate(IData* idata) {
                    if (!data || offset >= capacity)
                        return false;

                    data[offset] = idata;
                    data[offset]->owner = this;
                    offset = offset + 1;

                    return true;
                }

                state write(Address address, IData* idata, bool override = false) {
                    const auto emptyInit = [&]() {
                        // If the new data has a transient owner, copy the data and
                        // assign it to the cell. The value must be copied since
                        // we cannot override the internal data of the empty cell
                        // and must release the container reference.
                        if (idata->owner && idata->owner->transient)
                            idata = idata->clone();
                        
                        // Data with transient or no owner is now a value of this container.
                        if (!idata->owner)
                            idata->owner = this;
                        
                        data[address] = idata;
                        data[address]->modifiers = 0; // force reset modifiers
                    };

                    // Test for availability for the internal data buffer
                    if (!data || address >= capacity || address < 0)
                        return MS_EXCEPT_NULL; // TODO: MS_EXCEPT_NO_SPACE container has no allocated space (left)

                    // Nullifying a memory cell
                    if (!idata) {
                        freeData(data[address], this);
                        data[address] = nullptr;

                        return MS_SUCCESS;
                    }

                    // Internal moves are just references
                    if (idata->owner == this)
                        idata->references++;

                    // Overriding
                    if (data[address]) {
                        if (!override)
                            return MS_EXCEPT; // TODO: MS_EXCEPT_OVERRIDE

                        // Do not delete shared data like literals
                        //if (!data[address]->isProtected())
                        //    delete data[address];

                        // On a type mismatch the IData* will be exchanged,
                        // else an internal assignment (assignData) is chosen.
                        if (data[address]->getType() != idata->getType()) {
                            freeData(data[address], this);
                            emptyInit();

                            return MS_SUCCESS;
                        } else
                            return assignData(data[address], idata, false);
                    }

                    // Empty cell initialization
                    if (!data[address]) {
                        emptyInit();

                        return MS_SUCCESS;
                    }

                    return MS_SUCCESS;
                }

                template <typename T = IData>
                T* read(Address address) {
                    if (!data || address >= capacity || address < 0)
                        return nullptr;

                    return static_cast<T*>(data[address]);
                }

                // Only clears cell. Does NOT delete IData*.
                IData* clear(Address address) {
                    if (valid(address)) {
                        IData* d = data[address];

                        data[address] = nullptr;

                        return d;
                    }

                    return nullptr;
                }

                // Clears and frees the IData* stored in the cell.
                void free(Address address) {
                    if (valid(address) && data[address]) {
                        freeData(data[address], this);

                        data[address] = nullptr;
                    }
                }

                void free() {
                    if (data) {
                        for (int i = 0; i < capacity; i++) {
                            freeData(data[i], this);
                        }

                        delete[] data;
                        data = nullptr;
                    }

                    capacity = 0;
                    offset = 0;
                }

                // Resets offset to 0 but does NOT delete allocated memory.
                void clear() {
                    offset = 0;
                }

                inline bool valid(Address address) {
                    return address > 0 && address < capacity;
                }

                bool full() const {
                    return offset == capacity;
                }

                int size() const {
                    return offset;
                }

                int max() const {
                    return capacity;
                }
        };

        class StaticMemory {

        };

        // -- Static Variables

        /* Global and unique constant to identify runtime-nil objects.
         * Can be used to test for "IData* == NIL".
         */
        static IData* const NIL { new Data<void*, DataType::NIL>(nullptr) };

        // -- Member Functions

        NilData::NilData(IData* nil) : Data(nil) {
            if (nil != NIL)
                throw MSException(MS_FATAL, "NilData can only hold NIL or nullptr as the value");
        }

        /* Forwards the data extraction to the Data<D, T> implementation.
         * Can be used directly on the IData* but the wrapper type of the
         * Data<> wrapper 'W' must be equal to the data type set in the
         * IData::type field or else a MSException will be thrown.
         */
        template <typename T>
        T& IData::get() {
            using Info = DataInfo<T>;
            using Wrapper = typename Info::wrapper;

            if (this->type != (short) Info::data_type)
                throw MSException(MS_ERROR, "IData::get<T, W>: cannot cast " + traits::stringify::toString((DataType) this->type) + " to " + traits::stringify::toString((DataType) Info::data_type));

            return static_cast<Wrapper*>(this)->value;
        }

        // -- Utililty Functions

        template <typename T, typename W = typename DataInfo<T>::wrapper>
        auto wrapper(IData* obj) -> W* {
            return static_cast<W*>(obj);
        }

        template <typename W>
        auto extract(IData* obj) -> decltype(W::value) {
            
            return static_cast<W*>(obj)->value; // TODO: check for nullptr
        }

        template <typename W, typename T = decltype(W::value)>
        auto extract(IData* obj, const T& fallback) -> T {
            if (!obj || obj == NIL)
                return const_cast<T&>(fallback);
            
            return static_cast<W*>(obj)->value;
        }

        template <typename T, typename W = typename DataInfo<T>::wrapper>
        auto getValue(IData* obj, const T& fallback) -> T {
            if (!obj)
                return const_cast<T&>(fallback);

            return static_cast<W*>(obj)->value;
        }

        template <typename T>
        void setValue(IData* obj, const T& value) {
            if (obj)
                static_cast<typename DataWrapper<T>::type*>(obj)->assign(value);
        }

        template <typename T>
        state assign(IData* data, const T& value) {
            using Wrapper = typename DataInfo<T>::wrapper;

            if (data && data->type == DataInfo<T>::data_type)
                static_cast<Wrapper*>(data)->assign(value);
        }

        int64 extractInt(IData * data, int64 fallback = MS_DEFAULT_INTEGRAL_VALUE) {
            return extract<Integral>(data, fallback);
        }

        double64 extractDouble(IData * data, double64 fallback = MS_DEFAULT_DECIMAL_VALUE) {
            return extract<Decimal>(data, fallback);
        }

        std::string extractString(IData * data, const std::string& fallback = MS_DEFAULT_STRING_VALUE) {
            return extract<String>(data, fallback);
        }

        std::string toString(IData* data, bool detailed) {
            if (!data)
                return std::string("-");

            std::stringstream buf;

            switch (data->getType()) {
                case DataType::UNDEFINED: buf << "undefined"; break;
                case DataType::INVALID: buf << "invalid"; break;
                case DataType::NIL: buf << "nil"; break;

                case DataType::INTEGRAL: buf << extractInt(data); break;
                case DataType::DECIMAL: buf << extractDouble(data); break;
                case DataType::STRING: buf << '\'' << extractString(data) << '\''; break;

                default:
                    buf << "<unknown data type (" << data->type << ")>";

                    break;
            }

            if (detailed) {
                buf << " (" << traits::stringify::toString(data->getType()) << ")";

                int mc = Modifiers::count(data->modifiers);

                if (mc > 0) {
                    buf << " [";

                    // i = 1 to ignore 'none'
                    for (short i = 1, c = 0, r = 1; i < (short) Modifier::__count__; i++, r <<= 1) {
                        if (data->hasModifier((Modifier) r)) {
                            buf << traits::stringify::toString((Modifier) r);

                            if (++c < mc)
                                buf << ", ";
                        }
                    }

                    if (mc > 0)
                        buf << "]";
                }

                if (data->owner) {
                    buf << " {";
                    buf << data->owner->containerName;
                    buf << (data->owner->transient ? "*}" : "}");
                }

                if (!data->references)
                    buf << " ROOT";
                else
                    buf << " *" << (int) data->references;
            }

            return buf.str();
        }

        /* Returns true if the given IData* objects has the
         * 'type' data type. If data is nullptr, false is returned.
         */
        bool checkType(const IData* data, const DataType type) {
            if (!data)
                return type == DataType::NIL;

            return data->isType(type);
        }

        // Used by ObjectRef
        /*
        std::ostream& writeObjectDataString(std::ostream& stream, ObjectData& data) {
            return stream << data;
        }
        */

        // -- Memory Control Functions

        /* Creates an IData* object from the given source code
         * string 'content'. This might be:
         *  a) a string
         *  b) an integral (int)
         *  c) a decimal (double)
         * Other types are ignored and return a nullptr.
         */
        IData* craftData(DataType type, const std::string& content) {
            switch (type) {
                case DataType::INTEGRAL:
                    try {
                        return new Integral(std::stoi(content));
                    } catch (...) {
                        throw MSException(MS_ERROR_INVALID_DECIMAL_FORMAT, "failed to craft integral of '" + content + "'");
                    }

                case DataType::DECIMAL:
                    try {
                        return new Decimal(std::stod(content));
                    } catch (...) {
                        throw MSException(MS_ERROR_INVALID_DECIMAL_FORMAT, "failed to craft decimal of '" + content + "'");
                    }

                case DataType::STRING:
                    return new String(content);

                default:
                    return nullptr;
            }
        }

        /* Handles the deletion of Data<id, D> objects and frees
         * the allocated c++ object. Will change the data
         * parameter to nullptr after completion. Only works
         * with non-nullptr data parameters.
         */
        void freeData(IData *& data, const MemoryContainer* owner) {
            if (data) {
                // If the data belongs to a MemoryContainer, only this
                // owner can delete its values.
                if (data->owner && owner != data->owner)
                    return;

                // Check that we only delete the root object
                if (data->references-- > 0)
                    return;
                
                delete data;
                data = nullptr;
            }
        }

        /* Applies the IData modifiers from the source to the
         * target object. Returns false if a parameter is nullptr,
         * true otherwise.
         */
        bool transferModifiers(IData *& target, IData * source) {
            if (!target || !source)
                return false;

            if (target->isProtected())
                return false;

            target->modifiers = source->modifiers;

            return true;
        }

        /* Assigns the internal data of the source to the internal
         * data of the target. This function checks four all pre-
         * conditions first and then executes the IData::assign()
         * function on the target internally.
         * Will not throw but print any exception and returns
         * MS_EXCEPT_INTERNAL_ASSIGNMENT_FAIL in that case.
         * 
         * Assignment table:
         *  * requirements (X = target, Y = source):
         *   a) type(X) == type(Y)
         *   b) type(X, Y) == REF && type(X::ref) == type(Y::ref)
         * 
         *  const := const          ->  error
         *  const := non const      ->  error
         *  non const := const      ->  valid
         *  non const := non const  ->  valid
         */
        state assignData(IData *& target, IData* source, bool copyModifiers, bool initTargetOnNull) {
            if (!target) {
                // On initialization instead of reassignment: just assign pointer
                if (initTargetOnNull) {
                    target = source;

                    // Clear mods if this is a pure initialization.
                    // (e.g. mem[...] = literal -> do not copy CONST)
                    if (!copyModifiers && target)
                        target->modifiers = 0; // ignore PROTECTED (not using clearMods())

                    return MS_SUCCESS;
                }
                
                return MS_EXCEPT_NULL;
            }
            
            if (target->hasModifier(Modifier::CONST))
                return MS_EXCEPT_CONST_ASSIGN;

            if (target->hasModifier(Modifier::PROTECTED))
                return MS_EXCEPT_PROTECTED_ASSIGN;
            
            if (source) {
                if (target->type != source->type)
                    return MS_EXCEPT_TYPE_MISMATCH;

                // Rebinding data is done by each IData object internally ...
                try {
                    target->assign(source, copyModifiers);
                    // TODO: update owner??
                } catch (const MSException& exc) {
                    std::cout << "[Error] Internal assignment failed due to:\n >" << exc.getReason() << "\n";

                    return MS_EXCEPT_INTERNAL_ASSIGNMENT_FAIL;
                }

                return MS_SUCCESS;
            }

            return MS_EXCEPT_NULL;
        }

    }

    namespace debug {

        using namespace types;
        using namespace opcode;
        using namespace memory;

        namespace Console {

            enum Color : int {
                FG_BLACK    = 30,
                FG_RED      = 31,
                FG_GREEN    = 32,
                FG_YELLOW   = 33,
                FG_BLUE     = 34,
                FG_MAGENTA  = 35,
                FG_CYAN     = 36,
                FG_WHITE    = 37,
                // 38
                FG_DEFAULT  = 39,

                BG_BLACK    = 40,
                BG_RED      = 41,
                BG_GREEN    = 42,
                BG_YELLOW   = 43,
                BG_BLUE     = 44,
                BG_MAGENTA  = 45,
                BG_CYAN     = 46,
                BG_WHITE    = 47,
                // 48
                BG_DEFAULT  = 49
            };

            enum Style {
                RESET       = 0,
                BOLD        = 1,
                ITALIC      = 3,
                UNDERLINED  = 4,
                INVERSED    = 7,
                BOLD_OFF    = 21,
                ITALIC_OFF  = 23,
                UNDERLINE_OFF   = 24,
                INVERSE_OFF     = 27,
                FRAMED      = 51,
                DOUBLE_LINE = 61
            };

            class Modifier {
                Color fg;
                Color bg;
                Style style;
                int type;

                public:
                    Modifier(Color fg_, Color bg_, Style style_) : fg(fg_), bg(bg_), style(style_), type(4) {}
                    Modifier(Color fg_, Color bg_) : fg(fg_), bg(bg_), type(3) {}
                    Modifier(Color fg_, Style style_) : fg(fg_), style(style_), type(2) {}
                    Modifier(Color fg_) : fg(fg_) , type(1) {}
                    Modifier(Style style_) : style(style_) , type(0) {}
                    Modifier() {}

                    friend std::ostream&
                    operator<<(std::ostream& os, const Modifier& mod) {
                        if (mod.type == 4)
                            return os << "\033[" << mod.fg << ";" << mod.bg << ";" << mod.style << "m";
                        else if (mod.type == 3)
                            return os << "\033[" << mod.fg << ";" << mod.bg << "m";
                        else if (mod.type == 2)
                            return os << "\033[" << mod.fg << ";" << mod.style << "m";
                        else if (mod.type == 1)
                            return os << "\033[" << mod.fg << "m";
                        else if (mod.type == 0)
                            return os << "\033[" << mod.style << "m";
                        else return os;
                    }

            };

            // -- direct color output
            std::ostream& operator<<(std::ostream& os, Color color) {
                return os << "\033[" << static_cast<int>(color) << "m";
            }

            // -- direct style output
            std::ostream& operator<<(std::ostream& os, Style style) {
                return os << "\033[" << static_cast<int>(style) << "m";
            }

            static const Modifier _RESET_ALL(Style::RESET);
            static const Modifier _ERROR(Color::FG_RED);
            static const Modifier _INFO(Color::FG_GREEN);

            static std::string styled(std::string content, Style style) {
                std::stringstream ss;
                Modifier mod(style);

                ss << mod << content << _RESET_ALL;

                return ss.str();
            }

            static std::string mod(std::string content, Color fg, Color bg, Style style) {
                std::stringstream ss;
                Modifier mod(fg, bg, style);

                ss << mod << content << _RESET_ALL;

                return ss.str();
            }

            static std::string mod(std::string content, Color fg, Color bg = Color::BG_DEFAULT) {
                std::stringstream ss;
                Modifier mod(fg, bg);

                ss << mod << content << _RESET_ALL;

                return ss.str();
            }

            static std::string genMod(Color fg, Color bg = Color::BG_DEFAULT, Style style = Style::RESET) {
                std::stringstream ss;
                Modifier mod(fg, bg, style);

                ss << mod ;

                return ss.str();
            }

        }

        using ChatColor = Console::Color;
        using ChatStyle = Console::Style;

        std::ostream& applyColor(std::ostream& stream, ChatColor fg, ChatColor bg = ChatColor::BG_DEFAULT) {
            return stream << Console::Modifier(fg, bg);
        }

        std::ostream& resetStream(std::ostream& stream) {
            return stream << Console::_RESET_ALL;
        }

        Console::Color parseColor(char c, int colorOffset = 30) {
            int pick = c - 48; // 48 = ascii offset for '0'

            if (pick >= 0 && pick < 10 && pick != 8)
                return (Console::Color) (colorOffset + pick);
            
            return Console::Color::FG_DEFAULT;
        }

        // --- Transform JSON ---

        std::string epxandedJson(const std::string_view content) {
            std::stringstream buf;
            std::string spacer (0, '\t');

            int depth = 0;
            bool str {false};

            for (int i = 0; i < content.length(); i++) {
                if (content[i] == '{') {
                    spacer = std::string(++depth * 3, ' ');

                    buf << '{';
                    buf << '\n';
                    buf << spacer;
                } else if (content[i] == '}') {
                    spacer = std::string(--depth * 3, ' ');

                    buf << '\n';
                    buf << spacer;
                    buf << '}';
                } else if (content[i] == '[') {
                    spacer = std::string(++depth * 3, ' ');

                    buf << '[';
                    buf << '\n';
                    buf << spacer;
                } else if (content[i] == ']') {
                    spacer = std::string(--depth * 3, ' ');

                    buf << '\n';
                    buf << spacer;
                    buf << ']';
                } else if (content[i] == ',') {
                    buf << ',';
                    buf << '\n';
                    buf << spacer;
                } else if (content[i] == ':') {
                    buf << ':';
                    buf << ' ';
                } else if (content[i] == '\'') {
                    str = !str;

                    buf << '\'';
                } else if (content[i] != ' ' || str) {
                    buf << content[i];
                }
            }

            return buf.str();
        }

        // ---v print styled format v---

        std::string sformat(const char * str) {
            std::stringstream buf;

            while (*str) {
                if (*str == '`') {
                    if (*(str + 1) != 0)
                        buf << Console::Modifier(parseColor(*(str + 1), 40)); // 40 - background offset

                    str += 2;
                } else if (*str == '$') {
                    if (*(str + 1) == 0)
                        break;

                    if (*(str + 1) == 'r')
                        buf << Console::_RESET_ALL;
                    else if (*(str + 1) == 'b')
                        buf << Console::Modifier(Console::Style::BOLD);
                    else if (*(str + 1) == 'i')
                        buf << Console::Modifier(Console::Style::ITALIC);
                    else if (*(str + 1) == 'u')
                        buf << Console::Modifier(Console::Style::UNDERLINED);
                    else if (*(str + 1) == 's')
                        (void) 0; // ignore %s without params
                    else if (*(str + 1) == 'j') {
                        str += 2; // just skip this token, since $j can only be applied to left values (here = 0)
                    } else
                        buf << Console::Modifier(parseColor(*(str + 1)));

                    str += 2;
                } else
                    buf << *(str++);
            }

            buf << Console::_RESET_ALL;

            return buf.str();
        }

        template <typename T, typename... Ts>
        std::string sformat(const char * str, const T& base, const Ts&... appendix) {
            std::stringstream buf;

            while (*str) {
                if (*str == '`') {
                    if (*(str + 1) != 0)
                        buf << Console::Modifier(parseColor(*(str + 1), 40)); // 40 - background offset

                    str += 2;
                } else if (*str == '$') {
                    if (*(str + 1) == 0)
                        break;

                    if (*(str + 1) == 'r')
                        buf << Console::_RESET_ALL;
                    else if (*(str + 1) == 'b')
                        buf << Console::Modifier(Console::Style::BOLD);
                    else if (*(str + 1) == 'i')
                        buf << Console::Modifier(Console::Style::ITALIC);
                    else if (*(str + 1) == 'u')
                        buf << Console::Modifier(Console::Style::UNDERLINED);
                    else
                        buf << Console::Modifier(parseColor(*(str + 1)));

                    str += 2;
                } else if (*str == '%') {
                    if (*(str + 1) == 0)
                        break;
                    
                    // ignore normal % symbols
                    if (*(str + 1) != '%') {
                        buf << *(str++);
                        continue;
                    }
                    
                    buf << traits::stringify::toString(base); // always format
                    buf << sformat(str + 2, appendix...);

                    return buf.str();
                } else
                    buf << *(str++);
            }

            buf << Console::_RESET_ALL;

            return buf.str();
        }

        template <typename... Ts>
        void printsf(const char * str, const Ts&... appendix) {
            MS_IF_DEBUG {
                std::cout << sformat(str, appendix...) << std::endl;
            }
        }

        template <int level, typename... Ts>
        void printsf(const char * str, const Ts&... appendix) {
            if (ms_is_debug && debug_level >= level) {
                std::cout << sformat(str, appendix...) << std::endl;
            }
        }

        template <typename... Ts>
        void printsf_ignore_debug_mode(const char * str, const Ts&... appendix) {
            std::cout << sformat(str, appendix...) << std::endl;
        }

        //  (°)<  -swrrrt
        // (/_\)
        //  ^ ^

        // ---^ ................... ^---

        void resetAll() {
            std::cout << Console::_RESET_ALL << std::endl;
        }

        void print(const std::string& prefix, Console::Modifier& prefixMod, std::string& content, Console::Modifier& contentMod) {
            MS_IF_DEBUG {
                std::cout << prefixMod << prefix << Console::_RESET_ALL << contentMod << content << Console::_RESET_ALL << std::endl;
            }
        }

        void print(const std::string& str, const Console::Modifier& modifier) {
            MS_IF_DEBUG {
                std::cout << modifier << str << Console::_RESET_ALL << std::endl;
            }
        }

        void print(const std::string& str) {
            MS_IF_DEBUG {
                std::cout << str << std::endl;
            }
        }

        void error(const std::string& str) {
            MS_IF_DEBUG {
                print("Error: " + str, Console::BG_RED);
            }
        }

        void error(state code, const std::string& str) {
            MS_IF_DEBUG {
                print("[" + std::to_string(code) + "] Error: " + str, Console::BG_RED);
            }
        }

        std::string stateCode(state s) {
            if (ms::debug_translate_state_codes)
                return /*util::toUpperCase(*/ getStateCode(s) /*)*/;
            
            return std::to_string(s);
        }

        std::string stretch(const std::string& str, int len, char fill = ' ') {
            std::stringstream stream;

            stream << std::hex << str;

            if (str.length() < len) {
                for (int i = str.length(); i < len; i++)
                    stream << fill;
            }

            return stream.str();;
        }

        template <typename T>
        std::string format(const char * x, const T& value) {
            std::stringstream buffer;

            while (*x) {
                if (*x == '%') {
                    if (*(x + 1) == 0)
                        break;

                    if (*(x + 1) == '%')
                        buffer << value;

                    x++;
                } else
                    buffer << *(x++);
            }

            return buffer.str();
        }

        template <typename T, typename... Ts>
        std::string format(const char * x, const T& value, const Ts&... values) {
            std::stringstream buffer;

            while (*x) {
                if (*x == '%') {
                    if (*(x + 1) == 0)
                        break;

                    if (*(x + 1) == '%')
                        buffer << value;
                    
                    buffer << format(x + 2, values...);

                    return buffer.str();
                } else
                    buffer << *(x++);
            }

            return buffer.str();
        }

        std::string toString(InstructionSet& is, bool stopOnLIS = true) {
            std::stringstream str;
            uindex i = 0;
            uindex bytes = 0;

            Instruction err(Op::IIS);
            Instruction& ins = err;
            OpInfo info {"__ERROR__", Op::IIS, 0};

            str << "Index    Code   ID   Name          {0..4 parameters}" << std::endl;
            str << "----------------------------------------------------" << std::endl;

            for (i = 0; i < is.finalInstructions.size(); i++) {
                ins = is.finalInstructions[i];
                info = getOpInfo((Op) ins);
                bytes += sizeof(operation) + ins.paramCount() * sizeof(param);

                str << "0x" << std::hex << std::setfill('0') << std::setw(5) << i << "  ";
                str << "0x" << std::hex << std::setfill('0') << std::setw(3) << (unsigned int) info.b << "  ";
                
                if (ins.getLabel() != -1)
                    str << std::dec << std::setfill('0') << std::setw(3) << ins.getLabel() << "  ";
                else if (i == is.baseOffset) // just a marker
                    str << ">>>  ";
                else
                    str << "     ";
                
                // instruction name
                str << stretch(info.a, 13);

                for (int j = 0; j < ins.paramCount(); j++)
                    str << " 0x" << std::hex << std::setfill('0') << ins.getParam(j) << ((j < (ins.paramCount() - 1) || ins.hasVARG()) ? ',' : ' ');

                // variable string param
                if (ins.hasVARG())
                    str << " '" << ins.getVARG() << '\'';

                str << std::endl;

                if (ins == Op::LIS && stopOnLIS)
                    break;
            }

            str << '\n' << std::hex;
            str << "      Base offset: 0x" << is.baseOffset << '\n';

            str << std::dec;
            str << " Total operations: " << i << '\n';
            str << "       Bytes used: " << bytes << '\n';

            return str.str();
        }

        std::string toString(IData ** memory, usize size) {
            std::stringstream str;
            int nstart = -1, nend = -1; // null start, null end

            for (int i = 0; i < size; i++) {
                if (memory[i] == nullptr) {
                    if (nstart == -1)
                        nstart = i;

                    nend = i;
                } else {
                    if (nstart != -1) {
                        str << "0x" << std::hex << std::setfill('0') << std::setw(5) << nstart << "...";
                        str << "0x" << std::hex << std::setfill('0') << std::setw(5) << nend << ": NIL" << std::endl;

                        nstart = nend = -1;
                    }

                    str << "0x" << std::hex << std::setfill('0') << std::setw(5) << i << ": " << memory::toString(memory[i], true) << std::endl;
                }
            }

            if (nstart != -1) {
                str << "0x" << std::hex << std::setfill('0') << std::setw(5) << nstart << "...";
                str << "0x" << std::hex << std::setfill('0') << std::setw(5) << nend << ": NIL" << std::endl;
            }

            return str.str();
        }

        // -- Smart Debug

        static int debug_level = 0;

        void beginSection(const std::string& content) {
            MS_IF_DEBUG {
                for (int i = 0; i < debug_level; i++)
                    std::cout << "| ";

                std::cout << " / -- " << sformat(content.c_str()) << std::endl;
                debug_level++;
            }
        }

        void sdebug(const std::string& content) {
            MS_IF_DEBUG {
                for (int i = 0; i < debug_level; i++)
                    std::cout << "| ";

                std::cout << sformat(content.c_str()) << std::endl;
            }
        }

        template <typename... Ts>
        void sdebug(const std::string& content, const Ts&... args) {
            MS_IF_DEBUG {
                for (int i = 0; i < debug_level; i++)
                    std::cout << "| ";

                std::cout << sformat(content.c_str(), args...) << std::endl;
            }
        }

        state endSection(const std::string& content = "") {
            MS_IF_DEBUG {
                debug_level--;

                for (int i = 0; i < debug_level; i++)
                    std::cout << "| ";

                std::cout << " \\ ------ " << sformat(content.c_str()) << std::endl;
            }

            return 0;
        }

    }

    namespace info {

        using types::Operation;
        using types::OperationType;
        using types::OperationDirection;

        // -- Layout

        // TODO: move up??
        #define MS_LANG_INFO_ATTR_QUERY(name) if (std::strcmp(id, #name) == 0) return &name;
        #define MS_LANG_INFO_REQUEST(name, list) if (std::strcmp(registryId, name) == 0) return get_info(list, objectId);
        #define MS_LANG_INFO_REQUEST_QUERY(name, func) if (std::strcmp(registryId, name) == 0) return &func(query);

        /* LangInfoObject is the base class for every informational class.
         * It provides the standard 'content' attribute to identify a certain object.
         * 
         * Each LangInfoObject also implements following functions:
         *  - queryAttribute (const char*): get an attribute with a string name given
         *  - toString (std::ostream&): format this object and write it to a stream
         */
        struct LangInfoObject {
            std::string content;

            LangInfoObject(const std::string& value = "") : content(value) {}

            /* Returns a pointer to the attribute given by its name as a parameter.
             * Will return 'nullptr' if no attribute exists for the identifier.
             */
            virtual void* queryAttribute(const char*) {
                return nullptr;
            }

            /* Stringifies the LangInfoObject and writes it to a stream.
             * The default '<<' operator uses this implementation.
             */
            virtual std::ostream& toString(std::ostream& stream) const {
                return stream << "LangInfoObject {" << "content: " << content << "}";
            }

            friend std::ostream& operator <<(std::ostream& stream, const LangInfoObject& info) {
                return info.toString(stream);
            }
        };

        /* The KeywordInfoObject is a LangInfoObject with its 'content' attribute
         * being a valid language keyword.
         */
        struct KeywordInfoObject : public LangInfoObject {
            KeywordInfoObject(const std::string& keyword = "") : LangInfoObject(keyword) {}

            const std::string& getKeyword() const {
                return content;
            }

            std::ostream& toString(std::ostream& stream) const {
                stream << "KeywordInfoObject {" << "keyword: " << content << "}";

                return stream;
            }

        };

        struct OperationInfo : public LangInfoObject {
            Operation op;
            OperationType type;
            OperationDirection direction;
            int precedence;
            int paramCount; // unary, binary, ...

            OperationInfo(Operation _op = Operation::NONE, const std::string& token = "?", int _precedence = 5, OperationType _type = OperationType::UNDEFINED, OperationDirection _direction = OperationDirection::LEFT_TO_RIGHT, int _paramCount = 2) :
             LangInfoObject(token), op(_op), type(_type), direction(_direction), precedence(_precedence), paramCount(_paramCount) {}
            
            void* queryAttribute(const char* id) {
                MS_LANG_INFO_ATTR_QUERY(op)
                MS_LANG_INFO_ATTR_QUERY(type)
                MS_LANG_INFO_ATTR_QUERY(direction)
                MS_LANG_INFO_ATTR_QUERY(precedence)
                MS_LANG_INFO_ATTR_QUERY(paramCount)

                return nullptr;
            }

            bool isUnary() const {
                return paramCount == 1;
            }

            bool isBinary() const {
                return paramCount == 2;
            }

            std::ostream& toString(std::ostream& stream) const {
                stream << "OperationInfo {";
                stream << "token: " << content << ", ";
                stream << "operation: " << traits::stringify::toString(op, "-") << ", ";
                stream << "type: " << traits::stringify::toString(type, "-") << ", ";
                stream << "direction: " << traits::stringify::toString(direction, "-") << ", ";
                stream << "precedence: " << precedence << ", ";
                stream << "paramCount: " << paramCount;
                stream << "}";

                return stream;
            }

        };

        // -- Data
        
        /* Structure:
         *  
         * <Registry>               -- MS_VALUE_LIST
         *  |
         *  + <LangInfoObject>[]    -- LangInfoObject
         *    |
         *    + <Attribute>         -- <typename T>
         */

        /* ! */
        
        // OperationInfo = {Operation, Token, Precedence, OperationDirection, OperationType, ParamCount}
        MS_VALUE_LIST(OperationInfo) op_info {
            {Operation::ADD, "+", 4, OperationType::ARITHMETIC}
        };

        /* For alternate queries, a custom lookup can be added.
         * The function name should describe the query.
         * 
         * e.g.: query the first operation with precedence given through the parameter
         *  MS_LIST_QUERY_LOOKUP(get_op_info, op_info, OperationInfo, precedence == input, {})
         */

        // -- Serialized Access

        /* Returns the LangInfoObject pointer for a requested object by the 'objectId'
         * within the given registry. If no object was found for the name, nullptr is returned.
         */
        template <typename T>
        LangInfoObject* get_info(std::vector<T>& registry, const char* objectId) {
            for (auto& d : registry) {
                if (d.content == objectId)
                    return &d;
            }

            return nullptr;
        }

        /* Returns the LangInfoObject pointer for a requested object by the 'objectId'
         * within the given registry through its 'registryId'. If no object was found
         * for the name, nullptr is returned.
         */
        LangInfoObject* get_info(const char* registryId, const char* objectId) {
            MS_LANG_INFO_REQUEST("op-info", op_info)

            return nullptr;
        }

        /* Disabled, used for query lookups other than the default content comparison.

        template <typename V>
        LangInfoObject* get_info_by_query(const char* registryId, const V& query) {
            MS_LANG_INFO_REQUEST_QUERY("op-info", get_op_info)

            return nullptr;
        }

        template <typename T, typename V>
        T& get_info_by_query(const char* registryId, const V& query, const T& fallback) {
            LangInfoObject* obj {nullptr};
            
            if (!(obj = get_info(registryId, query)))
                return const_cast<T&>(fallback);

            return static_cast<T&>(*obj);
        }
        */

        bool has_attribute(LangInfoObject* iobj, const char* attr) {
            return (iobj && iobj->queryAttribute(attr));
        }

        template <typename T>
        T& get_attribute(LangInfoObject* iobj, const char* attr, const T& fallback) {
            void* result {nullptr};

            if (!iobj || !(result = iobj->queryAttribute(attr)))
                return const_cast<T&>(fallback);

            return *static_cast<T*>(result);
        }

        template <typename T>
        T& get_attribute(LangInfoObject& iobj, const char* attr, const T& fallback) {
            return get_attribute(&iobj, attr, fallback);
        }

        template <typename T>
        T& get_attribute(const char* infoRegistry, const char* infoObjectId, const char* attributeId, const T& fallback) {
            return get_attribute(get_info(infoRegistry, infoObjectId), attributeId, fallback);
        }

    }

    namespace lex {

        using namespace types;
        using namespace traits;
        using namespace util;

        struct Token {
            index column {-1}, row {-1};
            std::string content {""};
            std::string module {""};

            Token(index col, index line, const std::string& moduleName, const std::string& contentString) :
                column(col), row(line), module(moduleName), content(contentString) {}
            Token() : Token(-1, -1, "_unset_", "") {}

            bool is(const std::string_view& view) const {
                return content == view;
            }

            bool isEmpty() const {
                return content.length() == 0;
            }
        };

        struct CodePoint {
            int line {-1};
            int column {-1};

            CodePoint(int Line = -1, int Column = -1) : line(Line), column(Column) {}

            CodePoint& next() {
                column++;

                return *this;
            }

            CodePoint& nextLine() {
                line++;

                return *this;
            }
        };

        class CodeLine {
            friend class Tokenizer; // the tokenizer needs to fill in data

            private:
                int lineIndex {-1};
                std::vector<std::string> tokens;

            public:
                CodeLine(const std::vector<std::string>& elements) : tokens(elements) {}
                CodeLine(int index = -1) : lineIndex(index) {}

                const std::string_view getToken(int index) const {
                    static std::string_view empty { "" };

                    if (index < 0 || index >= tokens.size())
                        return empty;

                    return tokens[index];
                }

                std::string toString() const {
                    std::stringstream buf;

                    for (int i = 0; i < tokens.size(); i++) {
                        buf << tokens[i];

                        if (i < (tokens.size() - 1))
                            buf << ' ';
                    }

                    buf << '\n';

                    return buf.str();
                }

                std::string toMarkedString(int index, int offset = 0, debug::ChatColor markerColor = debug::ChatColor::FG_GREEN) const {
                    if (tokens.size() == 0)
                        return debug::Console::styled("no code view available", debug::ChatStyle::ITALIC);
                    
                    std::stringstream buf;
                    int anchor = 0;
                    int elemwidth = 0;

                    for (int i = 0; i < tokens.size(); i++) {
                        buf << tokens[i];

                        if (i < index) {
                            anchor += tokens[i].length();
                            anchor += 1;
                        } else if (i == index)
                            elemwidth = tokens[i].length();

                        if (i < (tokens.size() - 1))
                            buf << ' ';
                    }

                    if (offset != 0)
                        offset = std::max(0, std::min(80, offset));

                    //std::cout << std::string(anchor + elemwidth / 2 + offset, ' ') << '#' << '\n';

                    buf << '\n';
                    buf << std::string(anchor + elemwidth / 2 + offset, ' ');
                    buf << "^\n";

                    return buf.str();
                }

                std::string toMarkedIntervalString(int from, int to, int offset = 0, debug::ChatColor markerColor = debug::ChatColor::FG_GREEN) const {
                    if (tokens.size() == 0)
                        return debug::Console::styled("no code view available", debug::ChatStyle::ITALIC);
                    
                    std::stringstream buf;
                    int anchor = 0;
                    int elemwidth = 0;
                    int line = 0;

                    for (int i = 0; i < tokens.size(); i++) {
                        buf << tokens[i];

                        if (i < from) {
                            anchor += tokens[i].length();
                            anchor += 1;
                        } else if (i == from) {
                            elemwidth = tokens[i].length();
                        } else if (i <= to)
                            line += tokens[i].length() + 1;

                        if (i < (tokens.size() - 1))
                            buf << ' ';
                    }

                    if (offset != 0)
                        offset = std::max(0, std::min(80, offset));

                    //std::cout << std::string(anchor + elemwidth / 2 + offset, ' ') << '#' << '\n';

                    buf << '\n';
                    buf << std::string(anchor + elemwidth / 2 + offset, ' ');
                    buf << "^";
                    buf << std::string(std::max(0, line - elemwidth / 2), '~'); // std::min to avoid -1 and std::basic_string exception
                    buf << '\n';

                    return buf.str();
                }

                friend std::ostream& operator <<(std::ostream& stream, const CodeLine& line) {
                    return stream << line.toString();
                }

                int tokenCount() const {
                    return tokens.size();
                }
        };

        class Code {
            friend class Tokenizer; // the tokenizer needs to fill in data
            friend class comp::Context;

            public:
                const static Token EMPTY;

            private:
                std::string moduleName;
                std::string file;
                std::string plain;
                bool isRead {false};
                bool tokenized {false};

                std::vector<CodeLine> lines;
                Sequence<Token> tokens;

            public:
                Code() {
                    debug::printsf("`4Code");
                }
                virtual ~Code() {
                    tokens.~Sequence<Token>();

                    debug::printsf("`4~Code (module=%%)", moduleName);
                }

                state readFile(const std::string& file) {
                    if (isRead)
                        return MS_ERROR_CODE_ALREADY_READ; // requires new Code object
                    
                    this->file = file;
                    this->moduleName = getFileName(file);

                    static char ASCII_NL = (char) 10;

                    std::string line;
                    std::ifstream in(file);
                    std::stringstream build;

                    MS_IF_DEBUG {
                        std::cout << "File: " << file << '\n';
                        std::cout << "Module: " << moduleName << '\n';
                    }

                    if (in.is_open()) {
                        while (std::getline(in, line)) {
                            build << line << static_cast<char>(ASCII_NL);
                        }

                        in.close();
                    } else
                        return MS_ERROR_MISSING_FILE;

                    plain = build.str();
                    isRead = true;

                    return MS_SUCCESS;
                }

                state read(const std::string_view& raw) {
                    // TODO: implement

                    return MS_FATAL;
                }

                const std::string getContent(int index) const {
                    return getToken(index).content;
                }

                const std::string getContent(int from, int to) const {
                    std::stringstream buf;

                    for (int i = from; i < to; i++) {
                        buf << getToken(i).content;

                        if (i < (to - 1))
                            buf << ' ';
                    }

                    return buf.str();
                }

                const Token& getToken(int index) const {
                    if (index < 0 || index >= tokens.size())
                        return EMPTY;

                    return *tokens.get((uindex) index);
                }

                const std::string_view getContent(CodePoint point) const {
                    if (point.line < lines.size()) {
                        if (lines[point.line].tokenCount() < point.column)
                            return lines[point.line].getToken(point.column);
                    }

                    return "";
                }

                CodePoint getPointOfIndex(int tokenIndex) const {
                    CodePoint cp {-1, -1};

                    if (tokenIndex >= 0 && tokenIndex < tokens.size()) {
                        const Token* t = tokens.get(tokenIndex);

                        cp.line = t->row;
                        cp.column = t->column;
                    }
                    
                    return cp;
                }

                const CodeLine& getLine(int line) const {
                    const static CodeLine empty {-1};

                    if (line < 0 || line >= lines.size())
                        return empty;

                    return lines[line];
                }

                const std::string getModuleName() const {
                    return moduleName;
                }

                const std::vector<CodeLine>& getLines() const {
                    return lines;
                }

                const Sequence<Token>& getTokens() const {
                    return tokens;
                }

                const int length() const {
                    return tokens.size();
                }

                const size_t lineCount() const {
                    return lines.size() - 1; // ignore last empty line
                }

                const bool isTokenized() const {
                    return tokenized;
                }
        };

        struct TokenizerState {
            int totalCharacters {0};
            int position {0};
            int line {0};
            int column {0}; // token column
            int realColumn {0};
            std::stringstream* currentLine {nullptr};

            float getProgress() const {
                if (totalCharacters > 0.0f)
                    return (float) position / (float) totalCharacters;

                return 1.0f;
            }
        };

        class Tokenizer {
            private:
                Code * file {nullptr};

                // current state container
                std::stringstream buf;
                std::stringstream lineBuf;
                int pos {0}, line {0}, col {0};
                int buflen {0};

                // Helper functions, assumes that a first line does exist
                void init() {
                    pos = line = col = buflen = 0;
                    buf.str("");
                }

                void flush() {
                    // ignore empty buffers
                    if (buflen > 0)
                        pushToken();
                    
                    clearBuf();
                }

                void pushToken() {
                    Token t;

                    t.module = file->moduleName;
                    t.content = buf.str();
                    t.column = col++;
                    t.row = line;

                    file->tokens.append(t);
                    file->lines[line].tokens.push_back(t.content);
                }

                void push(char c) {
                    Token t;
                    
                    t.content = std::string(1, c);
                    t.column = col++;
                    t.row = line;
                    t.module = file->moduleName;

                    file->tokens.append(t);
                    file->lines[line].tokens.push_back(t.content);
                }

                void push(const std::string& s) {
                    Token t;
                    
                    t.content = s;
                    t.column = col++;
                    t.row = line;
                    t.module = file->moduleName;

                    file->tokens.append(t);
                    file->lines[line].tokens.push_back(t.content);
                }

                void log(const std::string& s) {
                    file->lines[line].tokens.push_back(s);
                }

                void append(char c) {
                    buf << c;
                    lineBuf << c;
                    buflen++;
                }

                void clearBuf() {
                    buf.str("");
                    buflen = 0;

                    lineBuf << " ";
                }

                void newLine() {
                    lineBuf.str("");

                    col = 0;
                    line++;

                    file->lines.push_back({line});
                }
                
            public:

                std::string getContent(Code& code, int from, int to) {
                    if (from >= 0 && to > from && to < code.plain.size())
                        return code.plain.substr(from, to - from);

                    return "";
                }

                std::string errInfo(Code& code) {
                    std::stringstream buf;

                    buf << lineBuf.str();
                    buf << code.plain[pos];

                    return buf.str();
                }

                state tokenize(Code& code, TokenizerState& state) {
                    if (!code.isRead)
                        return MS_ERROR_INVALID_CODE_STATE;

                    state.totalCharacters = code.plain.length();
                    state.currentLine = &lineBuf;

                    // set state
                    this->file = &code;
                    this->file->lines.push_back({line}); // create first line

                    // init state
                    init();

                    bool str = false;
                    bool dot = false;

                    int max = 20;

                    while (pos < code.plain.length()) {
                        char c = code.plain[pos];

                        // Debug
                        state.position = pos;
                        state.line = line;
                        state.column = col;
                        state.realColumn++;

                        // true, when we are parsing a decimal number
                        if (dot) {
                            if (isDigit(c))
                                dot = false;
                            else
                                return MS_ERROR_INVALID_DECIMAL_FORMAT; // expected a figure behind the dot --> INVALID_DECIMAL_FORMAT
                        }

                        if (str) {
                            if (c == '\'') {
                                str = false;

                                // error on empty string
                                if (buflen == 0)
                                    return MS_ERROR_INVALID_SYNTAX;

                                flush();
                                push(c);
                            } else
                                append(c);

                            pos++;

                            continue;
                        }

                        switch (c)
                        {

                        // -- omit line breaks '\n'
                        case 10 /* new line terminator */ :
                            flush(); // just push current buffer content; ignore line break
                            newLine();

                            state.realColumn = 0;

                            break;
                        
                        // -- omit spaces (exception: kept in strings)
                        case ' ':
                            flush();

                            break;

                        // -- string identifier
                        case '\'':
                            if (pos + 1 < code.plain.length() && code.plain[pos + 1] == '\'')
                                return MS_ERROR_EMPTY_STRING_LITERAL; // error: empty string

                            if (!str)
                                str = true;
                            else
                                str = false;

                            flush();
                            push(c);

                            break;
                        
                        // -- simple symbols to push
                        case ':':
                            //HANDLE_STRING;

                            if (pos + 1 < code.plain.length()) {
                                if (code.plain[pos + 1] == ':') {
                                    flush();
                                    push("::");

                                    // advance for forwarded character
                                    pos += 1;

                                    break;
                                }
                            }

                            flush();
                            push(c);

                            break;

                        case ',':
                        case '(':
                        case ')':
                        case '[':
                        case ']':
                        case '{':
                        case '}':
                        case '$':
                        case '&':
                            //HANDLE_STRING;
                            
                            flush();
                            push(c);

                            break;

                        case '#':
                            flush();
                            push(c);

                            break;

                        // -- push '.' operator (exception: kept in numbers)
                        case '.':
                            //HANDLE_STRING;

                            // fuse '...' into one token
                            if (pos + 2 < code.plain.length()) {
                                if (code.plain[pos + 1] == '.' && code.plain[pos + 2] == '.') {
                                    flush();
                                    push("...");

                                    pos += 2;

                                    break;
                                }
                            }

                            if (buf.tellp() > 0 && isNumber(buf.str(), false /* only check for integer as prefix */ )) {
                                // current buffer content is a number, thus this dot is evaluated as a decimal separator
                                // only append, awaiting rest of number to push

                                // ***** NOTE: if the next character is not a number -> ERROR *****
                                dot = true;

                                append(c);
                            } else {
                                // this dot is a function call operator

                                flush();
                                push(c);
                            } 

                            break;
                        
                        default:
                            append(c); // for any other character, just append it to the buffer

                            break;
                        }

                        pos++;
                    }

                    flush();

                    // Validate the Code to be parseable
                    code.tokenized = true;

                    return MS_SUCCESS;
                }
        };

        class TracebackElement {};

        class Traceback {};

        // -- Static Variables

        const Token Code::EMPTY {};

        // -- Functions

        std::string& tokenContent(const Sequence<Token>& tokens, uindex i) {
            static std::string empty {""};

            if (i < tokens.size())
                return tokens.get(i)->content;

            return empty;
        }

        std::string tokenContent(const Sequence<Token>& tokens, uindex start, uindex end, char delimiter = ' ') {
            static std::string empty {""};
            std::stringstream buf;

            if (start >= 0 && end <= tokens.size() && end > start) {
                for (uindex i = start; i < end; i++) {
                    buf << tokens.get(i)->content;

                    if ((i + 1) < end)
                        buf << delimiter;
                }

                return buf.str();
            }

            return empty;
        }

        static bool operator ==(const Token& token, const std::string_view cmp) {
            return token.content == cmp;
        }

        static bool operator !=(const Token& token, const std::string_view cmp) {
            return token.content != cmp;
        }

    }

    namespace lang {

        using namespace types;
        using namespace traits;
        using namespace lex;
        using namespace comp;

        // -- Static IDs

        static Address addressCounter {0};
        static int entityCounter {0};
        static int moduleIndex {0};

        // -- Declarations

        using ProtoCallback = state (*)(Proto&, types::ProtoCallbackId, memory::Object&);

        // -- Definitions

        // == Lang META and Reflection ==

        #define MS_REFLECT_NODE_NAME RNode
        #define MS_REFLECT_FUNC(table) MS_REFLECT_NODE_NAME& getNode(const std::string& key) {              \
            static auto map = std::map<std::string, MS_REFLECT_NODE_NAME> { table.begin(), table.end() };   \
                                                                                                            \
            return map.at(key);                                                                             \
        }
        #define MS_REFLECT_BEGIN MS_VALUE_MAP(std::string, MS_REFLECT_NODE_NAME) nodes {
        #define MS_REFLECT_END }; MS_REFLECT_FUNC(nodes)
        #define MS_REFLECT_FIELD(name, ...) {#name, MS_REFLECT_NODE_NAME(__VA_ARGS__)}

        struct MS_REFLECT_NODE_NAME {
            std::string string_val;

            union {
                char char_val;
                short short_val;
                int int_val;

                float float_val;
                double double_val;
            };

            MS_REFLECT_NODE_NAME(const char* val) : string_val(val) {}
            MS_REFLECT_NODE_NAME(int intVal) {
                int_val = intVal;
            }

            MS_REFLECT_NODE_NAME(int a, int b) {}
        };

        class Reflectable {
            public:
                virtual MS_REFLECT_NODE_NAME& getNode(const std::string& key) = 0;
        };

        class Test : public Reflectable {
            public:
                MS_REFLECT_BEGIN
                    MS_REFLECT_FIELD(paramCount, 0),
                    MS_REFLECT_FIELD(name, "")
                MS_REFLECT_END

                Test() {}
        };

        void a() {
            Test t;
            
            std::cout << t.getNode("name").string_val << "\n";
            std::cout << t.getNode("name").short_val << "\n";
        }

        // Lang Objects

        // reflections, error construction, ...
        class SymbolTable {

        };

        struct Callable {
            int requiredArguments {0};

            Callable(int requiredArgumentCount) : requiredArguments(requiredArgumentCount) {}
        };

        // --

        // Contains additional information for the type specifier
        struct TypeInfo {
            // FCall* fcall {nullptr};
            // Object* object {nullptr}; // might be a proto
        };

        // Type Specifier
        class TypeSpec {
            public:
                DataType dataType { DataType::UNDEFINED };
                DataType refType  { DataType::UNDEFINED }; // the referenced data type if 'dataType' is REF
                TypeInfo info;

            public:
                TypeSpec(DataType dt = DataType::UNDEFINED, DataType ref = DataType::REF) : dataType(dt), refType(ref) {}
                explicit TypeSpec(DataType dt, TypeInfo ti) : dataType(dt), info(ti) {}

                TypeSpec& operator =(const TypeSpec& another) {
                    dataType = another.dataType;
                    refType = another.refType;
                    info = another.info;

                    return *this;
                }

                operator DataType() {
                    return dataType;
                }

                bool operator ==(TypeSpec another) const {
                    return dataType == another.dataType && refType == another.refType;
                }

                bool operator ==(DataType type) const {
                    return dataType == type;
                }

                DataType finalType() const {
                    // TODO: add refId -> data type

                    if (dataType == DataType::REF)
                        return refType;

                    return dataType;
                }

                bool isRef() const {
                    return dataType == DataType::REF;
                }

                bool isFCall() const {
                    return dataType == DataType::FCALL;
                }
        };

        // --

        struct Entity {
            EntityType  type         { EntityType::UNKNOWN };
            Entity*     owner        { nullptr };
            std::string name;
            Address     address      { MS_NULL };
            Address     localAddress { MS_NULL };
            Modifiers   modifiers;

            Entity(Entity * owner = nullptr, EntityType type = EntityType::UNKNOWN, const std::string& name = "", Modifiers mods = {}) {
                this->owner = owner;
                this->type = type;
                this->name = name;
                this->modifiers = mods;

                if (isVariable())
                    this->address = addressCounter++;

                if (name.length() == 0)
                    this->name = std::string("E" + std::to_string(entityCounter++) + "_" + typeStr(type));

                debug::printsf("`4Entity (name=%%, type=%%)", name, traits::stringify::toString(type));
            }

            virtual ~Entity() {
                debug::printsf("`4~Entity (name=%%, type=%%)", name, traits::stringify::toString(type));
            }

            Entity& operator =(const Entity& ent) {
                if (this != &ent) {
                    type = ent.type;
                    owner = ent.owner;
                    name = ent.name;
                    address = ent.address;
                    modifiers = ent.modifiers;
                }

                return *this;
            }

            // -- State info --

            virtual bool isClause() const {
                return !isPrimitive() && type != EntityType::OBJECT;
            }

            virtual bool isNamespace() const {
                return (int) type >= (int) EntityType::NAMESPACE && (int) type <= (int) EntityType::CATCH;
            }

            virtual bool isPrimitive() const {
                return type == EntityType::INTEGRAL || type == EntityType::DECIMAL || type == EntityType::STRING;
            }

            virtual bool isVariable() const {
                return ((int) type >= (int) EntityType::VARIABLE) || type == EntityType::OBJECT;
            }

            Namespace * ns() const {
                if (!isNamespace())
                    throw MSException(MS_FATAL, "cannot convert entity of type " + std::string(typeStr(type)) + " to a namespace pointer");
                
                return (Namespace*) this;
            }

            operator Namespace*() {
                if (!isNamespace())
                    throw MSException(MS_FATAL, "cannot convert entity of type " + std::string(typeStr(type)) + " to a namespace pointer");
                
                return (Namespace*) this;
            }

            static std::string typeStr(EntityType t) {
                switch (t) {
                    case EntityType::UNKNOWN:
                        return "unknwon";
                    case EntityType::MODULE:
                        return "module";
                    case EntityType::NAMESPACE:
                        return "namespace";
                    case EntityType::FUNCTION:
                        return "function";
                    case EntityType::VARIABLE:
                        return "variable";
                    case EntityType::PARAM:
                        return "param";
                    case EntityType::VARARG_PARAM:
                        return "vararg";
                    default:
                        return std::to_string((int) t);
                }
            }

            virtual std::string toString() const {
                std::stringstream buf;
                std::stringstream origin;

                Entity * parent = this->owner;
                std::vector<std::string> path;

                while (parent) {
                    path.push_back(parent->name);
                    parent = parent->owner;
                    
                }

                for (int i = path.size() - 1; i >= 0; i--) {
                    origin << path[i];

                    if (i != 0)
                        origin << '>';
                }

                buf << "{name: " << name << ", type: " << typeStr(type) << ", owner: " << owner;
                buf << ", addresses: {local: " << localAddress << ", global: " << address << "}, origin: " << origin.str() << ", modifiers: " << util::toBinary(modifiers.data, 8) << "}";
            
                return buf.str();
            }

            friend std::ostream& operator <<(std::ostream& stream, const Entity& e) {
                return stream << e.toString();
            }
        };

        struct Namespace : public Entity {
            uindex level {0};
            uindex addressOffset {0};
            uindex internalOffset {0};

            //Namespace * parent;
            Map<std::string, Entity*> members;

            Namespace(Entity * owner, EntityType type = EntityType::NAMESPACE, const std::string& name = "") : Entity(owner, type, name) {
                this->type = type;

                if (owner && owner->isNamespace())
                    level = owner->ns()->level + 1;

                /*

                if (owner && owner->isNamespace()) {
                    if (owner && owner->type == EntityType::FUNCTION) {
                        addressOffset = owner->ns()->lastAddress() + 1;
                    }

                    level = owner->ns()->level + 1;
                } else {
                    level = 0;
                    addressOffset = 0;
                }
                */

                MS_IF_DEBUG
                    std::cout << "CREATE_NAMESPACE name=" << this->name << " type=" << typeStr(type) << " variables=" << &members << std::endl;
            }
            virtual ~Namespace() {
                for (auto& member : members) {
                    if (!member.unset && member.value)
                        delete member.value;

                    member.unset = true;
                }
            }

            Namespace& operator =(const Namespace& n) = default; /*{
                if (this != &n) {
                    ((Entity) *this) = (Entity) n;

                    level = n.level;
                    addressOffset = n.addressOffset;
                    variables = n.variables;
                }

                return *this;
            }
            */

            // used when a namespace's end is parsed by
            // the main parsing process
            //virtual state close(Context * ctx, Module * module, Block& b, uindex i) {}

            /*
            state put(const std::string& name, ValueInfo info) {
                if (variables.containsKey(name))
                    return ERROR_VAR_EXISTS;

                variables.put(name, info);

                return SUCCESS;
            }
            */

            state registerEntity(const std::string& name, Entity * entity) {
                if (!entity || members.containsKey(name))
                    return MS_ERROR_ENTITY_ALREADY_REGISTERED;

                // auto update entity address if not pre-set
                //if (entity->address == 0)
                    //entity->address = addressOffset + internalOffset++;

                // update the entity's address so the 'pushl' command will take effect
                entity->localAddress = internalOffset++;
                entity->owner = this;

                members.put(name, entity);

                return MS_SUCCESS;
            }

            Entity * registerOrFree(const std::string& name, Entity * entity) {
                if (registerEntity(name, entity) != MS_SUCCESS) {
                    delete entity;
                    return nullptr;
                }

                entity->owner = this;

                return entity;
            }

            Entity * getEntity(const std::string& name) const {
                return members.get(name, nullptr);
            }

            bool isLocal(const std::string& name) const {
                return members.containsKey(name);
            }

            state deleteEntity(const std::string& name) {
                bool b = members.remove(name);

                return b == MS_SUCCESS;
            }

            // ValueInfo * getValueInfo(const std::string& name, const std::string& keyIdentifier = "") {
                
            //     /*

            //     TODO: use 'keyIdentifier' for objects, tuples, maps and arrays

            //     */
                
            //     return variables.get(name);
            // }

            bool containsMember(const std::string& name) {
                return members.containsKey(name);
            }

            bool isModuleLevel() {
                return owner == nullptr;
            }

            virtual uindex requiredMemoryCells() const {
                return internalOffset;
            }

            uindex currentAddress() {
                return addressOffset + internalOffset;
            }

            uindex lastAddress() {
                return addressOffset + internalOffset + 1;
            }

            // __main__ (module)
            // |- print (function)
            //    |- x (param)
            std::string toStructureString(int depth = 0) const {
                std::stringstream buffer;
                char a = (char) 192; // bottom right edge
                char b = (char) 179; // vertical bar
                char c = (char) 195; // vertical bar with side bar

                using entry = Map<std::string, Entity*>::MapPair;
                std::string space {std::string(depth, ' ')};
                int index = 0;

                if (depth >= 0)
                    buffer << name << " (" << typeStr(type) << ") " << level << "-" << address << "\n";

                for (entry& e : members) {
                    if (e.unset)
                        continue;

                    buffer << space;
                    buffer << c;

                    if (e.value->isNamespace()) {
                        buffer << e.value->ns()->toStructureString(depth + 1);
                    } else
                        buffer << e.value->name << " (" << typeStr(e.value->type) << "), " << e.value->address << "%" << e.value->localAddress << "\n";
                }

                return buffer.str();
            }
        };

        struct Module : public Namespace {
            Code * codeFile {nullptr};
            ModuleState state;
            uindex globalCount {0};
            int index {-1};

            Module(Entity * owner, EntityType type = EntityType::NAMESPACE, const std::string& name = "") :
                Namespace(owner, type, name), state(ModuleState::TOKENIZED) {}
            Module() :
                Namespace(nullptr, EntityType::MODULE, std::string("module_" + std::to_string(moduleIndex++))),
                state(ModuleState::TOKENIZED), globalCount(0), index(moduleIndex++) {}
        };

        struct Variable : public Entity {

            VariableType varType {VariableType::UNDEFINED};
            TypeSpec dataType {DataType::UNDEFINED};

            Variable(Namespace* owner, const std::string& name = "", VariableType variableType = VariableType::UNDEFINED, DataType type = DataType::UNDEFINED) :
             Entity(owner, EntityType::VARIABLE, name) {
                varType = variableType;
                dataType = type;
            }

            bool isLocal() const {
                return varType == VariableType::LOCAL || varType == VariableType::PARAM;
            }

            std::string toString() const {
                std::stringstream buf;

                buf << "{";
                buf << "name: " << name << ", ";
                buf << "type: " << stringify::toString(varType, "-") << ", ";
                buf << "data: " << stringify::toString(dataType, "-") << ", ";
                buf << "owner: " << (owner ? owner->name : "-") << ", ";
                buf << "address: " << localAddress;
                buf << "}";

                return buf.str();
            }

        };

        // *** Function ***

        // Function parameter description
        struct FParam {

            /* Each parameter has an index which is also the local address in the StackFrame.
            * This value is automatically assigned by the owning function.
            */
            int index {0};

            /* Parameter name; also local variable in function namespace. */
            std::string name;

            /* Parameters can be assigned to a data type. This will generate some
            * guard code checking the given data type and throwing an error if invalid.
            */
            DataType guard {DataType::UNDEFINED};

            /* Parameters can be assigned to a default value. This value can automatically
            * put the guard type to the literal type. Default values are expressed through
            * literal indices which are looked up on runtime.
            */
            int defaultLiteral {-1};

            /* The last parameter can be a VAR-arg, meaning, that a not defined amount of
            * parameters can be put on function call.
            */
            bool varg {false};

            FParam(const std::string_view pname = "", int literalIndex = -1, DataType type = DataType::UNDEFINED, bool varArg = false) :
                name(pname), defaultLiteral(literalIndex), guard(type), varg(varArg) {}

            // required to be iterated over in a std::map<FParam, V>
            friend int operator <(FParam another, FParam ui) {
                return another.index - ui.index;
            }
            
            bool isValid() const {
                return isValidIdentifier(name);
            }

            void clear() {
                index = 0;
                name = "";
                guard = DataType::UNDEFINED;
                defaultLiteral = -1;
                varg = false;
            }

            std::string sourceString() const {
                std::stringstream buf;

                buf << name;

                if (defaultLiteral != -1)
                    buf << ": $" << defaultLiteral;

                if (varg)
                    buf << "...";

                return buf.str();
            }

            friend std::ostream& operator <<(std::ostream& stream, const FParam& param) {
                return stream << "{index: " << param.index << ", name: " << param.name
                         << ", guard: " << traits::stringify::toString(param.guard, "-") << ", literal: "
                         << param.defaultLiteral << ", varg: " << param.varg << "}";
            }
        };

        enum FMod : short {
            DEFAULT     = 0,

            // -- prefix
            EXTERN      = 1,
            FINAL       = 2,
            SYNC        = 4,
            LOCAL       = 8, // function only be access through the owning namespace
            // reserved for 16, 32 - as prefix mods

            // -- suffix
            OVERRIDE    = 64
        };

        /* A function is a meta container for all relevant information on parsing time.
         * To describe the abstract type of a function, following attributes are set:
         *  
         *  - startIndex: operation index to call
         *  - paramCount: amount of parameters
         *  - modifiers: special properties (extern)
         *  - params: parameter name -> index mapping
         *  - locals: local variables -> index mapping
         */
        struct Function : public Namespace, public Callable {

            /* The start index serves as the anchor point in the operation set
            * where this function starts. When compiled this will be put
            * as the parameter for the 'call' operation.
            */
            uindex startIndex { static_cast<uindex>(-1) };
            
            int label {-1};

            /* The parameter count specifies how many values will be popped off
            * the stack - and therefore, must be pushed beforehand.
            */
            usize paramCount {0};

            int fixedParamCount {0};

            /* Functions may have certain modifiers set as a keyword in front of their name:
            *  (Note: modifiers are inherited from Entity)
            *
            *  - extern: signals the compiler that this function does not contain a body
            *            nor later definition. Its logic is executed by the interpreter
            *            in the C language. There must be a listener for each extern function
            *            to handle the invocation. It is used for system functions,
            *            OS specific calls or generally all requests which can't be executed
            *            in MS.
            */
            
            // extBinding is set if this function has its body not realised in ms
            ext::ExtLink * extLinking { nullptr };

            // true if the function returns a value in every case
            bool containsReturn {false};
            bool returnVoid {false};
            bool vararg {false};

            // Function mods
            XMods<short, FMod> mods;

            // A parameter name to index mapping.
            std::vector<FParam> params;

            // A local value to index mapping.
            std::vector<std::string> locals;

            // Resturn type of the function.
            TypeSpec resultType {DataType::UNDEFINED};

            Function() : Namespace(nullptr, EntityType::FUNCTION), Callable(-1) {
                mods.stringMap.put(FMod::DEFAULT, "default");
                mods.stringMap.put(FMod::EXTERN, "extern");
                mods.stringMap.put(FMod::FINAL, "final");
                mods.stringMap.put(FMod::SYNC, "sync");
                mods.stringMap.put(FMod::OVERRIDE, "override");
                mods.stringMap.put(FMod::LOCAL, "local");
            }
            Function(Entity * owner, const std::string& initName, uindex startIndex_, uindex initparamCount) :
                Namespace(owner, EntityType::FUNCTION, initName), Callable(initparamCount), startIndex(startIndex_), paramCount(initparamCount) {
                    address = startIndex;

                    mods.stringMap.put(FMod::DEFAULT, "default");
                    mods.stringMap.put(FMod::EXTERN, "extern");
                    mods.stringMap.put(FMod::FINAL, "final");
                    mods.stringMap.put(FMod::SYNC, "sync");
                    mods.stringMap.put(FMod::OVERRIDE, "override");
                    mods.stringMap.put(FMod::LOCAL, "local");
                }

            Function& operator =(const Function& f) = default;

            state registerParam(const std::string_view name, int defaultLiteral = -1, DataType guard = DataType::UNDEFINED, bool vararg = false) {
                for (const FParam& p : params) {
                    if (p.name == name)
                        return MS_ERROR_ENTITY_ALREADY_REGISTERED;
                }

                std::string sname = std::string(name);

                if (!registerOrFree(sname, new Variable(this, sname, vararg ? VariableType::VARG_PARAM : VariableType::PARAM, guard)))
                    return MS_ERROR_ENTITY_ALREADY_REGISTERED;
                
                FParam param { name, defaultLiteral, guard, vararg };
                param.index = params.size();

                params.push_back(param);

                if (!vararg) {
                    // If the current parameter has no default value (literal == -1)
                    // it MUST be given on function call and is NOT optional.
                    if (param.defaultLiteral == -1)
                        fixedParamCount++;
                    
                    paramCount++;
                }

                return MS_SUCCESS;
            }

            /* Since every function is generic when not all parameters have
             * a type guard assigned, the resulting type must be computed
             * on a 'return' statement.
             */
            DataType getResultingType(const std::vector<DataType>& callTypes) {
                // TODO: ReturnExpression::GetType(callTypes);
            }

            // Accumulated count of defined paramters in the source.
            // This may varies from 'paramCount' (without var-arg) and
            // 'fixedParamCount' (all params without a default literal).
            int getDefinedParamCount() {
                if (mods.isset(FMod::EXTERN) && vararg) {
                    return paramCount + 1;
                }

                return paramCount;
            }

            std::string getSignature() const {
                std::stringstream buf;

                buf << name;
                buf << "(";

                for (int i = 0; i < params.size(); i++) {
                    buf << params[i].sourceString();

                    if (i < params.size() - 1)
                        buf << ", ";
                }

                buf << ")";

                return buf.str();
            }

            std::string toString() const {
                std::stringstream buf;

                buf << "{name: " << name << ", ";
                buf << "mods: [" << mods << "], ";
                buf << "start: " << std::hex << "0x" << startIndex << ", ";
                buf << "isVoid: " << std::boolalpha << isVoid() << ", ";
                buf << "containsReturn: " << containsReturn << ", ";
                buf << "returnType: " << traits::stringify::toString(resultType) << ", ",
                buf << "paramCount: " << paramCount << ", ";
                buf << "params: [";

                for (int i = 0; i < params.size(); i++) {
                    buf << params[i];
                    
                    if (i < (params.size() - 1))
                        buf << ", ";
                }

                buf << "]}";

                return buf.str();
            }

            // Might differ on external functions!
            bool isVoid() const {
                return !containsReturn || returnVoid || resultType == DataType::VOID;
            }
        };

        /* Function Invocation Information
         *  - can be used to resolve the return type of a function
         *    on compile time using the appropriate return statement << TODO!
         */
        struct FCall {

            struct CallParam {

                TypeSpec spec;
                int usedLiteral;
                Pair<int, int> debugRef;

                CallParam() : debugRef(-1, -1) {}
                CallParam(TypeSpec type, int defaultLiteral, int begin, int end) : spec(type), usedLiteral(defaultLiteral), debugRef(begin, end) {}

            };

            /* Reference to the owning function of this call. */
            Function* owner {nullptr};

            /* All given parameters at the call. Might be more than
             * the fixed paramter count on varg calls.
             */
            std::vector<CallParam> params;

            Code* code {nullptr};

            /* Location in the source code where the call appeared. */
            CodePoint debugRef;

            FCall() {}

            TypeInfo info() {
                return TypeInfo {};
            }

            friend std::ostream& operator <<(std::ostream& stream, const FCall& fcall) {     
                if (fcall.owner && fcall.code) {
                    stream << (fcall.code != nullptr ? fcall.code->getModuleName() : "?") << ".ms|";
                    stream << (fcall.debugRef.line + 1) << ":" << (fcall.debugRef.column + 1);
                    stream << " -> ";
                    stream << fcall.owner->name;
                    stream << "(";

                    for (int i = 0; i < fcall.owner->params.size(); i++) {
                        stream << fcall.owner->params[i].name << ": ";
                        stream << traits::stringify::toString(fcall.params[i].spec);
                        stream << " = ";
                        stream << fcall.code->getContent(fcall.params[i].debugRef.a, fcall.params[i].debugRef.b); // from .. to

                        if (i < fcall.owner->params.size() - 1)
                            stream << ", ";
                    }

                    stream << ")";
                } else
                    stream << "{invalid FCall}";

                return stream;
            }

        };

        // *** Object & Proto ***

        struct ObjectEntry {
            std::string name;
            TypeSpec type;
        };

        struct Object : public Namespace {

            std::vector<ObjectEntry> entries;

            Object() : Namespace(nullptr, EntityType::OBJECT) {}
        };

        struct Proto : public Object {
            std::vector<ProtoCallback> callbacks;

            Proto() {}
        };

        // *** Loops ***

        enum class LoopType {
            UNKNOWN,

            WHILE_DO,
            DO_WHILE,

            FOR_UNTIL,
            FOR_FROM_TO,
            FOR_IN
        };

        struct Loop : public Entity {

            LoopType loopType;

            Loop(LoopType type, Entity* owner = nullptr) : Entity(owner, EntityType::LOOP), loopType(type) {}

        };

        struct FromToLoop : public Loop {

            /* The variable of which the loop operates on which is
             * specified after the 'for' token. This can be a newly
             * created or already existing local or global variable.
             * 
             * MUST be an identifer within the module!
             */
            Variable* iterator {nullptr};

            /* Index of the 'from' token to read the assigning
             * expression for the iterator variable.
             */
            uindex fromToken {0};

            /* The index of the 'end' token to read the condition
             * of type 'iterator < condition'.
             */
            uindex toToken {0};

            /* True, if after the 'to' token a logical expression
             * follows instead of a deduced single type. By default,
             * this is false since this loop type automatically creates
             * a comparison between the iterator type and the deduced
             * type. Hence, these types should be equal in this case.
             */
            bool customCondition {false};

            FromToLoop() : Loop(LoopType::FOR_FROM_TO) {}
            virtual ~FromToLoop() {}

        };

        //
        // -- Clause
        //

        struct Clause {
            bool block {false};
            int label {-1};
            int codePoint {0};
            Entity* entity {nullptr};
            ClauseType type { ClauseType::NONE };
            Clause* parentClause {nullptr};

            Clause(Clause* parent, ClauseType clauseType, Entity* owner) : type(clauseType), parentClause(parent), entity(owner) {}
            virtual ~Clause() {}

            virtual state enter(Context& ctx) { return MS_SUCCESS; }
            
            virtual state leave(Context& ctx) { return MS_SUCCESS; }
        };

        template <typename E, ClauseType T>
        struct XClause : public Clause {
            //E* entity {nullptr};

            XClause(E* owner, Clause* parent = nullptr) : Clause(parent, T, owner) {}

            E* getEntity() const {
                return static_cast<E*>(entity);
            }
        };

        // -- Utility Functions

        // Build Type Specs

        TypeSpec makeTypeSpec(FCall& call) {
            return TypeSpec { DataType::FCALL, call.info() };
        }

        // ...

        std::string_view getEntityName(const Entity* ent, const std::string_view fallback) {
            if (!ent)
                return fallback;

            return ent->name;
        }

        std::string toString(const Entity* ent, const std::string& fallback) {
            if (ent)
                return ent->toString();

            return fallback;
        }

        bool isTerminator(const std::string_view value) {
            return value == "end" || value == "def" || value == "return" ||
                    value == "global" || value == "debug" || value == "if" || value == "is" ||
                    value == "'" || value == "--" || value == "let" || value == "=";
        }

        bool isArithmeticSymbol(const std::string_view value) {
            return value == "+" || value == "-" ||
                    value == "*" || value == "/";
        }

        bool isLogicalOperator(const std::string_view value) {
            return value == "and" || value == "or" || value == "is" || value == "not" || value == "<>"; // add <, >,
        }

        // NOTE: do NOT allow '--' or '++' since '--' indicates a comment!
        bool isOperator(const std::string_view value) {
            return value == "'" ||
                    value == "." || value == "," ||
                    value == "(" || value == ")" || 
                    value == "[" || value == "]" ||
                    value == "{" || value == "}" ||
                    value == ":" || value == "::" ||
                    value == "=" || value == "is" || value == "&" ||
                    value == "and" || value == "or" || value == "not" ||
                    value == "<" || value == "<=" || value == ">" || value == ">=" || value == "<>" ||
                    value == "+" || value == "-" || value == "*" || value == "/" ||
                    value == "$" || value == "#"; // ...
        }

        bool isOpeningParenthesis(const std::string_view value) {
            return value == "(" || value == "[" || value == "{" || value == "<";
        }

        bool isClosingParenthesis(const std::string_view value) {
            return value == ")" || value == "]" || value == "}" || value == ">";
        }

        bool isParenthesis(const std::string_view value) {
            return isOpeningParenthesis(value) || isClosingParenthesis(value);
        }

        bool isKeyword(const std::string_view value) {
            return false;
        }

        bool isValidIdentifier(const std::string& identifier) {
            if (identifier.length() <= 0)
                return false;
            
            for (char c : identifier) {
                std::string val = std::string(1, c);

                if (isOperator(val))
                    return false;
            }

            return !isTerminator(identifier) && !isNumber(identifier) && !isNumber(identifier, true) && !isKeyword(identifier);
        }

        // currently not possible: func()[index], use a = func(), b = a[index] instead
        uindex findExpressionEnd(const Sequence<Token>& tokens, uindex start, usize bindingRange = 2) {
            bool str = false, closedString = false;
            int r = 0;

            for (uindex i = start; i < tokens.size(); i++) {
                const std::string& content = tokenContent(tokens, i);

                // only look for operators
                if (isOperator(content)) {
                    // a string literal must be closed appropiately
                    if (content == "'") {
                        if (!str)
                            str = true;
                        else {
                            closedString = true;
                            str = false;
                        }
                    }

                    // ')' and ' MUST be followed by an operator and not a terminator value
                    if ((isClosingParenthesis(tokenContent(tokens, i)) || closedString) &&
                        !isOperator(tokenContent(tokens, i + 1))) {
                    /*
                        !isArithmeticSymbol(tokenContent(tokens, i + 1)) &&
                        !isClosingParenthesis(tokenContent(tokens, i + 1)) &&
                        !isLogicalOperator(tokenContent(tokens, i + 1)) &&
                        // TODO: disabled: tokenContent(tokens, i + 1) != "[" && // a () can be followed by [] if a functions returns an array/object
                        tokenContent(tokens, i + 1) != "." &&
                        tokenContent(tokens, i + 1) != "#") 
                        
                    */
                        
                        return i + 1; // expression end is certain; return ) + 1
                    }                 

                    r = 0;
                    closedString = false;
                } else if (isTerminator(tokenContent(tokens, i)) || isKeyword(tokenContent(tokens, i)))
                    return i;
                else
                    r++;

                if (r == bindingRange)
                    return i;
            }

            return tokens.size();
        }

        // TODO: working?
        uindex findExpressionStart(const Sequence<Token>& tokens, uindex start, usize bindingRange = 2) {
            int r = 0;

            for (int i = tokens.size() - 1; i >= 0; i--) {
                const std::string content = tokenContent(tokens, i);

                // only look for operators
                if (isOperator(content)) {
                    // a ')' must be followed by a certain set of operators
                    if (tokenContent(tokens, i) == ")" &&
                        !isArithmeticSymbol(tokenContent(tokens, i + 1)) &&
                        tokenContent(tokens, i + 1) != ")" &&
                        tokenContent(tokens, i + 1) != ".") {
                        
                        return i + 1; // expression end is certain; return ) + 1
                    }

                    r = 0;
                } else if (isTerminator(tokenContent(tokens, i)) || isKeyword(tokenContent(tokens, i)))
                    return i;
                else
                    r++;

                if (r == bindingRange)
                    return i;
            }

            return tokens.size();
        }

        uindex findEntityPathBegin(const Sequence<Token>& tokens, uindex begin) {
            int exit = 1, c {0};
            bool scope {false};

            for (int i = begin - 1; i >= 0; i--) {
                const Token& t = *tokens.get(i);

                if (t == ".")
                    c = 0;

                if (t == "]")
                    scope = true;

                if (t == "[") {
                    scope = false;
                    c = 0;

                    continue;
                }

                // Found invalid position this returning the last valid one.
                if (c == exit)
                    return i + 1;

                if (!scope && t != ".")
                    c++;
            }

            return 0;
        }

        index findScopeEnd(const Sequence<Token>& tokens, uindex begin, std::string_view openCharacter, std::string_view closeCharacter) {
            int depth = 0;

            for (int i = begin; i < tokens.size(); i++) {
                if (tokens.get(i)->content == openCharacter)
                    depth++;
                else if (tokens.get(i)->content == closeCharacter) {
                    depth--;

                    if (!depth)
                        return i;
                    else if (depth < 0)
                        return -1;
                }
            }

            return tokens.size();
        }

        // -- For Loops

        enum class ForLoopType {
            INVALID,
            UNTIL,
            FROM_TO
        };

        std::pair<uindex, ForLoopType> findForLoopBounds(Context& ctx, Code& code, uindex begin) {
            for (; begin < code.getTokens().size(); begin++) {
                if (code.getContent(begin) == "do")
                    return std::make_pair(begin, ForLoopType::INVALID);
                else if (code.getContent(begin) == "until")
                    return std::make_pair(begin, ForLoopType::UNTIL);
                else if (code.getContent(begin) == "from")
                    return std::make_pair(begin, ForLoopType::FROM_TO);
            }

            return std::make_pair(begin, ForLoopType::INVALID);
        }

        // -- Functions

        /* DataType Combination Matrix

            UNDEFINED   = 0,
            INVALID     = 1,
            NIL         = 2,

            INTEGRAL    = 3,
            DECIMAL     = 4,
            STRING      = 5,
            REF         = 6,
            OBJECT      = 7,
            PROTO       = 8,
            ARRAY       = 9,
            CONTAINER   = 10,

            VOID        = 11, <-- TODO

        */
        DataType getResultingType(DataType left, DataType right) {
            static const unsigned int dimension = (unsigned int) DataType::__count__;
            static const int matrix[dimension][dimension] =
                { //    L\R   u0 i1 n2 i3 d4 s5 r6 o7 p8 a9  c10 v11
                  /* u0 */  { 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,  0,  11 }, // u0
                  /* i1 */  { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  1,  1  }, // i1
                  /* n2 */  { 0, 1, 2, 1, 1, 5, 1, 7, 1, 1,  10, 2  }, // n2
                  /* i3 */  { 0, 1, 1, 3, 4, 5, 1, 7, 1, 1,  10, 1  }, // i3
                  /* d4 */  { 0, 1, 1, 4, 4, 5, 6, 7, 1, 1,  10, 1  }, // d4
                  /* s5 */  { 0, 1, 5, 5, 5, 5, 5, 5, 5, 5,  5,  1  },
                  /* r6 */  { 0, 1, 1, 3, 4, 5, 6, 7, 8, 10, 10, 1  },
                  /* o7 */  {},
                  /* p8 */  {},
                  /* a9 */  {},
                  /* c10 */ {}
                };

            return static_cast<DataType>(matrix[(unsigned int) left][(unsigned int) right]);
        }

        TypeSpec getResultingType(TypeSpec left, TypeSpec right) {
            TypeSpec ts;

            ts.dataType = getResultingType(left.dataType, right.dataType);
            ts.refType = getResultingType(left.isRef() ? left.refType : left.dataType,
                                        right.isRef() ? right.refType : right.dataType);

            return ts;
        }

        opcode::Op getOppositeJump(Operation op) {
            using opcode::Op;

            switch (op) {
                case Operation::EQUALS: return Op::jnz;
                case Operation::NOT_EQUALS: return Op::jz;
                case Operation::LESS_THAN: return Op::jgez;
                case Operation::LESS_THAN_EQUALS: return Op::jgz;

                default:
                    return Op::IIS;
            }

            return Op::IIS;
        }

        Module* createModule(Code& code) {
            Module* ns {nullptr};

            try {
                ns = new Module(nullptr, EntityType::MODULE, code.getModuleName());
            } catch (...) {
                debug::error("failed to 'createNamespace': out of memory");
            }

            return ns;
        }

    }

    namespace ext {

        using namespace types;
        using namespace lang;

        using Callback = state (*)(comp::Context&, runtime::VM&, Call&);

        class Call {
            friend class ExtLink;
            friend class ExtHandler;
            
            protected:
                std::map<std::string, memory::IData*> data;
                int paramCount {0}; // amount of params given on function call
                int vargCount {0};

            public:
                Call() {}

                memory::IData* getParam(const std::string& name) {
                    if (data.find(name) == data.end())
                        return nullptr;

                    return data[name];
                }

                int getVargCount() const {
                    return vargCount;
                }

                int getParamCountOnCall() const {
                    return paramCount;
                }
        };

        class ExtLink {
            friend class ExtHandler;

            protected:
                int id {-1};
                std::string name;
                DataType returnType { DataType::UNDEFINED };

                int fixedParamCount {0};
                int paramCount {0};

                Callback callback;
                std::vector<std::string> parameters;

            public:
                ExtLink(int ID, const std::string& identifier, DataType retType, const std::vector<std::string>& params, const Callback& call) :
                    id(ID), name(identifier), returnType(retType), parameters(params), callback(call) {}

                state call(comp::Context& ctx, runtime::VM& vm, Call& call, int pcount) {
                    call.paramCount = pcount;
                    call.vargCount = pcount - paramCount;

                    return callback(ctx, vm, call);
                }

                const bool isPartner(Function* function) const {
                    if (!function)
                        return false;
                    
                    if (function->name == name && function->getDefinedParamCount() == parameters.size())
                        return true;

                    return false;
                }

                const std::string& getIdentifier() const {
                    return name;
                }

                const int getId() const {
                    return id;
                }

                const std::string getParam(int index) const {
                    if (index < 0 || index >= parameters.size())
                        return std::string("[" + std::to_string(index) + "]");

                    return parameters[index];
                }

                const int getParamCount() const {
                    return parameters.size();
                }

                const DataType getReturnType() const {
                    return returnType;
                }
        };

        class ExtHandler {
            protected:
                std::vector<ExtLink*> linkings;

            public:
                ExtHandler() {}

                state bind(const std::string& name, DataType returnType, const std::vector<std::string>& params, const Callback& callback) {
                    for (ExtLink* lnk : linkings) {
                        if (lnk && lnk->getIdentifier() == name)
                            return MS_ERROR_DUPLICATE;
                    }

                    try {
                        ExtLink* binding = new ExtLink(linkings.size(), name, returnType, params, callback);

                        debug::printsf("$2[EXT] Bound function $u%%$r$2 (%% param(s)) to id $b%%", name, params.size(), binding->getId());

                        linkings.push_back(binding);
                    } catch (...) {
                        return MS_ERROR;
                    }

                    return MS_SUCCESS;
                }

                state linkFunction(Function* function) {
                    if (!function)
                        return MS_ERROR_NULL;

                    if (function->extLinking)
                        return MS_SUCCESS;

                    for (ExtLink* lnk : linkings) {
                        if (lnk && lnk->isPartner(function)) {
                            function->extLinking = lnk;
                            function->resultType = lnk->getReturnType();

                            if (lnk->getReturnType() != DataType::VOID) {
                                function->containsReturn = true;
                                function->returnVoid = false;
                            }

                            lnk->paramCount = function->paramCount;
                            lnk->fixedParamCount = function->fixedParamCount;

                            debug::printsf("$2[EXT] Linked function $u%%$r$2 successfully.", function->name);

                            return MS_SUCCESS;
                        }
                    }

                    return MS_ERROR_NO_EXT_LINKING;
                }

                state call(comp::Context& ctx, runtime::VM& vm, int id, int pcount);
        };

    }

    namespace comp {

        using namespace types;
        using namespace util;
        using namespace lex;
        using namespace lang;
        using namespace opcode;
        using namespace memory;

        // -- Declarations

        class Expression;
        class Context;

        state parseEntity(Context& ctx, Expression& expr, Code& code, int from, int to);
        state parseExpression(Context& ctx, Expression& expr, Code& code, int begin, int end, int minPrecedence = 0);
        state parseFunction(Context& ctx, Code& code, Function& f, int& i);
        state parseObjectLiteral(Context& ctx, Code& code, int begin, int end);
        state parse(Context& ctx, Code& code);

        state readInFile(Context& ctx, const std::string& fileName, Code* result = nullptr);
        state compile(Context& ctx, Code* code = nullptr);

        // -- Definitions

        class ConstValuePool : MemoryContainer {
            private:
                std::vector<IData*> data;

            public:
                ConstValuePool() : MemoryContainer("ConstValuePool") {}
                ~ConstValuePool() {
                    for (const auto& x : data)
                        delete x;

                    data.clear();

                    debug::printsf("`4~ConstValuePool");
                }

                int append(IData* value, bool allowNullptr = true) {
                    if (value || allowNullptr) {
                        data.push_back(value);
                        value->owner = this;

                        return data.size() - 1;
                    }

                    return -1;
                }

                void put(int index, IData* value, bool override = true) {
                    if (validIndex(index)) {
                        if (override && data[index])
                            memory::freeData(data[index]);

                        data[index] = value;
                        data[index]->owner = this;
                    }
                }

                DataType getDataType(int index) const {
                    if (validIndex(index))
                        return static_cast<DataType>(data[index]->getType());

                    return DataType::INVALID;
                }

                IData* getValue(int index) const {
                    if (validIndex(index))
                        return data[index];

                    return nullptr;
                }

                bool validIndex(int index) const {
                    return index >= 0 && index < data.size();
                }

                int getCount() const {
                    return data.size();
                }
        };

        class EntityNameResolver {
            private:
                std::stringstream buf;

                void clearbuf() { buf.str(""); }

            public:
                state pushEntityAddress(Context& ctx, Code& code, int begin, int end, const Namespace* base);
        };

        // TODO: move to top
        #define MS_EXPR_CHECK_STATES() if (left != MS_SUCCESS) return left; \
                                        if (right != MS_SUCCESS) return right;

        #define MS_EXPR_SETUP(expr, taskLeft, taskRight) \
            leftExpr = expr.setupLeft(taskLeft);         \
            rightExpr = expr.setupRight(taskRight);

        #define MS_EXPR_DO(op)                                              \
            MS_EXPR_SETUP(expr, 0, 0)                                       \
            MS_ASSERT_EXPR(ctx, *leftExpr, code, begin, i, left, op)        \
            MS_ASSERT_EXPR(ctx, *rightExpr, code, i + 1, end, right, op)    \
            MS_EXPR_CHECK_STATES()

        #define MS_EXPR_TASK_PARSE_PARAMS 0
        #define MS_EXPR_TASK_PUSH_ATTRIBUTE 1

        /* Expressions?!?
         *
         *    expr00) 2           -> literal
         *    expr01) 2 + 2       -> arithmetic 'add'
         *    expr02) 2 * 2       -> arithmetic 'mul'
         *    expr03) f           -> function call
         *    expr04) T           -> constructor call
         *    expr05) (1, 2, a)   -> listing (depending on parent) -> function params / proto params
         *    expr0) 2 + 2 * 3    -> complex
         * 
         *    // Structure of expressions (resolved from LEAF to ROOT (bottom to top); horizontal = defined by operator)
         * 
         *    ........... 1 + 2 + 3
         *                 /|\
         *                1 + 2 + 3         .. + -> Expression(arithmetic 'add'): left=literal, right=arithmetic 'add'
         *                     /|\
         *                    2 + 3         .. + -> Expression(arithmetic 'add'): left=literal, right=literal
         * 
         *    ........... 1 + f(2 * 3, a)
         *                 /|\
         *                1 + f(2 * 3, a)   .. + -> Expression(arithmetic 'add'): left=literal, right=call
         *                    /|\
         *                   f (2 * 3, a    .. ( -> Expression(call): left=reference (*function -> FUNCTION call), right=listings (req params)
         *                          /|\
         *                     2 * 3 , a    .. , -> Expression(listing (req params)): left=arithmetic 'add', right=reference
         *                      /|\
         *                     2 * 3        .. * -> Expression(arithmetic 'mul'): left=literal, right=literal
         * 
         *     // Sypnosis of Expression
         *  
         *    type:       ExpressionType
         *    operation:  Operation
         *    task:       int
         *    depth:      int
         *    subCount:   int             -- total amount of sub-expressions (fused value with children)
         *    context:    Namespace*
         *    parent:     Expression*
         *    left:       Expression*
         *    right:      Expression*
         *    info:       ExpressionInfo
         *    flags:      XMods<short, ExpressionFlag>
         * 
         *    #DEBUG
         *        tokenBegin:     int
         *        tokenEnd:       int
         *        tokenOperator:  int
         *    #END
         *    
         *    // Sypnosis of ExpressionInfo
         *    
         *    literalIndex:   int
         *    reference:      Entity*
         *    listedCount:    int
         * 
         *    #DEBUG
         *        terminatorValue:    std::string
         *    #END
         * 
         */

        enum class ExpressionType {
            UNKNOWN = 0,
            LITERAL,
            REF,

            ARITHMETIC_ADD,
            ARITHMETIC_MUL,
            ARITHMETIC_ANY, // use Operation as reference

            COMMA_ENUM,
            FUNC_CALL,

            PARENTHESIS_0

        };

        enum class ExpressionFlag : short {
            ONLY_REFERENCES             = 1,
            EMPTY_EXPRESSIONS_ALLOWED   = 2,
            VOID_RESULT_ALLOWED         = 4,
            VOID_RESULT_REQUIRED        = 8
        };

        struct ExpressionInfo {
            int literalIndex {-1};
            int listedCount {0};
            Entity* reference {nullptr};

            //#ifdef MS_DEBUG
                std::string terminatorValue;
            //#endif
        };

        struct ExpressionResult {
            TypeSpec type {DataType::UNDEFINED};
            int protoId {-1};
        };

        // For comma expressions in function calls, arrays and objects
        struct ExpressionEnum {
            bool enabled {false};
            bool captureOnlyLeft {true};
            std::vector<Expression*> expressions;
        };

        struct Expression {
            Namespace* context {nullptr};

            Expression* parent {nullptr};
            Expression* left {nullptr};
            Expression* right {nullptr};

            ExpressionEnum xenum;

            ExpressionType type {ExpressionType::UNKNOWN};
            Operation operation {Operation::NONE};
            int task {-1};
            int depth {0};
            int subCount {1};
            int negation {1};
            int precedence {0};
            bool appendNegationInstruction {true};
            bool terminator {false};
            bool assignmentIncluded {false}; // op <target>
            bool empty {false};

            Entity* target {nullptr};
            ExpressionInfo info;
            ExpressionResult result;
            XMods<short, ExpressionFlag> flags;

            //#ifdef MS_DEBUG
                int tokenBegin {0}, tokenEnd {0}, tokenOperator {0};
            //#endif

            //
            Expression(int defTask = 0, std::initializer_list<ExpressionFlag> defFlags = {ExpressionFlag::EMPTY_EXPRESSIONS_ALLOWED}) :
                task(defTask), flags(defFlags) {}
            
            virtual ~Expression() {
                if (left && !xenum.enabled) {
                    delete left;
                    left = nullptr;
                }

                if (right && (!xenum.enabled || xenum.captureOnlyLeft)) {
                    delete right;
                    right = nullptr;
                }
            }

            void combineChildren() {
                if (left)
                    negation *= left->negation;

                if (right)
                    negation *= right->negation;
            }

            void combineResultType(Expression* other) {
                if (other) {
                    // int, dec -> dec
                    // str, dec -> str
                    // arr, str -> arr

                    result.type = lang::getResultingType(result.type, other->result.type);
                }
            }

            Expression* setupLeft(int task = 0, std::initializer_list<ExpressionFlag> flags = {ExpressionFlag::EMPTY_EXPRESSIONS_ALLOWED}) {
                if (left)
                    delete left;
                
                left = new Expression(task, flags);
                subCount++;
                
                setupChild(left);

                if (xenum.enabled)
                    xenum.expressions.push_back(left);

                return left;
            }

            Expression* setupRight(int task = 0, std::initializer_list<ExpressionFlag> flags = {ExpressionFlag::EMPTY_EXPRESSIONS_ALLOWED}) {
                if (right)
                    delete right;
                
                right = new Expression(task, flags);
                subCount++;
                
                setupChild(right);

                if (xenum.enabled && !xenum.captureOnlyLeft)
                    xenum.expressions.push_back(right);

                return right;
            }

            void setupChild(Expression* expr) {
                expr->parent = this;
                expr->depth = depth + 1;
                expr->context = context; // copy current namespace; may be changed by scope operator
            }

            std::string resultName() {
                if (result.type.isRef())
                    return debug::format("*%%", traits::stringify::toString(result.type.refType));

                return traits::stringify::toString(result.type.dataType);
            }

            bool isLogical() {
                return operation == Operation::EQUALS || operation == Operation::NOT_EQUALS || operation == Operation::LESS_THAN;
            }

            // helpers: negation

            Expression& negate() {
                negation *= -1;

                return *this;
            }

            bool isNegated() const {
                return negation == -1;
            }

            // helpers: flag access

            bool isTerminator() const {
                return terminator;
            }

            bool isEmptyExpressionsAllowed() {
                return flags.isset(ExpressionFlag::EMPTY_EXPRESSIONS_ALLOWED);
            }

            bool isVoidResultAllowed() {
                return flags.isset(ExpressionFlag::VOID_RESULT_ALLOWED);
            }

            bool isVoidResultRequired() {
                return flags.isset(ExpressionFlag::VOID_RESULT_REQUIRED);
            }

            bool isOnlyReferences() {
                return flags.isset(ExpressionFlag::ONLY_REFERENCES);
            }

        };

        /*
        class ExprContext {
            friend state parseEntity(Context& ctx, ExprContext& expr, Code& code, int from, int to);
            friend state parseExpression(Context& ctx, ExprContext& expr, Code& code, int begin, int end, int minPrecedence);
            friend state parse(Context& ctx, Code& code);

            ExprContext* parent {nullptr};
            ExprContext* child {nullptr};

            // The requester is a Callable like a function or Prototype providing
            // the ability to be called and constructed. This reference can be used
            // to get the required argument count or conditions for the expression
            // evaluation. The field is initialized to nullptr by default but
            // might be set on a specific active task.
            Callable* requester {nullptr};

            Namespace* owner {nullptr};
            Entity* result {nullptr};

            int task {0};
            int depth {0};
            int exprCount {1}; // always 1+ EXCEPT on len=0 expressions: will be set by 'parseExpression'
            int funcParams {1};

            bool onlyReferences {false};
            bool emptyExpressionAllowed {true};
            bool voidResultAllowed {false}; // whenever the expression must not return a value
            bool voidResultRequired {false}; // whenever the expression is not allowed to push a value

            public:
                ExprContext(int taskId = 0) : task(taskId) {}

                /*



                DAS MACHST DU GERADE:
                -> ExprContext überarbeiten, sodass sub-expressions (children)
                  einige Informationen der Eltern beibehalten:

                  z.b. stream.foreach():

                   stream . foreach ()
                          |
                   stream               .. getEntity(stream) =: result
                   foreach ()           .. task(PUSH_ATTRIBUTES) (da rechts vom .)
                           |
                   foreach              .. NEUER ExprContext, aber unter Prämisse (PUSH_ATTRIBUTE)!!
                   ()

                    => also:
                        a) testen, ob man ein Kind eines Eltern-Expr-Contexts ist
                        b) wenn ja, abfragen, was die Aufgabe ist, und ob man selbst 'nur' ein Attribut oder ein Entity ist
                           wenn nein, normal nach 'entity' in 'namespace' suchen

                    => dafür TODO:
                        - Konzept mit parent*, child_left*, child_right* aufbauen
                        - dafür Funktion einbinden, damit nicht immer ein neuer ExprContext erstellt werden muss
                        - dynamisches erzeugen und LÖSCHEN!!!
                        - Meta-Daten und DEBUG!!!

                */

                /* Which fields are inherited to a child expression?
                 *
                 *  + p_task -> parent task 
                 * 
                 */
                /*
                void inherit() {
                    
                }

                state enter(int task) {
                    return MS_SUCCESS;
                }
        };
        */

        class CompileTimeFunction : public Entity {

            public:
                CompileTimeFunction(const std::string& name) : Entity(nullptr, EntityType::COMP_FUNC, name) {}

                virtual state execute(Context& ctx, Expression& caller, Expression& params) {}

        };

        /* Stores information of the Context when switching modules. */
        class Snapshot {
            friend class Context;

            private:
                ProgramState programState;
                CodePoint codePoint;

                Code* activeCode;
                Namespace* currentNamespace;

                state compileState {0}, errorState {0}, warnState {0};

                Snapshot(ProgramState pstate, CodePoint point, Code* code, Namespace* ns, state compState, state errState, state warningState) :
                    programState(pstate), codePoint(point), activeCode(code), currentNamespace(ns), compileState(compState), errorState(errState), warnState(warningState) {}
        };

        class Context {
            private:

                friend state parseExpression(Context& ctx, Expression& expr, Code& code, int begin, int end, int minPrecedence);
                friend state parseFunction(Context& ctx, Code& code, Function& f, int& i);
                friend state parse(Context& ctx, Code& code);

                friend state readInFile(Context& ctx, const std::string& fileName, Code* result);
                friend state compile(Context& ctx, Code* code);
            
            private:
                Tokenizer tokenizer;
                InstructionSet instructionSet;
                
                Map<std::string, Code*> codeFiles;
                Code* entryCode {nullptr};
                Code* activeCode {nullptr};

                std::vector<std::string> loadedModules; // import guard
                std::map<std::string, Module*> modules;

                std::map<std::string, CompileTimeFunction*> compFunctions;
                std::vector<FCall*> functionCalls;
                ext::ExtHandler extHandler;

                ProgramState programState { ProgramState::INIT };

            public:
                std::string modulePath;

                CodePoint codePoint {0, 0};
                int tokenCompIndex {-1}; // index in the token sequence, will advance block wise for expression, ...

                state compileState {0}, errorState {0}, warnState {0};

                Namespace* currentNamespace {nullptr};
                Function* currentFunction {nullptr};
                ConstValuePool* literals;
                //std::vector<IData*> literals;
                //int literalIndex {0};

                // current entity clause
                Clause* clause {nullptr};

                Context(const std::string& defaultModulePath = "") :
                    modulePath(defaultModulePath),
                    codeFiles(1), activeCode(nullptr) {

                        // initialize literals
                        literals = new ConstValuePool;

                        debug::printsf("`4Context");
                    }

                virtual ~Context() {
                    // === CLEAN UP ===

                    // delete literals
                    delete literals;

                    // delete program structure
                    for (const auto& module : modules)
                        delete module.second;

                    for (const auto& ctf : compFunctions)
                        delete ctf.second;

                    for (const auto& fcall : functionCalls)
                        delete fcall;

                    for (auto& code : codeFiles) {
                        if (!code.unset && code.value)
                            delete code.value;

                        code.unset = true;
                    }

                    // free the Code; safe since it is not a namespace and won't
                    // trigger the destructor of its assigned module
                    delete activeCode;

                    // Free NIL constant
                    delete memory::NIL;

                    debug::printsf("`4~Context");

                    #ifdef MS_TRACK_IDATA
                        if (IData::TOTAL > 0)
                            debug::printsf("`1Warning: not all allocated data was freed! (%% remaining)", IData::TOTAL);
                    #endif
                }

                // === Debug & Error Handling ===

                std::string getMarkedLine(int line, int marker, int offset = 0, Code* code = nullptr, debug::ChatColor markerColor = debug::ChatColor::FG_GREEN) const {
                    if (code || (code = activeCode))
                        return code->getLine(line).toMarkedString(marker, offset, markerColor);

                    return "";
                }

                std::string getMarkedInterval(int line, int from, int to, int offset = 0, Code* code = nullptr, debug::ChatColor markerColor = debug::ChatColor::FG_GREEN) const {
                    if (to < from)
                        return code->getLine(line).toString();
                    
                    if (code || (code = activeCode))
                        return code->getLine(line).toMarkedIntervalString(from, to, offset, markerColor);

                    return "";
                }

                void printError(state code, const CodePoint point, const std::string_view content, bool showCode = false) {
                    if (code == MS_WARNING)
                        debug::printsf_ignore_debug_mode("$5[Warn@%%] <$r$i$5%%$r$5> %%", traits::stringify::toString(programState), debug::stateCode(code), content);
                    else
                        debug::printsf_ignore_debug_mode("$1[Error@%%] <$r$i$1%%$r$1> %%", traits::stringify::toString(programState), debug::stateCode(code), content);

                    if (showCode) {
                        int offset = 8 + (activeCode ? activeCode->moduleName.length() : 1);

                        offset += (int) std::log10(point.line + 1) + 1;
                        offset += (int) std::log10(point.column + 1) + 1;

                        debug::printsf_ignore_debug_mode("$1(%%.ms|%%:%%) $7%%",
                            (activeCode ? activeCode->moduleName : "-"),
                            point.line + 1, point.column + 1,
                            getMarkedLine(point.line, point.column, offset, activeCode));
                    } else {
                        debug::printsf_ignore_debug_mode("");
                    }
                }

                void printErrorForInterval(state code, const int from, const int to, const std::string_view content, bool showCode = false) {
                    if (code == MS_WARNING)
                        debug::printsf_ignore_debug_mode("$5[Warn@%%] <$r$i$5%%$r$5> %%", traits::stringify::toString(programState), debug::stateCode(code), content);
                    else
                        debug::printsf_ignore_debug_mode("$1[Error@%%] <$r$i$1%%$r$1> %%", traits::stringify::toString(programState), debug::stateCode(code), content);

                    if (showCode && activeCode) {
                        CodePoint start = activeCode->getPointOfIndex(from);
                        CodePoint end = activeCode->getPointOfIndex(to);
                        int offset = 8 + (activeCode ? activeCode->moduleName.length() : 1);

                        offset += (int) std::log10(start.line + 1) + 1;
                        offset += (int) std::log10(start.column + 1) + 1;

                        debug::printsf_ignore_debug_mode("$1(%%.ms|%%:%%) $7%%",
                            (activeCode ? activeCode->moduleName : "-"),
                            start.line + 1, start.column + 1,
                            getMarkedInterval(start.line, start.column, end.column, offset, activeCode));
                    }
                }

                state throwError(state s) { return s; }

                template <typename... Ts>
                state throwError(state s, const std::string& reason, Ts... args) {
                    std::string formatted { debug::sformat(reason.c_str(), args...) };
                    
                    printError(s, codePoint, formatted);

                    return s;
                }

                template <typename... Ts>
                // Throw descripted error, containing the code line
                // where the error occured and a marker.
                state throwd(state s, const std::string& reason, Ts... args) {
                    std::string formatted { debug::sformat(reason.c_str(), args...) };
                    
                    printError(s, codePoint, formatted, true);

                    return s;
                }

                template <typename... Ts>
                // Throw descripted error, containing the code line
                // where the error occured and a marker.
                state throwd(state s, const CodePoint point, const std::string& reason, Ts... args) {
                    std::string formatted { debug::sformat(reason.c_str(), args...) };
                    
                    printError(s, point, formatted, true);

                    return s;
                }

                template <typename... Ts>
                // Throw descripted error, containing the code line
                // where the error occured and a marker.
                state throwd(state s, const int index, const std::string& reason, Ts... args) {
                    std::string formatted { debug::sformat(reason.c_str(), args...) };
                    
                    if (activeCode)
                        printError(s, activeCode->getPointOfIndex(index), formatted, true);
                    else
                        printError(s, codePoint, formatted, true);

                    return s;
                }

                template <typename... Ts>
                // Throw descripted error, containing the code line
                // where the error occured and a marker.
                state throwd(state s, const int begin, const int end, const std::string& reason, Ts... args) {
                    std::string formatted { debug::sformat(reason.c_str(), args...) };
                    
                    printErrorForInterval(s, begin, end, formatted, true);

                    return s;
                }

                template <typename... Ts>
                // Throw descripted error, containing the code line
                // where the error occured and a marker at the NEXT position.
                state throwd1(state s, const std::string& reason, Ts... args) {
                    std::string formatted { debug::sformat(reason.c_str(), args...) };
                    
                    printError(s, CodePoint {codePoint.line, codePoint.column + 1}, formatted, true);

                    return s;
                }

                template <typename T, typename... Args>
                const T& throwAndReturn(const T& elem, state s, const std::string& reason, Args... args) {
                    std::string formatted { debug::sformat(reason.c_str(), args...) };
                    
                    printError(s, codePoint, formatted);

                    return elem;
                }

                // === State Control ===

                // will delete snapshot
                state loadSnapshot(Snapshot*& s) {
                    if (!s)
                        return throwError(MS_ERROR_NULL, "failed to load snapshot: NULL");

                    programState = s->programState;
                    codePoint = s->codePoint;

                    activeCode = s->activeCode;
                    currentNamespace = s->currentNamespace;

                    compileState = s->compileState;
                    errorState = s->errorState;
                    warnState = s->warnState;

                    delete s;
                    s = nullptr;

                    return MS_SUCCESS;
                }

                Snapshot* createSnapshot() {
                    return new Snapshot(programState, codePoint, activeCode, currentNamespace, compileState, errorState, warnState);
                }

                // -- Instruction Blocks

                state enterBlock(Namespace* owner) {
                    state s { MS_SUCCESS };

                    if (!owner)
                        return MS_ERROR_NULL;

                    if ((s = instructionSet.enterLayer(currentNamespace, owner->name, owner->type == EntityType::MODULE)) != MS_SUCCESS)
                        return s;

                    currentNamespace = owner;
                    debug::printsf("ENTER_BLOCK owner=%% level=%%", currentNamespace->name, instructionSet.currentLayer->level);

                    return s;
                }

                state leaveBlock() {
                    if (!instructionSet.currentLayer)
                        return throwd(MS_ERROR_NULL, "no active block to leave");
                    
                    state s = MS_SUCCESS;
                    
                    //if ((s = instructionSet.appendBlock(*instructionSet.currentBlock)) != MS_SUCCESS)
                    //    return s;

                    //if (!instructionSet.currentBlock->ns)
                    //    return throwd(MS_ERROR_NULL, "active block has no namespace stored");
                    
                    // will put 'ns' back to parent
                    //instructionSet.leaveBlock();

                    //if (instructionSet.currentBlock)
                    //    currentNamespace = instructionSet.currentBlock->ns;
                    //else if (activeCode && (currentNamespace = modules.get(activeCode->moduleName, nullptr))) { /* success */ }
                    //else {
                    //    currentNamespace = nullptr;

                    //    return throwd(MS_ERROR_NULL, "no active code nor module to return to");
                    //}

                    Namespace* parent = instructionSet.currentLayer->parent;
                    int level = instructionSet.currentLayer->level;

                    if ((s = instructionSet.leaveLayer()) != MS_SUCCESS) {
                        if (s == MS_ERROR_UNFINISHED_LABEL_APPLICATION)
                            return throwd(s, "cannot leave instruction layer since last instruction required a label application");

                        return throwd(s, "failed to leave instruction layer");
                    }
                    
                    debug::printsf("LEAVE_BLOCK owner=%% level=%%", (parent ? parent->name : "-"), level);
                    currentNamespace = parent;

                    return s;
                }

                // -- State Clauses

                state enterClause(Clause* clause, int codePoint = 0) {
                    if (!clause)
                        return throwd(MS_ERROR_NULL, "clause to enter is null");

                    if (!clause->entity)
                        return throwd(MS_ERROR_NULL, "clause has no entity member");

                    state s = MS_SUCCESS;

                    if (clause->block) {
                        if (!clause->entity->isNamespace())
                            return throwd(MS_ERROR_INTERNAL, "clause-entity must be a namespace");
                        
                        if ((s = enterBlock(clause->entity->ns())) != MS_SUCCESS)
                            return s;
                    }

                    if ((s = clause->enter(*this)) != MS_SUCCESS)
                        return s;

                    if (clause->type == ClauseType::FUNCTION)
                        currentFunction = static_cast<Function*>(clause->entity);

                    clause->parentClause = this->clause;
                    clause->codePoint = codePoint;
                    this->clause = clause;

                    debug::printsf("ENTER_CLAUSE type=%% owner=%%", traits::stringify::toString(clause->type), clause->entity->name);

                    return s;
                }
                
                state leaveClause() {
                    if (!clause)
                        return throwd(MS_ERROR_NULL, "no active clause to leave");

                    if (!clause->entity)
                        return throwd(MS_ERROR_NULL, "active clause has no entity member");

                    state s = MS_SUCCESS;

                    if ((s = clause->leave(*this)) != MS_SUCCESS)
                        return s;

                    if (clause->block) {
                        if (!clause->entity->isNamespace())
                            return throwd(MS_ERROR_INTERNAL, "clause-entity must be a namespace");

                        if ((s = leaveBlock()) != MS_SUCCESS)
                            return s;
                    }

                    if (clause->type == ClauseType::FUNCTION && currentFunction) {
                        debug::printsf(" $7> LEAVE FUNCTION: %%", debug::epxandedJson(currentFunction->toString()));

                        currentFunction = nullptr;
                    }

                    debug::printsf("LEAVE_CLAUSE type=%% owner=%% parent=%%",
                        traits::stringify::toString(clause->type),
                        clause->entity->name,
                        (clause->parentClause && clause->parentClause->entity ? clause->parentClause->entity->name : "-"));

                    delete clause;
                    clause = clause->parentClause;
                    
                    return s;
                }

                // -- Function Handling

                state joinFunction(Function* function);

                // === Code Files & Tokenizing ===

                state storeCode(Code* code) {
                    if (!code)
                        return throwError(MS_ERROR_NULL);
                    
                    if (codeFiles.containsKey(code->moduleName))
                        return throwError(MS_ERROR_DUPLICATE);

                    return (codeFiles.put(code->moduleName, code) != nullptr) ? MS_SUCCESS : MS_FAIL;
                }

                state makeActiveCode(Code* code) {
                    if (activeCode) {
                        if (!codeFiles.containsKey(activeCode->moduleName)) {
                            codeFiles.put(activeCode->moduleName, activeCode);
                        }
                    }

                    activeCode = code;

                    return MS_SUCCESS;
                }

                // === Entity Management ===

                Entity* findEntityInModule(const std::string& name, const Namespace* current = nullptr) {
                    if (!current && !(current = currentNamespace)) // currentNamespace should at least be the init module
                        return nullptr;

                    debug::printsf("REQUEST getEntity(name: %%, namespace: %%)", name, current->name);

                    while (current && !(current->members).containsKey(name)) {
                        if (current->owner && current->owner->isNamespace())
                            current = static_cast<Namespace*>(current->owner);
                        else break;

                        debug::printsf("REQUEST getEntity(name: %%, namespace: %%)", name, current->name);
                    }

                    return current->getEntity(name); // current cannot be nullptr
                }

                Entity* findEntity(const std::string& name, const Namespace* base = nullptr) {
                    // currentNamespace should at least be the init module
                    if (!base && !(base = currentNamespace))
                        return nullptr;

                    // First, lookup special reserved names.
                    // .. none

                    // Second, search from the current namespace up to the module level.
                    Entity* res { findEntityInModule(name, base) };

                    // If no entity for name 'name' was found anywhere in the current module,
                    // search in others beginning already on module level.
                    if (!res) {
                        for (const std::pair<std::string, Module*>& module : modules) {
                            if (module.second && module.second->name != activeCode->getModuleName() // skip already scanned current module
                                && (res = findEntityInModule(name, module.second))) // check the whole module
                                break;
                        }
                    }

                    // If still no entity was found, try to find a compile time function.
                    if (!res) {
                        if (compFunctions.find(name) != compFunctions.end())
                            return compFunctions[name];
                    }

                    MS_IF_DEBUG {
                        if (res)
                            debug::printsf("RESULT -> %%", res->toString());
                        else
                            debug::printsf("RESULT -> unknown variable '%%'", name);
                    }

                    return res;
                }

                // Searches through all modules
                Entity* findEntityInModules(const std::string& name) {
                    Entity* ent { nullptr };

                    for (const std::pair<std::string, Module*>& module : modules) {
                        if (module.second && (ent = findEntity(name, module.second)))
                            break;
                    }

                    return ent;
                }

                state pushEntityAddress(Code& code, int begin, int end, const Namespace* base = nullptr) {
                    EntityNameResolver enr;

                    return enr.pushEntityAddress(*this, code, begin, end, currentNamespace);
                }

                Entity* registerEntity(const std::string& name, EntityType type, Namespace* ns = nullptr) {
                    if (!ns && !(ns = currentNamespace))
                        return nullptr;

                    Entity* e = new Entity(ns, type, name);
                    e = ns->registerOrFree(name, e);

                    return e;
                }

                Variable* registerVariable(const std::string& name, VariableType type, DataType dataType, Namespace* ns = nullptr) {
                    if (!ns && !(ns = currentNamespace))
                        return nullptr;

                    Variable* v = nullptr;

                    try {
                        v = new Variable(ns, name, type, dataType);
                    } catch (...) {
                        throw MSException(MS_FATAL, "out of memory: failed to construct Variable*");
                    }

                    return (Variable*) ns->registerOrFree(name, v);
                }

                // === Literals ===

                index appendLiteral(DataType type, const std::string& content) {
                    IData* data { memory::craftData(type, content) };

                    if (data) {
                        data->applyMods(Modifier::CONST);
                        data->applyMods(Modifier::PROTECTED);
                    } else
                        return throwAndReturn(-1, MS_ERROR_NULL, "failed to create literal for data type '%%' with content '%%'",
                            traits::stringify::toString(type), content);

                    int index = literals->append(data);

                    debug::printsf("Recognized literal %% -> %%", memory::toString(data, true), index);

                    return index;
                }

                index getLiteralIndex() {
                    return literals->getCount();
                }

                // === Context Management ===

                state finalize() {
                    state s = MS_SUCCESS;

                    // when still in clause, return error
                    if (clause) {
                        int pos = clause->codePoint;

                        s = throwd(MS_ERROR, pos, "missing 'end' - unclosed clause"); // TODO: error code
                    }

                    return s;
                }

                Module* getCurrentModule() {
                    if (activeCode)
                        return modules[activeCode->getModuleName()];

                    return nullptr;
                }

                bool isEntryModule() const {
                    return activeCode == entryCode;
                }

                // === Access Methods ===

                ext::ExtHandler& ext() {
                    return extHandler;
                }

                Tokenizer& getTokenizer() {
                    return tokenizer;
                }

                Code* getActiveCode() const {
                    return activeCode;
                }

                InstructionSet& getInstructions() {
                    return instructionSet;
                }

                std::vector<FCall*>& getFunctionCalls() {
                    return functionCalls;
                }
        };

        // -- Clauses (current entity context)

        struct FuncClause : public XClause<Function, ClauseType::FUNCTION> {
            FuncClause(Function* func) : XClause(func) {
                block = true;
            }

            state enter(Context& ctx) {
                label = Instruction::nextLabel();
                
                getEntity()->label = label;
                getEntity()->address = label;

                ctx.getInstructions().placeLabelOnNextInstruction(label);

                debug::printsf("Linked $2$bdef$r to label $2$i%%", label);

                return MS_SUCCESS;
            }

            state leave(Context& ctx) {
                if (!getEntity()->containsReturn || getEntity()->returnVoid)
                    ctx.getInstructions().append(Op::ret);
                else
                    ctx.getInstructions().append(Op::retc);

                return MS_SUCCESS;
            }
        };

        struct IfClause : public XClause<Entity, ClauseType::IF> {
            IfClause() : XClause(nullptr) {}
            IfClause(Entity* owner) : XClause(owner) {}

            state enter(Context& ctx) {
                label = Instruction::currentLabel();

                debug::printsf("Linked $2$bif$r to label $2$i%%", label);

                return MS_SUCCESS;
            }

            state leave(Context& ctx) {
                ctx.getInstructions().placeLabelOnNextInstruction(label);

                return MS_SUCCESS;
            }
        };

        struct ForClause : public XClause<Entity, ClauseType::FOR> {
            ForClause() : XClause(nullptr) {}
            ForClause(Entity* owner) : XClause(owner) {}

            state enter(Context& ctx) {
                label = Instruction::nextLabel();

                debug::printsf("Linked $2$bfor$r to label $2$i%%", label);

                return MS_SUCCESS;
            }

            state leave(Context& ctx) {
                ctx.getInstructions().placeLabelOnNextInstruction(label);

                return MS_SUCCESS;
            }
        };

        struct WhileDoClause : public XClause<Entity, ClauseType::WHILE> {
            int conditionBegin {0};
            Op jmpCmd {Op::NOP};

            WhileDoClause() : XClause(nullptr) {}
            WhileDoClause(Entity* owner, int condBegin, Op jump) : XClause(owner), conditionBegin(condBegin), jmpCmd(jump) {}

            state enter(Context& ctx) {
                // Jump to the loop exit if condition is satisfied.
                ctx.getInstructions().appendJmp(jmpCmd);

                // Use 'currentLabel' since 'appendJmp' already increases the label index.
                label = Instruction::currentLabel();

                debug::printsf("Linked $2$bwhile$r loop label to $2$i%%$r and exit label to $2$i%%", conditionBegin, label);

                return MS_SUCCESS;
            }

            state leave(Context& ctx) {
                // This jump will return to the first instruction of the condition.
                Instruction loop (Op::jmp);

                loop.append(conditionBegin, true);
                ctx.getInstructions().append(loop);

                // Apply the exit label to the first instruction after the loop end.
                ctx.getInstructions().placeLabelOnNextInstruction(label);

                return MS_SUCCESS;
            }
        };

        // -- Member Functions
        
        state Context::joinFunction(Function* function) {
            if (!function)
                return MS_ERROR_NULL;

            // -- init function
            // -- head added when block is left
            // deprecated: function->addressOffset = instructionSet.currentIndex() + 1;

            FuncClause* clause = new FuncClause(function);
            state s = MS_SUCCESS;

            if ((s = enterClause(clause)) != MS_SUCCESS) {
                delete clause;
            }

            return s;
        }

        /** Note: Var-Arg instruction are used to give a better error handling
         ** on runtime. Using strings instead of indices allow a possibilty to
         ** reflect on compilation state data.
         */ 

        state EntityNameResolver::pushEntityAddress(Context& ctx, Code& code, int begin, int end, const Namespace* base) {
            if (!base)
                return ctx.throwError(MS_ERROR_NULL, "(pushEntityAddress) needs an active namspace");
                
            CodePoint cp { code.getPointOfIndex(begin) };
            int i = begin;

            debug::printsf("$6[%%]", tokenContent(code.getTokens(), begin, end));

            // last read content
            std::string content;
            int depth {0}, scopeBegin {-1}, scopeEnd {-1};
            bool indexMode {false};

            if (end - begin == 0)
                return ctx.throwd(MS_ERROR_INVALID_SYNTAX, cp, "missing target for assignment");

            for (; i < end; i++) {
                const Token& token = code.getToken(i);

                if (token == ".") {
                    if (i == begin)
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, cp, "an entity path may not begin with '.'");
                    
                    if (i == 0)
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, cp, "invalid use of '.' operator: left side is missing");

                    if (i == (end - 1))
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, cp, "invalid use of '.' operator: right side is missing");

                    depth++;
                } else if (token == "[") {
                    // Note: a dot in front of a bracket is handled below as 'invalid identifier'

                    if (!depth)
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, cp, "an entity path may not begin with '['");

                    // [ Integral Index | String Identifier | Expression ]
                    // [] stands for an empty, new LIST object - NOT an array
                    scopeBegin = i + 1;
                    scopeEnd = lang::findScopeEnd(code.getTokens(), i, "[", "]"); //findExpressionEnd(code.getTokens(), i + 1);

                    if (scopeEnd == -1)
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, "unused scope operator ']' has no partner");

                    // An opening bracket needs its closing partner
                    //if (code.getToken(scopeEnd) != "]")
                        //return ctx.throwd(MS_ERROR_INVALID_SYNTAX, "closing bracket ']' not found");

                    indexMode = true;
                } else if (depth == 0) {
                    // If the current token is the first part of
                    // the entity path, it gets handled as a normal entity.
                    // This is directly accessible through its address
                    // which can be determined at compilation.
                    Entity* ent = ctx.findEntity(token.content, base);

                    if (!ent)
                        return ctx.throwd(MS_ERROR_NO_ENTITY_FOR_NAME, cp, "unknown entity for name '%%'", token.content);

                    // Local variable, address relative to StackFrame.
                    if (base->type != EntityType::MODULE && base->isLocal(token.content)) {
                        ctx.getInstructions().append(Op::plref, ent->localAddress);
                    } else {
                        ctx.getInstructions().append(Op::pgv, ent->address);
                    }

                    depth++;
                } else if (depth > 0) {
                    if (!indexMode) {
                        // Identifier check is required at this location since all upcoming entities of the path are
                        // built on runtime and may or may not occur. So only the fact that their name is valid
                        // matters on compilation, existence check is handled by the VM.
                        if (!isValidIdentifier(token.content))
                            return ctx.throwd(MS_ERROR_INVALID_ENTITY_NAME, cp, "invalid identifier '%%'", token.content);

                        Instruction ins {Op::pattr};
                        ins.setVARG(token.content);
                        
                        ctx.getInstructions().append(ins);
                    } else {
                        Expression expr;
                        state s = parseExpression(ctx, expr, code, scopeBegin, scopeEnd);

                        if (s != MS_SUCCESS)
                            return MS_ERROR;

                        // Index is parsed by the expression and used on runtime
                        ctx.getInstructions().append(Op::pindx);

                        i = scopeEnd + 1;
                        cp = code.getPointOfIndex(i);
                    }
                }

                if (token.row > cp.line) {
                    cp.nextLine();
                    cp.column = 0;
                } else
                    cp.next();
            }

            return MS_SUCCESS;
        }

        // -- Functions

        inline void advance(int& i, int off) {
            i += off;
        }

        // Helpers

        bool isGlobal(Namespace* n) {
            if (!n)
                return false;

            if (n->type == EntityType::MODULE)
                return true;

            return false;
        }

        /* Returns the corresponding DataType of the given Entity.
         * Available results are (entity type):
         *  .. # VARIABLE => Variable::dataType
         *  .. # FUNCTION => Function::resultType
         */
        DataType resolveType(Entity* ent) {
            if (!ent)
                return DataType::INVALID;

            switch (ent->type) {
                case EntityType::VARIABLE:
                    return static_cast<Variable*>(ent)->dataType;
                case EntityType::FUNCTION:
                    return static_cast<Function*>(ent)->resultType;

                default:
                    return DataType::INVALID;
            }
        }

        DataType resolveType(DataType given, Entity* context = nullptr) {
            if (given == DataType::REF) {
                if (!context)
                    return DataType::INVALID;

                return resolveType(context);
            }

            return given;
        }

        std::string typeName(DataType type, Entity* ctx = nullptr) {
            std::stringstream buf;

            if (!ctx)
                return traits::stringify::toString(type);

            switch (ctx->type) {
                case EntityType::VARIABLE:
                    buf << "*";
                    buf << traits::stringify::toString(static_cast<Variable*>(ctx)->dataType);
                    
                    break;

                case EntityType::FUNCTION:
                    buf << ctx->name;
                    buf << "(): ";
                    buf << traits::stringify::toString(static_cast<Function*>(ctx)->resultType);

                    break;

                default:
                    buf << "(unknown entity type)";

                    break;
            }

            return buf.str();
        }

        void setExpressionSettings(Expression& expr, ExpressionType type, Operation operation) {
            expr.type = type;
            expr.operation = operation;
        }

        // Parsers

        /* Parses an entity for given input range. An entity may be represented
         * as a literal (1..3 tokens) or as a reference using its identifier.
         */
        state parseEntity(Context& ctx, Expression& expr, Code& code, int from, int to) {
            InstructionSet& is = ctx.getInstructions();
            int range = to - from;
            state s = MS_SUCCESS;

            debug::printsf("[%%..%% -> %%] $4%%$r", from, to, range, tokenContent(code.getTokens(), from, to));

            if (range <= 0)
                return ctx.throwd(MS_ERROR_INVALID_SYNTAX, "failed to parse empty input");
            else if (range == 1) {
                if (isNumber(code.getContent(from))) {
                    index i = ctx.appendLiteral(DataType::INTEGRAL, code.getContent(from));

                    is.append(Op::plit, i);

                    return MS_SUCCESS;
                } else if (isNumber(code.getContent(from), true)) {
                    index i = ctx.appendLiteral(DataType::DECIMAL, code.getContent(from));

                    is.append(Op::plit, i);

                    return MS_SUCCESS;
                } else if (expr.task == MS_EXPR_TASK_PUSH_ATTRIBUTE) {
                    if (!isValidIdentifier(code.getContent(from)))
                        return ctx.throwd(MS_ERROR_INVALID_ENTITY_NAME, code.getPointOfIndex(from), "invalid entity identifier '%%'", code.getContent(from));
                    
                    Instruction ins {Op::pattr};
                    ins.setVARG(code.getContent(from));

                    is.append(ins);

                    return MS_SUCCESS;
                } else {
                    const std::string name = code.getContent(from);
                    Entity* ent = ctx.findEntity(name);

                    if (!ent)
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, CodePoint {ctx.codePoint.line, code.getToken(from).column}, "entity %% does not exist", name); 

                    // TODO: move below???
                    expr.info.reference = ent;
                    
                    if (!ctx.currentNamespace)
                        return ctx.throwError(MS_ERROR_NULL, "(internal) current namespace is null");

                    // Local variable, address relative to StackFrame.
                    if (ctx.currentNamespace->type != EntityType::MODULE && ctx.currentNamespace->isLocal(name)) {
                        ctx.getInstructions().append(Op::plref, ent->localAddress);
                    } else {
                        ctx.getInstructions().append(Op::pgv, ent->address);
                    }

                    return MS_SUCCESS;
                }

                return ctx.throwd(MS_ERROR_INVALID_SYNTAX, CodePoint {ctx.codePoint.line, code.getToken(from).column}, "unknown identifier %%", code.getContent(from));
            } else if (range == 2) {

            } else if (range == 3) {
                if (code.getContent(from) == "'") {
                    if (code.getContent(to - 1) != "'")
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, "string literal is not closed properly");

                    index i = ctx.appendLiteral(DataType::STRING, code.getContent(from + 1));

                    is.append(Op::plit, i);

                    return MS_SUCCESS;
                }
            }
            
            return MS_NO_OP;
        }

        state parseLiteral(Context& ctx, Code& code, int index, int& literalIndex) {
            const std::string& content = code.getContent(index);

            if (content == "'") {
                if (code.getContent(index + 2) != "'")
                    return ctx.throwd(MS_ERROR_INVALID_SYNTAX, index, "malformed string literal (missing ')");

                literalIndex = ctx.appendLiteral(DataType::STRING, code.getContent(index + 1));
            } else if (isNumber(content, true)) {
                literalIndex = ctx.appendLiteral(DataType::DECIMAL, content);
            } else if (isNumber(content)) {
                literalIndex = ctx.appendLiteral(DataType::INTEGRAL, content);
            } else
                return ctx.throwd(MS_ERROR_INVALID_SYNTAX, index, "failed to identify literal");

            return MS_SUCCESS;
        }

        state registerParam(Context& ctx, const std::string& name) {
            state s { MS_SUCCESS };

            if (name.length() == 0)
                return ctx.throwd1(MS_ERROR_NULL, "missing variable name");

            if (!lang::isValidIdentifier(name))
                return ctx.throwd1(MS_ERROR_INVALID_ENTITY_NAME, "invalid variable name '%%'", name);

            // TODO: change to registerVariable(...)
            if (!(ctx.registerEntity(name, EntityType::VARIABLE)))
                return ctx.throwError(s, "failed to register entity '%%' in active namespace", name);

            debug::printsf("Registered variable '%%' in namespace '%%'", name, getEntityName(ctx.currentNamespace, "N/A"));

            return s;
        }

        /* == Object Parser ==
         *
         *  I) Literals
         *   a) Syntax
         *     i) o = {}
         *    ii) p = {key: value, key2: value2}
         *   iii) q = {value0, value1}
         */
        state parseObject(Context& ctx, Expression& expr, Code& code, int begin, int end) {

        }

        inline state parseParenthesis(Context& ctx, const std::string& content, const std::string_view open, const std::string_view close, int type, int* ps, int* pe, int* pdepth, int i) {
            /* parsing left to right
            
            if (content == open) {
                if (ps[type] == -1)
                    ps[type] = i;
                    
                pdepth[type]++;
            } else if (content == close) {
                pdepth[type]--;

                if (!pdepth[type] && pe[type] == -1)
                    pe[type] = i;

                if (pdepth[type] < 0)
                    return ctx.throwd(MS_ERROR_INVALID_SYNTAX, i, "scope operator '%%' has no partner", close);
            }
            */

            // parsing right to left <- currently used
            if (content == open) {
                pdepth[type]--;

                if (!pdepth[type] && ps[type] == -1)
                    ps[type] = i;

                if (pdepth[type] < 0)
                    return ctx.throwd(MS_ERROR_INVALID_SYNTAX, i, "scope operator '%%' has no partner", open);
            } else if (content == close) {
                if (pe[type] == -1)
                    pe[type] = i;

                pdepth[type]++;
            }

            //debug::printsf(" '%%' %%%% - level %%", content, open, close, pdepth[type]);

            return MS_SUCCESS;
        }

        #define MS_EXPR_EXEC_SUB(op)                                                                \
            leftExpr = expr.setupLeft();                                                            \
            rightExpr = expr.setupRight();                                                          \
                                                                                                    \
            if ((left = parseExpression(ctx, *leftExpr, code, begin, i)) != MS_SUCCESS)             \
                return ctx.throwd(left, begin, i, "failed to parse expression for '%%'", op);       \
                                                                                                    \
            if ((right = parseExpression(ctx, *rightExpr, code, i + 1, end)) != MS_SUCCESS)         \
                return ctx.throwd(right, i + 1, end - 1, "failed to parse expression for '%%'", op);\
            

        /* == The Glorious Expression Parser ==
         *
         *  §1 Expressions:
         *  §2 Syntax:
         *  §3 Literals:
         *  §4 References:
         *  §5 Recursive Resolution:
         *  §6 Expression Context:
         * 
         */
        state parseExpression(Context& ctx, Expression& expr, Code& code, int begin, int end, int minPrecedence) {
            InstructionSet& is = ctx.getInstructions();
            int range = end - begin;
            state s = MS_SUCCESS;

            debug::printsf("[%%..%% -> %%] $4%%$r", begin, end, range, tokenContent(code.getTokens(), begin, end));

            expr.tokenBegin = begin;
            expr.tokenEnd = end;

            // === Literals, References, Terminators ===

            if (range <= 0) {
                if (!expr.flags.isset(ExpressionFlag::EMPTY_EXPRESSIONS_ALLOWED))
                    return ctx.throwd(MS_ERROR_INVALID_SYNTAX, code.getPointOfIndex(begin - 1 /* TODO: keep? */), "got empty expression");
                
                expr.subCount = 0;

                return s;
            } else if (range == 1) {
                // references are parsed in the 'range == 2'-section
                // dont allow, will disable 'i' too
                //if (expr.onlyReferences)
                    //return ctx.throwd(MS_ERROR_INVALID_EXPRESSION_STATE, code.getPointOfIndex(begin), "literals are not allowed here");
                
                if (code.getContent(begin) == "not") {
                    if (!expr.isOnlyReferences())
                        return ctx.throwd(MS_ERROR_INVALID_EXPRESSION_STATE, code.getPointOfIndex(begin), "'not' is not allowed here");

                    expr.negate();

                    return MS_SUCCESS;
                }

                if (isNumber(code.getContent(begin))) {
                    // in case numbers are not allowed
                    if (expr.isOnlyReferences())
                        return ctx.throwd(MS_ERROR_INVALID_EXPRESSION_STATE, code.getPointOfIndex(begin), "literals are not allowed here");

                    index i = ctx.appendLiteral(DataType::INTEGRAL, code.getContent(begin));

                    is.append(Op::plit, i);

                    expr.type = ExpressionType::LITERAL;
                    expr.info.terminatorValue = code.getContent(begin);
                    expr.terminator = true;
                    expr.result.type = DataType::INTEGRAL;

                    debug::printsf<3>("$b`3{%%} -> %%", code.getContent(begin), traits::stringify::toString(expr.result.type));

                    return MS_SUCCESS;
                } else if (isNumber(code.getContent(begin), true)) {
                    // in case numbers are not allowed
                    if (expr.isOnlyReferences())
                        return ctx.throwd(MS_ERROR_INVALID_EXPRESSION_STATE, code.getPointOfIndex(begin), "literals are not allowed here");
                    
                    index i = ctx.appendLiteral(DataType::DECIMAL, code.getContent(begin));

                    is.append(Op::plit, i);

                    expr.type = ExpressionType::LITERAL;
                    expr.info.terminatorValue = code.getContent(begin);
                    expr.terminator = true;
                    expr.result.type = DataType::DECIMAL;

                    debug::printsf<3>("$b`3{%%} -> %%", code.getContent(begin), traits::stringify::toString(expr.result.type));

                    return MS_SUCCESS;
                } else if (expr.task == MS_EXPR_TASK_PUSH_ATTRIBUTE) {
                    if (!isValidIdentifier(code.getContent(begin)))
                        return ctx.throwd(MS_ERROR_INVALID_ENTITY_NAME, "invalid entity identifier '%%'", code.getContent(begin));
                    
                    Instruction ins {Op::pattr};
                    ins.setVARG(code.getContent(begin));

                    is.append(ins);

                    return MS_SUCCESS;
                } else {
                    const std::string name = code.getContent(begin);

                    // -- Special Keywords ---

                    // Handle 'self' usage in member functions.
                    if (name == MS_THIS_KEYWORD) {
                        if (!ctx.currentFunction) // TODO: + check if owner is a proto!
                            return ctx.throwd(MS_ERROR, begin, "'%%' can only be applied in member functions", MS_THIS_KEYWORD);

                        expr.result.type = DataType::REF;
                        expr.terminator = true;
                        //expr.result.type.refType = ctx.currentFunction-> /PROTO_ID/

                        // TODO!!!
                        Instruction ins(Op::push);
                        ins.append(0); // current object
                        ins.setVARG(std::string("TODO: ") + std::string(MS_THIS_KEYWORD));

                        ctx.getInstructions().append(ins);

                        return MS_SUCCESS;
                    }

                    // -- Generic Entities --

                    Entity* ent = ctx.findEntity(name, expr.context);

                    if (!ent)
                        return ctx.throwd(MS_ERROR_NO_ENTITY_FOR_NAME, CodePoint {ctx.codePoint.line, code.getToken(begin).column}, "entity '%%' does not exist", name); 

                    // init meta; may be updated by functions
                    expr.info.reference = ent;
                    expr.result.type = DataType::REF;

                    if (!ctx.currentNamespace)
                        return ctx.throwError(MS_ERROR_NULL, "(internal) current namespace is null");

                    if (ent->type == EntityType::FUNCTION) {
                        Function* func = static_cast<Function*>(ent);
                        int params = expr.xenum.expressions.size(); //expr.info.listedCount;
                        int param = 0;

                        // -- Validation

                        if (func->mods.isset(FMod::LOCAL) && ctx.currentNamespace != func->owner)
                                return ctx.throwd(MS_ERROR, begin, "local function '%%' can only be called withing its owning namespace", func->name);

                        if (expr.isVoidResultRequired() && (func->containsReturn || func->resultType != DataType::VOID) && !ms::allow_non_assigning_non_void_func)
                            return ctx.throwd(MS_ERROR, begin, "called function is required to have no return result (could lead to ghost values on stack)");

                        //if (expr.isVoidResultRequired() && (func->containsReturn || func->resultType == DataType::VOID))
                            //return ctx.throwd(MS_ERROR, begin, "called function is required to have no return result");

                        if (!func->mods.isset(FMod::EXTERN) && func->isVoid() && !expr.isVoidResultAllowed())
                            return ctx.throwd(MS_ERROR, begin, "called function returns nothing but is used in a context which requires a return value");

                        if (params < func->fixedParamCount)
                            return ctx.throwd(MS_ERROR, begin, "insufficient parameter count (given: %%, required: %%)", params, func->fixedParamCount); // ERROR_INVALID_PARAM_COUNT

                        if (!func->vararg && params > func->paramCount)
                            return ctx.throwd(MS_ERROR, begin, "too many arguments for a non-varg function (given: %%, expected: %%)", params, func->paramCount); // ERROR_INVALID_PARAM_COUNT

                        // -- Type checking & Parameters

                        OnErrorDeleted<FCall> fcall (new FCall);

                        fcall->owner = func;
                        fcall->debugRef = ctx.codePoint;
                        fcall->code = ctx.activeCode;

                        // check for guards
                        debug::printsf(" $2$iinvoke %% with pcount=%%", func->getSignature(), params);

                        /* The right expression can be:
                         *  - nothing = no param
                         *  - one expression
                         *  - an expression enum (multiple expressions listed by comma)
                         * 
                         * Go through all expressions (stored in rightExpr->xenum) and test
                         * against function parameter types to guarantee type safety.
                         */
                        for (Expression* e : expr.xenum.expressions) {
                            if (!func->vararg && param >= func->paramCount)
                                return ctx.throwd(MS_ERROR, begin, end, "too many params (%%/%%)", param, func->paramCount);
                            
                            // Get expression type; resolve references through the info.reference
                            DataType etype = resolveType(e->result.type, e->info.reference);

                            // Accept states whether a guard is satisfied or not. If the guard
                            // requires undefined, any param type is accepted.
                            bool accept = func->params[param].guard == DataType::UNDEFINED || etype == func->params[param].guard;

                            debug::printsf("  $2$i%% (%%) := %% (%%) -> %%",
                                func->params[param].name,
                                traits::stringify::toString(func->params[param].guard),
                                code.getContent(e->tokenBegin, e->tokenEnd),
                                traits::stringify::toString(etype),
                                accept);

                            if (!accept)
                                return ctx.throwd(MS_ERROR, e->tokenBegin, e->tokenEnd - 1, "invalid parameter type; expected %% but got %%",
                                    traits::stringify::toString(func->params[param].guard), traits::stringify::toString(etype));

                            // On successfull calls, append this information to the FCall
                            fcall->params.push_back(FCall::CallParam { e->result.type, func->params[param].defaultLiteral, e->tokenBegin, e->tokenEnd });

                            param++;
                        }

                        // If the given expression list is not sufficient for the
                        // needed amount of parameters, append all defaults now.
                        for (; param < func->paramCount; param++) {
                            if (func->params[param].defaultLiteral != -1) {
                                debug::printsf("  $2$i%% Adding literal %% as default", func->params[param].name, func->params[param].defaultLiteral);

                                ctx.instructionSet.append(Op::plit, func->params[params].defaultLiteral);
                            }

                            // else: should never happen since the parseFunction() already checks
                            //       that after a default param all others have a default too
                        }

                        // -- Expression Meta

                        expr.type = ExpressionType::FUNC_CALL;
                        expr.result.type = func->resultType;
                        //expr.info.reference = leftExpr->info.reference;

                        // -- Appending instructions

                        // V-Args ...
                        int vargCount = params - func->paramCount; // amount of given VARG parameters

                        // If we have more than one VARG, collect all of them
                        // and push them as one container. Else, no or only
                        // the one value will be passed. This behavior should
                        // be considered in external functions when casting
                        // the parameters to Container types!
                        if (vargCount > 1)
                            ctx.instructionSet.append(Op::collect, vargCount);

                        // Extern functions ...
                        if (func->mods.isset(FMod::EXTERN)) {
                            if (!func->extLinking)
                                return ctx.throwd(MS_ERROR_NO_EXT_LINKING, "extern function '%%' has no linking (no c++ partner)", func->name);

                            // Should be covered already ...
                            //if (!expr.isVoidResultAllowed())
                                //ctx.throwd(MS_WARNING, begin, "the extern function is used in a context which requires a return value");

                            return ctx.instructionSet.append(Op::extcall, func->extLinking->getId(), func->paramCount);
                        }

                        // append FCall information
                        ctx.functionCalls.push_back(fcall.release());

                        // append instruction

                        Instruction call(Op::call);

                        call.append(func->label, true);
                        call.append(func->requiredMemoryCells());
                        call.append(params);

                        ctx.instructionSet.append(call);

                        // If a non-void function is called without using
                        // its returned value, pop it and clear the stack
                        // from any possible 'ghost value'.
                        if (expr.isVoidResultRequired() && (func->containsReturn || func->resultType != DataType::VOID) && ms::allow_non_assigning_non_void_func)
                            ctx.instructionSet.append(Op::pop);

                        return MS_SUCCESS;
                    }

                    else if (ent->type == EntityType::VARIABLE) {
                        expr.result.type = static_cast<Variable*>(ent)->dataType;
                        expr.terminator = true;
                        // TODO: change to expr.result.type.refType ??
                    }

                    debug::printsf<3>("$b`3{%%} -> %%", code.getContent(begin), expr.resultName());

                    // Local variable, address relative to StackFrame.
                    if (ctx.currentNamespace->type != EntityType::MODULE && ctx.currentNamespace->isLocal(name)) {
                        ctx.getInstructions().append(Op::plref, ent->localAddress);
                    } else {
                        ctx.getInstructions().append(Op::pgv, ent->address);
                    }

                    return MS_SUCCESS;
                }

                return ctx.throwd(MS_ERROR_INVALID_SYNTAX, CodePoint {ctx.codePoint.line, code.getToken(begin).column}, "unknown identifier %%", code.getContent(begin));
            } else if (range == 2) {
                if (code.getContent(begin) == "not") {
                    expr.negate();

                    return parseExpression(ctx, expr, code, begin + 1, end); // should be a terminator (literal or reference)
                }

                else if (code.getContent(begin) == "{") {
                    if (code.getContent(end) != "}")
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, begin, end, "malformed object literal");

                    // expr.result.type.id = ctx.types.ofId(TypeId::OBJECT_LITERAL);

                    return MS_SUCCESS;
                }

                else if (code.getContent(begin) == "$") {
                    Expression special;

                    MS_ASSERT_EXPR(ctx, special, code, begin + 1, end, s, "$")

                    is.append(Op::cmd, 0x1A);

                    debug::printsf(" $4%% (cmd 0x1A)", '$');

                    return MS_SUCCESS;
                }

                // Note: "end - 1" since param is INCLUSIVE
                return ctx.throwd(MS_ERROR_INVALID_SYNTAX, begin, end - 1, "invalid expression syntax");
            } else if (range == 3) {
                // String literals            
                if (code.getContent(begin) == "'") {
                    // in case no string LITERAL is allowed
                    if (expr.isOnlyReferences())
                        return ctx.throwd(MS_ERROR_INVALID_EXPRESSION_STATE, code.getPointOfIndex(begin), "literals are not allowed here");

                    if (code.getContent(end - 1) != "'")
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, "string literal is not closed properly");

                    index i = ctx.appendLiteral(DataType::STRING, code.getContent(begin + 1));

                    is.append(Op::plit, i);

                    expr.type = ExpressionType::LITERAL;
                    expr.info.terminatorValue = code.getContent(begin);
                    expr.result.type = DataType::STRING;

                    debug::printsf<3>("$b`3{%%} -> %%", code.getContent(begin, end), traits::stringify::toString(expr.result.type));

                    return MS_SUCCESS;
                }
            }

            // === Recursive Expression Parser ===

            CodePoint cp {code.getPointOfIndex(begin)};

            int i = end - 1;
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
            state left {MS_SUCCESS}, right {MS_SUCCESS};

            // Results of sub-algorithm calls
            Expression* leftExpr {nullptr};
            Expression* rightExpr {nullptr};

            for (; i >= begin; i--) {
                const Token& content = code.getToken(i);
                cp = CodePoint {content.row, content.column};

                // -- meta

                expr.tokenOperator = i; // the operator string is at pos 'i'
                expr.precedence = p;

                // -- string and ignored content

                if (content == "'") {
                    ignore = !ignore;
                }

                if (ignore)
                    continue;

                // -- parenthesis control

                /* NOTE: brackets cannot be parsed with a simple precedence.
                 * When there are no arithmetic operators on the level of the bracket, the bracket
                 * gets parsed first. But if otherwise, the arithmetic operator has the
                 * higher precedence and must be the main anchor.
                 * e.g.:
                 *  a) a[0 + 1] -> first parse [], then 0 + 1
                 *  b) a[0] + 1 -> first parse +, then []
                 * Therefore, use the exclusing mechanic below instead of
                 * simply checking for the precendence of brackets.
                 */

                //if (p >= 7) {
                    MS_ASSERT_STATE(s, parseParenthesis(ctx, content.content, "(", ")", 0, ps, pe, pdepth, i))
                    MS_ASSERT_STATE(s, parseParenthesis(ctx, content.content, "[", "]", 1, ps, pe, pdepth, i))
                    MS_ASSERT_STATE(s, parseParenthesis(ctx, content.content, "{", "}", 2, ps, pe, pdepth, i));
                //}
                //MS_ASSERT_STATE(s, parseParenthesis(ctx, content.content, "{", "}", 2, ps, pe, pdepth, i))
                //MS_ASSERT_STATE(s, parseParenthesis(ctx, content.content, "<", ">", 3, ps, pe, pdepth, i))

                // Whenever a parenthesis is present in the current level,
                // the scoped content gets parsed later. All precedences
                // of normal operators are parsed on this level before.
                if ((pdepth[0] | pdepth[1] | pdepth[2] | pdepth[3]) > 0)
                    continue;

                // == operators ==

                /* Note:
                 *  use the MS_ASSERT_EXPR makro to easily parse one side
                 *  of an operation and immediately return on fail without
                 *  parsing the other side nor doing other tasks before
                 *  termination.
                 */

                // -- special

                if (p == 5 && content == "#") {
                    // Check for range [i + 1 < end - 1] for the code review.
                    // If the range is malformed, the error output would be empty.
                    if (i + 2 < end)
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, i + 1, end - 1, "'#' takes extactly one token as the right expression");
                    else if (i + 1 == end)
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, i, "'#' requires one token as the right expression");

                    MS_ASSERT_EXPR(ctx, expr, code, begin, i, left, "#")

                    Instruction x(Op::cmd);

                    x.append(MS_CMD_EXTRACT_META);
                    x.setVARG(code.getContent(end - 1));

                    is.append(x);

                    debug::printsf(" $4# (meta extractor)");

                    break;
                }

                // -- logical

                if (p == 5 && content == "not") {
                    expr.negate();

                    // don't break;
                }

                if (p == 0 && content == "is") {
                    MS_ASSERT_EXPR(ctx, expr, code, begin, i, left, "is")
                    MS_ASSERT_EXPR(ctx, expr, code, i + 1, end, right, "is")

                    is.append(Op::cmp);

                    if (expr.isNegated() && expr.appendNegationInstruction)
                        is.append(Op::negate);
                    
                    //is.appendJmp(Op::jnz);

                    expr.operation = Operation::EQUALS;

                    debug::printsf(" $4is (cmp & jnz)");

                    break;
                }

                if (p == 5 && content == "<>") {
                    MS_EXPR_DO("<>")

                    //left = parseExpression(ctx, expr, code, begin, i);
                    //right = parseExpression(ctx, expr, code, i + 1, end);

                    //MS_EXPR_CHECK_STATES()

                    is.append(Op::cmp);
                    //is.appendJmp(Op::jz);

                    expr.operation = Operation::NOT_EQUALS;

                    debug::printsf(" $4<> <=> not is (cmp & jz)");

                    break;
                }

                if (p == 5 && content == "<") {
                    MS_EXPR_DO("<")

                    //left = parseExpression(ctx, expr, code, begin, i);
                    //right = parseExpression(ctx, expr, code, i + 1, end);

                    //MS_EXPR_CHECK_STATES()

                    is.append(Op::cmp);
                    //is.appendJmp(Op::jz);

                    expr.operation = Operation::LESS_THAN;

                    debug::printsf(" $4<");

                    break;
                }

                // -- arithmetic operators

                if (p == 6 && content == "*") {
                    //expr.tokenOperator = i;

                    MS_EXPR_SETUP(expr, 0, 0)
                    MS_ASSERT_EXPR(ctx, *leftExpr, code, begin, i, left, "*")
                    MS_ASSERT_EXPR(ctx, *rightExpr, code, i + 1, end, right, "*")

                    //left = parseExpression(ctx, expr, code, begin, i);
                    //right = parseExpression(ctx, expr, code, i + 1, end);

                    MS_EXPR_CHECK_STATES()

                    is.append(Op::mul);

                    expr.operation = Operation::MULTIPLY;
                    expr.type = ExpressionType::ARITHMETIC_MUL;
                    expr.result.type = lang::getResultingType(leftExpr->result.type, rightExpr->result.type);

                    debug::printsf(" $4MUL");
                    debug::printsf<3>("$b`3{%% | %% %% %%} -> %%", tokenContent(code.getTokens(), begin, end),
                        leftExpr->resultName(),
                        content.content,
                        rightExpr->resultName(),
                        traits::stringify::toString(expr.result.type));

                    break;
                }

                if (p == 5 && content == "+") {

                    setExpressionSettings(expr, ExpressionType::ARITHMETIC_ADD, Operation::ADD);
                    MS_EXPR_EXEC_SUB("+")
                    
                    expr.result.type = lang::getResultingType(leftExpr->result.type, rightExpr->result.type);

                    //if (leftExpr->isTerminator() && rightExpr->isTerminator())

                    if (expr.target && expr.target->owner == expr.context) {
                        if (expr.context->isModuleLevel())
                            is.append(Op::add, expr.target->address);
                        else
                            is.append(Op::add, expr.target->localAddress);

                        // Tell the parser to skip the 'alv' or 'agv' part.
                        expr.assignmentIncluded = true;
                    } else
                        is.append(Op::add);
                    //else
                        //is.append(Op::add_t, (int) leftExpr->result.type.dataType);

                    debug::printsf(" $4ADD");
                    debug::printsf<3>("$b`3{%% | %% %% %%} -> %%",
                        tokenContent(code.getTokens(), begin, end),
                        leftExpr->resultName(),
                        content.content,
                        rightExpr->resultName(),
                        traits::stringify::toString(expr.result.type));

                    // TODO: handle invalid
                    if (expr.result.type == DataType::INVALID) {
                        return ctx.throwd(MS_ERROR_INVALID_OPERAND_TYPES, begin, end - 1, "invalid type combination in operation {%%} %% {%%} -> invalid",
                            leftExpr->resultName(), content.content, rightExpr->resultName());
                    }
                    
                    break;
                }

                if (p == 5 && content == "-") {
                    left = parseExpression(ctx, expr, code, begin, i);
                    right = parseExpression(ctx, expr, code, i + 1, end);

                    MS_EXPR_CHECK_STATES()

                    is.append(Op::sub);

                    debug::printsf(" $4SUB");

                    break;
                }

                // -- splitting & context operators

                if (p == 6 && content == "::") {
                    const std::string module = code.getContent(i - 1);

                    if (!isValidIdentifier(module))
                        return ctx.throwd(MS_ERROR, (i > 0 ? i - 1 : i), "invalid module name '%%'", module);

                    if (ctx.modules.find(module) == ctx.modules.end())
                        return ctx.throwd(MS_ERROR, (i > 0 ? i - 1 : i), "no module found for '%%'. Missing import?", module);

                    rightExpr = expr.setupRight(0);
                    rightExpr->context = ctx.modules[module];

                    if ((right = parseExpression(ctx, *rightExpr, code, i + 1, end)) != MS_SUCCESS)
                        return ctx.throwd(right, i + 1, end, "failed to parse target for scope operator ::");

                    debug::printsf(" $4:: (%%)", module);

                    break;
                }

                if (p == 6 && content == ".") {
                    // The first occurance of the dot-operator will require
                    // a reference as the first part. All other parts of the
                    // path are runtime attributes and thus must be pushed
                    // as varible keys.

                    leftExpr = expr.setupLeft();
                    leftExpr->flags.enable(ExpressionFlag::ONLY_REFERENCES);

                    // inherit task if already pushing attributes
                    if (expr.task == MS_EXPR_TASK_PUSH_ATTRIBUTE)
                        leftExpr->task = expr.task;

                    if ((left = parseExpression(ctx, *leftExpr, code, begin, i)) != MS_SUCCESS)
                        return ctx.throwd(left, begin, i, "invalid syntax of the left side of dot-operator");
                    
                    rightExpr = expr.setupRight(MS_EXPR_TASK_PUSH_ATTRIBUTE); // 'pattr' instead of references
                    right = parseExpression(ctx, *rightExpr, code, i + 1, end);

                    // disabled debug statement for a cleaner output
                    // debug::printsf("  $4.");

                    MS_EXPR_CHECK_STATES()

                    break;
                }

                if (p == 0 && content == ",") {

                    expr.type = ExpressionType::COMMA_ENUM;
                    
                    // parse terminator (cannot be empty)
                    leftExpr = expr.setupLeft();
                    leftExpr->flags.disable(ExpressionFlag::EMPTY_EXPRESSIONS_ALLOWED);
                    left = parseExpression(ctx, *leftExpr, code, begin, i);

                    // As long the left side contains a sub enumeration
                    // append all its children to this xenum.
                    if (leftExpr->type == ExpressionType::COMMA_ENUM) {

                    } else {
                        // Else: leftExpr is a single expression and will be parsed now
                        
                    }

                    expr.xenum.enabled = true;
                    expr.xenum.expressions.push_back(leftExpr);

                    // parse rest (cannot be empty)
                    rightExpr = expr.setupRight();
                    rightExpr->flags.disable(ExpressionFlag::EMPTY_EXPRESSIONS_ALLOWED);
                    right = parseExpression(ctx, *rightExpr, code, i + 1, end);

                    // append right side
                    if (rightExpr->type == ExpressionType::COMMA_ENUM) {
                        for (Expression* e : rightExpr->xenum.expressions) {
                            expr.xenum.expressions.push_back(e);
                        }
                    } else {
                        expr.xenum.expressions.push_back(rightExpr);
                    }
                    // save total amount of total expressions listed by the ',' operator
                    expr.info.listedCount = leftExpr->subCount + rightExpr->subCount;

                    // if we are the parent expression of the listing
                    // put the total sub expression count to the listing count
                    if (expr.task == MS_EXPR_TASK_PARSE_PARAMS)
                        expr.subCount = expr.info.listedCount;

                    MS_EXPR_CHECK_STATES()

                    debug::printsf(" $4,");

                    break;
                }

                // -- parenthesis execution

                // parenthesis ()
                if (p == 7 && ps[0] != -1 && pe[0] != -1) {
                    // Instantiate rightExpr for later usage
                    rightExpr = expr.setupRight(MS_EXPR_TASK_PARSE_PARAMS); // allows empty expressions

                    // Only parse non-empty expressions
                    if (ps[0] != pe[0]) {
                        right = parseExpression(ctx, *rightExpr, code, ps[0] + 1, pe[0]);

                        if (right != MS_SUCCESS)
                            return ctx.throwd(right, i, pe[0], "invalid expression body in ()");
                    } else
                        rightExpr->empty = true;

                    // initial meta data
                    expr.tokenOperator = expr.tokenBegin;
                    expr.type = ExpressionType::PARENTHESIS_0; // TODO: change to PARENTHESIS_0
                    expr.operation = Operation::NONE; // TODO: change too

                    // If on the outer side of the () is no defining operation
                    // (happens when only using 'not (...)'), set the operation
                    // inside of the brackets as the main operation.
                    if (expr.operation == Operation::NONE)
                        expr.operation = rightExpr->operation;

                    // parse second since 'parseExpression' with len=1 and content=funcion_name
                    // will automatically append a 'call' instruction
                    if (ps[0] > begin) {

                        leftExpr = expr.setupLeft();
                        leftExpr->flags.enable(ExpressionFlag::ONLY_REFERENCES); // and 'not' :D

                        if (expr.flags.isset(ExpressionFlag::VOID_RESULT_REQUIRED))
                            leftExpr->flags.enable(ExpressionFlag::VOID_RESULT_REQUIRED);

                        if (expr.flags.isset(ExpressionFlag::VOID_RESULT_ALLOWED))
                            leftExpr->flags.enable(ExpressionFlag::VOID_RESULT_ALLOWED);

                        // Only sync xenum if there are multiple parameters;
                        // else the right expression it-self is the only parameter.
                        if (rightExpr->xenum.enabled)
                            leftExpr->xenum.expressions = rightExpr->xenum.expressions;
                        else if (!rightExpr->empty)
                            leftExpr->xenum.expressions.push_back(rightExpr);

                        // replaced by xenum.expressions.size():
                        // leftExpr->info.listedCount = rightExpr->subCount;

                        left = parseExpression(ctx, *leftExpr, code, begin, ps[0]); // see (, parse prefix

                        if (left != MS_SUCCESS)
                            return ctx.throwd(left, begin, ps[0], "could not parse prefix of expression '()'");

                        if (leftExpr->info.reference && leftExpr->info.reference->type == EntityType::COMP_FUNC) {
                            CompileTimeFunction* ctf = static_cast<CompileTimeFunction*>(leftExpr->info.reference);

                            if ((left = ctf->execute(ctx, expr, *rightExpr)) != MS_SUCCESS)
                                return ctx.throwd(MS_ERROR, begin, "failed to invoke '%%'", begin);
                        }
                    }

                    // take negation values into account and combine them with parent (current) expression
                    expr.combineChildren();

                    // Handle function calls
                    if (leftExpr && leftExpr->type == ExpressionType::FUNC_CALL) {
                        expr.result.type = leftExpr->result.type;
                    } else {
                        expr.result.type = rightExpr->result.type;
                    }

                    debug::printsf(" $4()"); //, tokenContent(code.getTokens(), begin, ps[0]), tokenContent(code.getTokens(), ps[0] + 1, pe[0]));

                    pdepth[0] = 0;
                    ps[0] = pe[0] = -1;

                    break;
                }

                // scope operator []
                if (p == 6 && ps[1] != -1 && pe[1] != -1) {
                    if (ps[1] > begin) {

                        leftExpr = expr.setupLeft();
                        leftExpr->flags.enable(ExpressionFlag::ONLY_REFERENCES);

                        left = parseExpression(ctx, *leftExpr, code, begin, ps[1]); // see (, parse prefix

                        if (left != MS_SUCCESS)
                            return ctx.throwd(left, begin, ps[1], "could not parse prefix of expression '[]'");
                    } else {
                        // []-operator ALWAYS requires a prefix (aka owner)
                        // TODO: may be chained after a function call

                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, ps[1], "missing owner of ...[]-operator");
                    }
                    
                    rightExpr = expr.setupRight();
                    right = parseExpression(ctx, *rightExpr, code, ps[1] + 1, pe[1]);

                    if (right != MS_SUCCESS)
                        return ctx.throwd(right, i, pe[1], "missing or invalid index");

                    ctx.getInstructions().append(Op::pindx);

                    debug::printsf(" $4[%%]", tokenContent(code.getTokens(), ps[1] + 1, pe[1]));

                    pdepth[1] = 0;
                    ps[1] = pe[1] = -1;

                    break;
                }

                // objects {}
                if (p == 1 && ps[2] != -1 && pe[2] != -1) {
                    if ((s = parseObjectLiteral(ctx, code, ps[2] + 1, pe[2])) != MS_SUCCESS)
                        return ctx.throwd(s, "failed to parse object literal");

                    break;
                }

                // When reaching the end of given input range and not all
                // precedences are resolved, start at the beginning on
                // a higher precedence.
                if (i == begin && ++p < mp)
                    i = end;
            }

            if (left != MS_SUCCESS)
                return ctx.throwd(left, i, "failed to parse left expression");

            if (right != MS_SUCCESS)
                return ctx.throwd(right, i, "failed to parse right expression");

            // -- unclosed parenthesis

            if (pdepth[0] > 0)
                return ctx.throwd(MS_ERROR_INVALID_SYNTAX, ps[0], end - 1, "missing closing partner for scope operator ')'");

            if (pdepth[1] > 0)
                return ctx.throwd(MS_ERROR_INVALID_SYNTAX, ps[1], end - 1, "missing closing partner for scope operator ']'");

            if (pdepth[2] > 0)
                return ctx.throwd(MS_ERROR_INVALID_SYNTAX, ps[2], end - 1, "missing closing partner for scope operator '}'");

            if (pdepth[3] > 0)
                return ctx.throwd(MS_ERROR_INVALID_SYNTAX, ps[3], end - 1, "missing closing partner for scope operator '>'");

            return s;
        }

        /*  1 + 2 + 3
         *   /|\
         *  1 + 2 + 3
         *  |    /|\
         *  1   2 + 3
         *      |   |
         *      2   3
         * 
         *  1 + 2 * 3 + 4 * 5
         *   /|\
         *  1 + 2 * 3 + 4 * 5
         *           /|\
         *      2 * 3 + 4 * 5
         *       /|\     /|\
         *      2 * 3   4 * 5
         * 
         *  ===
         * 
         *  1 + 2 + 3
         *  l: 1
         *  r: 2 + 3
         *  o: +
         *  -
         *  1
         *  -
         *  2 + 3
         *  l: 2
         *  r: 3
         *  o: +
         *  -
         *  2
         *  3
         * 
         *  ===
         * 
         *  ( 1 + 2 ) + 3
         *  l: ( 1 + 2 )
         *  r: 3
         *  o: +
         *  -
         *  ( 1 + 2 )
         *  l: -
         *  r: 1 + 2
         *  o: ( )
         *  -
         *  1 + 2
         *  l: 1
         *  r: 2
         *  o: +
         *  -
         *  1
         *  2
         * 
         *  ===
         * 
         *  print (exp, offset = 0)
         *      if exp.tokenCount == 1
         *          out << << ' ' * offset << exp
         *          return
         *      else
         *          out << ' ' * offset << exp.left << ' # ' << exp.right << \n
         * 
         *      if exp.left then
         *          print (expr.left, offset)
         * 
         *      out << ' # '
         * 
         *      if exp.right then
         *          print (expr.right)
         *  end
         * 
         *  print_0(exp)
         *   out << exp
         * 
         *   print(exp.left)
         *   out << ' # '
         *   print(exp.right, exp.left.tokenCount + 1 for operator)
         * 
         *  ===
         * 
         *  (1 + 2 + 3) -> print_0
         *   out << 1 + 2 + 3
         * 
         *   print(1)
         *    out << ' ' * 0 << 1
         *   out << ' # '
         *   print(2 + 3, 2)
         *    out << ' ' * 0 << 2 << ' + ' << 3 << \n
         *    
         *    print(2, 2)
         *     out << ' ' * 2 << 2
         *    out << ' # '
         *    print(3)
         *     out << ' ' * 0 << 3
         * 
         *  print_0(1 + 2 + 3)
         *  output:
         *   1 + 2 + 3
         *   1 # 2 + 3
         *       2 # 3
         *   
         * 
         */
        void printExpressionBase(std::ostream& stream, Code& code, Expression& expr) {
            for (int i = expr.tokenBegin; i < expr.tokenEnd; i++) {
                stream << code.getContent(i);

                if (i < expr.tokenEnd - 1)
                    stream << ' ';
            }
        }

        void printExpression(std::ostream& stream, Code& code, Expression& expr, int base = -1, int offset = 0) {
            if (base == -1)
                base = expr.tokenBegin;

            int tokenCount = expr.tokenEnd - expr.tokenBegin;
            int off = expr.tokenBegin - base;
            std::stringstream buf;

            int tokenOp = expr.tokenOperator;
            int tokenInnerOp = -1;

            //debug::printsf("{%%} -> begin: %%, offset: %%, off: %%", tokenContent(code.getTokens(), expr.tokenBegin, expr.tokenEnd), expr.tokenBegin, offset, off);

            if (expr.type == ExpressionType::PARENTHESIS_0) {
                if (expr.right)
                    tokenInnerOp = expr.right->tokenOperator;
            }

            if (tokenCount == 1) {
                std::cout << code.getContent(expr.tokenBegin);

                return;
            } else {
                if (expr.left)
                    printExpressionBase(stream, code, *expr.left);

                debug::applyColor(stream, debug::ChatColor::FG_RED);
                
                if (expr.type != ExpressionType::PARENTHESIS_0)
                    stream << ' ';

                stream << code.getContent(tokenOp) << ' ';
                debug::resetStream(stream);

                if (expr.right)
                    printExpressionBase(stream, code, *expr.right);

                // closing )
                if (expr.type == ExpressionType::PARENTHESIS_0) {
                    debug::applyColor(stream, debug::ChatColor::FG_RED);
                    stream << ' ' << code.getContent(expr.tokenEnd - 1) << ' ';
                    debug::resetStream(stream);
                }

                stream << '\n';
            }

            if (expr.type == ExpressionType::PARENTHESIS_0)
                stream << std::string((tokenInnerOp - base) * 2 - 1, ' ');
            else
                stream << std::string((tokenOp - base) * 2 - 1, ' ');

            stream << "/ \\\n";

            if (expr.left) {
                if (expr.type == ExpressionType::PARENTHESIS_0)
                    stream << std::string((tokenInnerOp - base) * 2, ' '); // * 2 for normal and symbol spaces
                else
                    stream << std::string((off * 2), ' '); // * 2 for normal and symbol spaces
                
                printExpression(stream, code, *expr.left, base, off);
            }

            stream << "   ";
            
            if (expr.right)
                printExpression(stream, code, *expr.right, base);
        }

        // def [extern|atomic]* {[module::][Proto.]name}([TYPE arg: defaultLITERAL [,]]*):
        // def extern get_os_name(int version: 0)
        // def Proto.member(param)
        // def math::abs(num x) override:

        /*
         * Note: will - if NOT marked as 'extern' - automatically join the
         * functions's clause and continue to parse the function body. NO
         * 'joinFunction' is needed here.
         *
         * Valid modifications:
         *  + prefix:
         *      - extern
         *      - atomic -- unimplemented
         * 
         *  + suffix:
         *      - override -- unimplemented
         * 
         * Parameters:
         *  + [guard] name [...] [: default]
         *  + multiple guards: only last is recognized
         *  + guards are overshadowed by the default value
         */
        state parseFunction(Context& ctx, Code& code, Function& f, int& i) {
            state s = MS_SUCCESS;
            int begin = i;
            int lastSuccessfull = i;

            //std::vector<std::string> mods;
            std::string fname {""};
            int stage {0};
            bool moduleContext {false};
            bool dotOperator {false};
            bool defaultVal {false}; // true when a default value was found; stays true

            // TODO: change!
            // Just a temporary fix...
            if (ctx.currentNamespace && ctx.currentNamespace->type != EntityType::MODULE)
                return ctx.throwd(MS_ERROR_UNSUPPORTED_LANG_CONSTRUCT, "nested functions are currently not supported.");

            if (ctx.modules.find(code.getModuleName()) == ctx.modules.end())
                return ctx.throwd(MS_ERROR_INTERNAL, "parseFunction(ctx, code, f, i): code->getModuleName() -> no module for name found");

            // Entities which can contain member functions:
            //  - modules, prototypes, objects
            Namespace* owner { ctx.modules.at(code.getModuleName()) };

            // Modifications
            XMods<short, FMod> mods;

            // Parameters
            std::string pname;
            int plitIndex {-1};
            DataType pguard {DataType::UNDEFINED};
            bool pvarg {false};

            for (; i < code.getTokens().size(); i++) {
                const std::string& content = code.getToken(i).content;

                // -- Prefix: modifications and target+name
                if (stage == 0) {
                    // 1) check for known modifications
                    if (content == "extern") {
                        f.mods.enable(FMod::EXTERN);
                    } else if (content == "atomic") {
                        return ctx.throwd(MS_ERROR_UNSUPPORTED_LANG_CONSTRUCT, "atomic functions are not implemented yet");
                    } else if (content == "local") {
                        f.mods.enable(FMod::LOCAL);
                    }

                    // 2) on '::' - change namespace context to external module
                    else if (content == "::") {
                        if (moduleContext)
                            return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, code.getPointOfIndex(i), "the function path is already located in a module (duplicate '::' operator)");

                        if (ctx.modules.find(code.getContent(i - 1)) == ctx.modules.end())
                            return ctx.throwd(MS_ERROR_NO_ENTITY_FOR_NAME, code.getPointOfIndex(i - 1), "no module for name '%%' loaded", code.getContent(i - 1));
                    
                        owner = ctx.modules[code.getContent(i - 1)];
                        moduleContext = true;
                        f.owner = owner;
                    }

                    // 3) on '.' - change member context; only applicable once!
                    else if (content == ".") {
                        // only parse the left token to update the owner
                        // the right side will be parsed by a later '.' or
                        // might be the name as the last token



                        // THIS IS NOT SUITABLE FOR A COMPILER
                        // THIS IS CODE FOR AN INTERPRETER -> CHANGE TO 'pattr' ...

                        // OR allow only one scope operator '.' for one prototype
                        // which is known on compilation time


                        if (dotOperator)
                            return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, code.getPointOfIndex(i), "a function can only be assigned to a member depth of 1");

                        if (!owner)
                            return ctx.throwd(MS_ERROR_INTERNAL, "failed function declaration: no active owner");

                        Entity* member = owner->getEntity(code.getContent(i - 1));

                        if (!member)
                            return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, code.getPointOfIndex(i - 1), "no member '%%' registered in owner %% (%%)", code.getContent(i - 1), owner->name, Entity::typeStr(owner->type));
                    
                        if (!member->isNamespace())
                            return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, code.getPointOfIndex(i - 1), "member '%%' is no namespace and thus is not able to be the owner of a function", code.getContent(i - 1));

                        // TODO: WARN: functions allowed too

                        if (member->type == EntityType::FUNCTION)
                            ctx.throwd(MS_WARNING, i - 1, "owner '%%' is a function", member->name);

                        // TODO: allow only certain types of namespaces?
                        // e.g. test with owner->canContainMemberType(EntityType::FUNCTION);

                        f.owner = owner = member->ns();
                        moduleContext = true; // enable, since after an '.'-operator no '::' is allowed anymore
                    }

                    // 4) on '(' - terminator and begin of parameter list
                    else if (content == "(") {
                        fname = code.getContent(i - 1);

                        if (!owner)
                            return ctx.throwd(MS_ERROR_INTERNAL, "failed function declaration: no active owner");

                        if (owner->containsMember(fname))
                            return ctx.throwd(MS_ERROR_ENTITY_ALREADY_REGISTERED, code.getPointOfIndex(i), "member '%%' already exists in owner '%%'", fname, owner->name);

                        if (!isValidIdentifier(fname))
                            return ctx.throwd(MS_ERROR_INVALID_ENTITY_NAME, code.getPointOfIndex(i), "invalid function name '%%'", fname);

                        stage = 1;
                        lastSuccessfull = i;
                        f.name = fname;

                        continue;
                    } 
                    
                    // 5) 
                    else {
                        const std::string& next = code.getContent(i + 1);

                        if (next != "(" && next != "." && next != "::")
                            return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, i, "invalid token '%%'", content);
                    }
                }
                
                // -- Center: parameter list
                else if (stage == 1) {


                    // TODO: if default value is set AND a guard is given => check for type consistency


                    // 1) end of parameter list
                    if (content == ")") {
                        if (plitIndex == -1 && defaultVal)
                            return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, i - 1,
                                "a previous parameter had a default value assigned, all parameters after that must also have a default value");
                        
                        // pname.length() == 0 <=> function has no parameters
                        if (pname.length() > 0 && (s = f.registerParam(pname, plitIndex, pguard, pvarg)) != MS_SUCCESS)
                            return ctx.throwd(s, lastSuccessfull + 1, "duplicate parameter name '%%'", pname);

                        pname = "";
                        plitIndex = -1;
                        pguard = DataType::UNDEFINED;
                        pvarg = false;

                        // extern functions do not have a body and thus are finished by the last ')'
                        if (f.mods.isset(FMod::EXTERN)) {
                            stage = 3;
                            
                            if ((s = ctx.extHandler.linkFunction(&f)) != MS_SUCCESS)
                                return ctx.throwd(s, "no extern linking found for function signature '$b%%$r$1'", f.getSignature());

                            break;
                        }

                        stage = 2;
                        lastSuccessfull = i;

                        continue;
                    }

                    // 2) separation of parameters
                    else if (content == ",") {
                        if (plitIndex == -1 && defaultVal)
                            return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, i - 1,
                                "a previous parameter had a default value assigned, all parameters after that must also have a default value");
                        
                        if ((s = f.registerParam(pname, plitIndex, pguard, pvarg)) != MS_SUCCESS)
                            return ctx.throwd(s, lastSuccessfull + 1, "duplicate parameter name '%%'", pname);

                        pname = "";
                        plitIndex = -1;
                        pguard = DataType::UNDEFINED;
                        pvarg = false;
                        lastSuccessfull = i;
                    }

                    // 3) default values
                    else if (content == ":") {
                        // only parse right side
                        int literal = -1;
                        
                        if ((s = parseLiteral(ctx, code, i + 1, literal)) != MS_SUCCESS)
                            return s;

                        if (literal == -1)
                            return ctx.throwd(MS_ERROR_INVALID_SYNTAX, i + 1, "invalid default literal");

                        defaultVal = true;
                        plitIndex = literal;
                        pguard = ctx.literals->getDataType(literal);

                        if (pguard == DataType::STRING)
                            i += 3;
                        else
                            i++;
                    }

                    // data type & name
                    else {
                        /* a) if content is data type then put guard
                         * b) if content is '...' then enable varg
                         * c) content must be the identifier
                         */

                        // Integral guard
                        if (content == "int") {
                            pguard = DataType::INTEGRAL;
                        }

                        // Decimal guard
                        else if (content == "dec") {
                            pguard = DataType::DECIMAL;
                        }

                        // VARG
                        else if (content == "...") {
                            if (defaultVal)
                                return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, i,
                                    "a previous parameter had a default value assigned, v-arg parameters are not allowed then");
                            
                            if (f.vararg)
                                return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, i, "a function cannot hold more than one v-arg parameter");
                            
                            // Needed, or else ',... name' would put pname=,
                            if (pname.length() == 0)
                                return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, i, "v-arg can only be applied behind the parameter name");

                            pname = code.getContent(i - 1);
                            pvarg = true;
                            f.vararg = true;
                        }
                        
                        // Identifier
                        else {
                            if (pname.length() > 1 && plit != -1)
                                return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, i, "invalid token placement");
                            
                            if (pname.length() > 0)
                                return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, i, "parameter name already set (%%)", pname);
                            
                            if (!isValidIdentifier(content))
                                return ctx.throwd(MS_ERROR_INVALID_ENTITY_NAME, i, "invalid parameter name '%%'", content);
                            
                            pname = content;
                        }
                    }
                }

                // -- Suffix: additional modifiers on end (like 'override')
                else if (stage == 2) {

                    // 1) if NOT extern, the colon ends the function head
                    if (content == ":") {
                        stage = 3;

                        break;
                    }
                }

                // all other stages should be unreachable
                else {
                    return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, "unknown error at 'parseFunction', source@%%", __LINE__);
                }
            }

            if (stage < 3)
                return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, "invalid function declaration syntax");

            if (!owner)
                return ctx.throwd(MS_ERROR_MALFORMED_FUNCTION_DECLARTION, "failed to build function: no owner found");

            // put function instruction block start
            //f.startIndex = ctx.getInstructions().finalInstructions.size();

            debug::printsf(" $7> ENTER FUNCTION: %%", f.getSignature());

            if (!owner->registerOrFree(f.name, &f))
                return ctx.throwd(MS_ERROR, "failed to register function '%%' to namespace '%%'", f.name, owner->name); // MS_ERROR_FAILED_ENTITY_REGISTRATION

            if (!f.mods.isset(FMod::EXTERN))
                s = ctx.joinFunction(&f);

            return s;
        }

        /* Initializer lists:
         *
         *  {var = {expression}}, ...
         */
        state parseInitializerList(Context& ctx, Code& code, int begin, int end) {
            state s { MS_SUCCESS };
            
            int r = end - begin;
            int d = -1;
            
            std::string var {""};
            Variable* ent {nullptr};

            if (r <= 0)
                return ctx.throwd(MS_ERROR_INVALID_SYNTAX, begin, "empty initializer list");

            for (int i = begin; i < end; i++) {
                const std::string& content = code.getToken(i).content;

                if (content == ",") {
                    if (d == -1)
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, i, "missing target ('$ =' not present)");

                    Expression expr;
                    
                    if ((s = parseExpression(ctx, expr, code, d, i)) != MS_SUCCESS)
                        return ctx.throwd(s, d, i, "failed to parse expression");

                    if (ent) {
                        if (!ctx.currentNamespace)
                            return ctx.throwError(MS_ERROR_NULL, "(internal) current namespace is null");

                        // Local variable, address relative to StackFrame.
                        if (ctx.currentNamespace->type != EntityType::MODULE && ctx.currentNamespace->isLocal(ent->name)) {
                            ctx.getInstructions().append(Op::alv, ent->localAddress);
                        } else {
                            ctx.getInstructions().append(Op::agv, ent->address);
                        }
                    } else
                        return ctx.throwd(MS_ERROR_NO_ENTITY_FOR_NAME, i, "failed to assign expression result to missing target");

                    d = -1;
                }

                else if (content == "=") {
                    if (d != -1)
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, i, "'=' is not allowed here");
                    
                    var = code.getContent(i - 1);

                    if (!ctx.findEntity(var) && ms::ms_auto_create) {
                        if (!(ent = ctx.registerVariable(var, VariableType::LOCAL, DataType::UNDEFINED)))
                            return ctx.throwd(s, i - 1, "invalid variable as initializer");
                    } else
                        return ctx.throwd(s, i - 1, "entity '%%' not found. (enable ms_auto_create to allow direct initialization)", var);

                    d = i + 1;
                }
            }

            return s;
        }

        /* == Object Parser ==
         *
         *  Input syntax: "identifier : expression [,]"
         *  Output: ObjectLiteral pointer
         */
        state parseObjectLiteral(Context& ctx, Code& code, int begin, int end) {
            state s { MS_SUCCESS };
            int phase   { 0 }, // 0 - identifier, 1 - colon, 2 - expression
                entries { 0 }, // entry count so far
                eb      { 0 }, // ExpressionEnd - used to avoid using 'parseExpressionEnd' late parse the value
                level   { 0 };

            std::string identifier;
            int i = begin;

            debug::printsf("{} $6(%% .. %%) -> $4%%", begin, end, code.getContent(begin, end));

            for (; i < end; i++) {
                const std::string_view token { code.getContent(i) };

                if (phase == 0) {
                    if (!isValidIdentifier(std::string(token)))
                        return ctx.throwd(MS_ERROR_INVALID_ENTITY_NAME, i, "invalid member name '%%'", token);
                    
                    identifier = token;
                    phase++;
                } else if (phase == 1) {
                    if (token != ":")
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, i, "expected ':'");

                    phase++;
                } else if (phase == 2) {
                    if (!eb)
                        eb = i;
                    
                    else if (isOpeningParenthesis(token)) {
                        level++;
                    }

                    else if (isClosingParenthesis(token)) {
                        level--;
                    }

                    // End of value expression
                    else if (!level && token == ",") {
                        // parse expression now
                        Expression expr;

                        if ((s = parseExpression(ctx, expr, code, eb, i)) != MS_SUCCESS)
                            return ctx.throwd(s, eb, i, "failed to parse value for '%%'", identifier);

                        eb = 0; // expression begin
                        phase = 0;
                        entries++;
                    }
                }
            }

            if (phase != 2)
                return ctx.throwd(MS_ERROR_INTERNAL, "invalid object parsing phase on end (%% instead of 2)", phase);

            if (level)
                return ctx.throwd(MS_ERROR_INVALID_SYNTAX, eb, i, "got lost inside of a value (missing parenthesis)");

            // parse last expression
            Expression expr;

            if ((s = parseExpression(ctx, expr, code, eb, end)) != MS_SUCCESS)
                return ctx.throwd(s, eb, end, "failed to parse value for '%%'", identifier);

            return s;
        }

        LoopType findLoopType(Context& ctx, Code& code, int index) {
            
            /* for ..
             *  a) FROM_TO: for _iterator_ from _assign_ to _condition_ do
             *  b) UNTIL:   for _initializer_list_ until _condition_ do
             *  c) IN:      for _iterator_ in _container_ do
             */
            if (code.getContent(index) == "for") {
                for (; index < code.length(); index++) {
                    if (code.getContent(index) == "from")       return LoopType::FOR_FROM_TO;
                    else if (code.getContent(index) == "until") return LoopType::FOR_UNTIL;
                    else if (code.getContent(index) == "in")    return LoopType::FOR_IN;
                    else if (code.getContent(index) == "do" || code.getContent(index) == "end")
                        return ctx.throwAndReturn(LoopType::UNKNOWN, MS_ERROR_INVALID_SYNTAX, "malformed for-loop header");
                }
            }

            // while
            else if (code.getContent(index) == "while") {
                return LoopType::WHILE_DO;
            }

            // do .. while
            else if (code.getContent(index) == "do") {
                return LoopType::DO_WHILE;
            }

            return ctx.throwAndReturn(LoopType::UNKNOWN, MS_ERROR, "unknown loop type for input '%%'", code.getContent(index));
        }

        state parseLoop(Context& ctx, Code& code, LoopType type, int& index) {
            //LoopType type { findLoopType(ctx, code, index) };
            state s { MS_SUCCESS };

            if (type == LoopType::UNKNOWN)
                return ctx.throwd(MS_ERROR, index, "could not read the loop header");

            switch (type) {
                case LoopType::WHILE_DO:

                    /* == While DO ==
                     *
                     *  while {condition} do
                     * 
                     *   - condition: an expression resolving to a logical type
                     */
                
                    {

                        // Set current token to the first of the condition (after the 'while' token)
                        index = index + 1;

                        Expression cond;
                        uindex condEnd { findExpressionEnd(code.getTokens(), index) };
                        
                        // Mark the beginning of the condition.
                        // Must used labels since indices of instruction may change during linking.
                        int label = Instruction::nextLabel(); // will be put on the first instruction of the condition

                        ctx.getInstructions().placeLabelOnNextInstruction(label);
                        
                        // Parse the condition
                        if ((s = parseExpression(ctx, cond, code, index, condEnd)) != MS_SUCCESS)
                            return ctx.throwd(MS_ERROR, index, condEnd - 1, "invalid condition for while loop");

                        if (!cond.isLogical())
                            return ctx.throwd(MS_ERROR, index, condEnd - 1, "condition must be a logical expression");

                        index = condEnd;

                        if (code.getContent(index) != "do")
                            return ctx.throwd(MS_ERROR, index, "expected token 'do'");

                        WhileDoClause* wdc = new WhileDoClause(ctx.currentNamespace, label, lang::getOppositeJump(cond.operation));

                        if ((s = ctx.enterClause(wdc, index)) != MS_SUCCESS) {
                            delete wdc;
                            return ctx.throwd(s, index, "failed to enter while..do clause");
                        }

                        debug::printsf("$3[while..do] condition: %%", tokenContent(code.getTokens(), cond.tokenBegin, cond.tokenEnd));

                        // return MS_SUCCESS;

                    }

                    break;

                case LoopType::DO_WHILE: break;
                case LoopType::FOR_FROM_TO:

                    /* == From To ===
                     *
                     *  for {iterator} from {assignment} to {condition} do
                     * 
                     *   - iterator: an identifier to a local or global variable in the LOCAL module
                     *               (Note: can be automatically created if ms_auto_create is enabled)
                     *   
                     *   - assignment: a default value the iterator variable gets assigned to
                     * 
                     *   - condition:
                     *      a) a value of the same type like the assignment value to be tested against;
                     *         will test for "iterator < condition"
                     *      b) a custom condition to test the iterator; expression must be logical
                     */

                    { // required for the c++ compiler
                        // I) Check that only 1 identifier gets passed!
                        if (code.getContent(index + 2) != "from")
                            return ctx.throwd(MS_ERROR, index + 1, "malformed from-to loop header; iterator identifier may only be one token");

                        // II) The iterator variable
                        Entity* ent = ctx.findEntityInModule(code.getContent(++index));

                        if (!ent) {
                            if (ms_auto_create) {
                                if (!isValidIdentifier(code.getContent(index)))
                                    return ctx.throwd(MS_ERROR, index, "invalid variable identifier '%%'", code.getContent(index));

                                ent = ctx.registerVariable(code.getContent(index), VariableType::LOCAL, DataType::UNDEFINED);

                                if (!ent)
                                    return ctx.throwd(MS_ERROR, index, "failed to create the iterator variable");
                            } else
                                return ctx.throwd(MS_ERROR, index, "no variable for name '%%' found (enable ms_auto_create to automatically create the iterator)", code.getContent(index));
                        }

                        if (ent->type != EntityType::VARIABLE)
                            return ctx.throwd(MS_ERROR, index, "referenced identifier '%%' must be a variable", code.getContent(index));

                        Variable* var = static_cast<Variable*>(ent);
                        OnErrorDeleted<FromToLoop> loop (new FromToLoop);

                        loop->iterator = var;
                        loop->fromToken = index + 1;
                        
                        // III) The assigning expression
                        index += 2; // skip 'from'

                        Expression expr;
                        uindex exprEnd { findExpressionEnd(code.getTokens(), index) };

                        if ((s = parseExpression(ctx, expr, code, index, exprEnd)) != MS_SUCCESS)
                            return ctx.throwd(MS_ERROR, index, exprEnd - 1, "failed to read the assigning expression for the identifier");

                        // IV) Check that the next token is 'to'
                        if (code.getContent(exprEnd) != "to")
                            return ctx.throwd(MS_ERROR, exprEnd, "malformed from-to loop header; token 'to' expected");

                        // TODO: local vs. global (+ see below)
                        Address itAddress { var->localAddress }; // iterator address
                        ctx.getInstructions().append(Op::alv, itAddress);

                        // V) The condition
                        loop->toToken = exprEnd;
                        index = exprEnd + 1; // skip 'to'

                        Expression cond;
                        uindex condEnd { findExpressionEnd(code.getTokens(), index) };

                        if ((s = parseExpression(ctx, cond, code, index, condEnd)) != MS_SUCCESS)
                            return ctx.throwd(MS_ERROR, index - 1, "failed to read the condition of the from-to loop");

                        // Check that 'from {expr}::type' equals 'to {cond}::type'
                        // OR that to {cond} resolves to true or false <=> custom condition
                        if (cond.result.type != expr.result.type) {
                            if (!cond.isLogical()) {
                                ctx.throwd(MS_ERROR, index, condEnd - 1, " I) type of initial value does not equal to conditioned value type {%% vs %%}", expr.resultName(), cond.resultName());
                                ctx.throwd(MS_ERROR, index, condEnd - 1, "II) the 'to-expression' does not resolve to a logical value (true or false)");
                                ctx.throwError(MS_ERROR, "either I) or II) must be satisfied");

                                return MS_ERROR;
                            } else
                                loop->customCondition = true; // enable custom condition in FromToLoop struct
                        } else {
                            // Append comparison of _iterator_ < _condition_

                            // TODO local vs. global
                            ctx.getInstructions().append(Op::plref, itAddress);
                            ctx.getInstructions().append(Op::cmp);
                            
                            // TODO: clause! -> 
                        }

                        // VI) Validation
                        index = condEnd;

                        if (code.getContent(index) != "do")
                            return ctx.throwd(MS_ERROR, index, "malformed from-to loop header; token 'do' expected");
                    
                        if (!ctx.currentNamespace)
                            return ctx.throwd(MS_ERROR_INTERNAL, "no current namespace as the owner for the from-to loop");

                        debug::printsf("$3[from..to] iterator: %%, assigned to: %%, condition: %% < %%", var->name, expr.resultName(), var->name, tokenContent(code.getTokens(), loop->toToken + 1, index));
                        
                        return ctx.currentNamespace->registerEntity(loop->name, loop.release());
                    }

                    break;

                default: // should never happen
                    break;
            }

            return MS_SUCCESS;
        }

        state parse(Context& ctx, Code& code) {
            if (!code.isTokenized())
                return ctx.throwError(MS_ERROR_INVALID_CODE_STATE, "invalid state: code must be tokenized before parsing");
            
            debug::printsf(">> Parsing module $5%%$r (%% tokens):", code.getModuleName(), code.getTokens().size());

            InstructionSet& is = ctx.instructionSet;
            state s { MS_SUCCESS };

            int commentBegin {-1};
            bool comment {false}, commentSection {false};
            
            // anchor = successfully compiled token index
            for (int i = 0, anchor = 0; i < code.getTokens().size(); i++) {
                const Token& t = code.getToken(i);

                // Trigger new line; MUST be on top for comment check!
                if (t.row > ctx.codePoint.line) {
                    ctx.codePoint.column = 0;
                    ctx.codePoint.line = t.row;
                }

                // -- Commment Handling

                if (commentSection && (commentSection = !(t.is("]") && code.getContent(i + 1) == "--")))
                    continue;

                if (comment && (comment = t.row <= commentBegin))
                    continue;

                commentBegin = -1;
                
                debug::printsf("-- PARSING TOKEN [%%:%%] $2%%$r", t.row + 1, t.column + 1, t.content);

                if (t.is("--")) {
                    comment = true;
                    commentBegin = t.row;
                    commentSection = code.getContent(i + 1) == "[";

                    continue;
                }

                // -- Keyword Tokens

                if (t.is("end")) {
                    if ((s = ctx.leaveClause()) != MS_SUCCESS)
                        return ctx.throwd(s, "failed to leave clause");

                    anchor = i;
                }

                // Switches context temporarily to another module
                else if (t.is("import")) {
                    const Token& name = code.getToken(++i);

                    if (ctx.currentNamespace && ctx.currentNamespace->type != EntityType::MODULE)
                        return ctx.throwd(MS_ERROR, i - 1, "invalid import location; must be called on module level");

                    if (&name == &Code::EMPTY)
                        return ctx.throwError(MS_ERROR_NULL, "missing module name");

                    // In case of longer paths with preserved tokens,
                    // the module name can be put between ''.
                    if (name.content == "'") {
                        const_cast<Token&>(name) = code.getToken(++i);
                        ++i; // increase counter for closing '

                        debug::printsf("Path input for module: %%", name.content);
                    }

                    if (!isValidIdentifier(name.content))
                        return ctx.throwd1(MS_ERROR_INVALID_ENTITY_NAME, "invalid module name to load '%%'", name.content);

                    // Store current context
                    Snapshot* snap = ctx.createSnapshot();

                    Code* module = new Code;
                    state s = readInFile(ctx, name.content + ".ms", module);

                    if (s != MS_SUCCESS) {
                        delete module;
                        delete snap; // can be deleted here since not activated yet

                        return ctx.throwError(s, "failed to read in module '%%'", name.content);
                    }

                    // active code file; state ignored
                    s = ctx.makeActiveCode(module); // not covered by readInFile since module-Code* not generated
                    s = compile(ctx, module);

                    if (s != MS_SUCCESS) {
                        delete module;
                        delete snap;

                        return ctx.throwError(s, "failed to compile module '%%'", name.content);
                    }

                    if ((s = ctx.loadSnapshot(snap)) != MS_SUCCESS)
                        return ctx.throwError(s, "failed to import module; could not restore snapshot");

                    // update anchor
                    anchor = i;

                    debug::printsf(">> Returning to compilation of module $5%%$r:", ctx.activeCode ? ctx.activeCode->getModuleName() : "<no active module>");
                }
                
                // Registers a variable for given name
                else if (t.is("let")) {
                    const Token& name = code.getToken(++i);

                    if (&name == &Code::EMPTY)
                        return ctx.throwd1(MS_ERROR_NULL, "missing variable name");

                    if (!lang::isValidIdentifier(name.content))
                        return ctx.throwd1(MS_ERROR_INVALID_ENTITY_NAME, "invalid variable name '%%'", name.content);

                    if (!ctx.currentNamespace)
                        return ctx.throwd(MS_ERROR_INTERNAL, "no active namespace to append variable");

                    if (ctx.currentNamespace->containsMember(name.content))
                        return ctx.throwd(MS_ERROR_ENTITY_ALREADY_REGISTERED, "entity '%%' already exists", name.content);

                    if (!ctx.registerVariable(name.content, isGlobal(ctx.currentNamespace) ? VariableType::GLOBAL : VariableType::LOCAL, DataType::UNDEFINED))
                        return ctx.throwError(MS_ERROR, "failed to register entity '%%' in active namespace", name.content);

                    debug::printsf("Registered variable '%%' in namespace '%%'", name.content, getEntityName(ctx.currentNamespace, "N/A"));

                    //advance(i, 1); // skip name
                    //advance(anchor, 1);
                }

                else if (t.is("=")) {
                    uindex start = findEntityPathBegin(code.getTokens(), i);
                    uindex end = findExpressionEnd(code.getTokens(), i + 1);
                    Expression expr;

                    Variable* ent {nullptr};
                    bool directAssignment {false};
                    bool stackAssignment {false};

                    // -- Parse target
                    if (i - start == 1) {
                        const std::string name = code.getContent(start);

                        if (!(ent = (Variable*) ctx.findEntity(name))) {
                            if (ms_auto_create) {
                                if (!lang::isValidIdentifier(name))
                                    return ctx.throwd(MS_ERROR_INVALID_ENTITY_NAME, start, "invalid varibale name '%%'", name);

                                VariableType vtype = VariableType::UNDEFINED;

                                if (ctx.currentNamespace && ctx.currentNamespace->type == EntityType::MODULE)
                                    vtype = VariableType::GLOBAL;
                                else
                                    vtype = VariableType::LOCAL;

                                if (!(ent = ctx.registerVariable(name, vtype, DataType::UNDEFINED)))
                                    return ctx.throwd(MS_ERROR, code.getPointOfIndex(start), "failed to register variable '%%'", name);
                                
                                debug::printsf("Registered variable '%%' as %%", name, toString(ent, "-"));
                            } else
                                return ctx.throwd(MS_ERROR_NO_ENTITY_FOR_NAME, code.getPointOfIndex(start),
                                    "unknown entity for name '%%'\n(enable 'ms_auto_create' to allow automatic entity creation)", name);
                        }

                        directAssignment = true;
                    } else {
                        if ((s = parseExpression(ctx, expr, code, start, i)) != MS_SUCCESS)
                            return ctx.throwd(MS_ERROR_INVALID_SYNTAX, i, "failed to parse target");

                        stackAssignment = true;
                    }

                    expr.target = ent;
                    expr.context = ctx.currentNamespace;

                    // -- Parse Value
                    if ((s = parseExpression(ctx, expr, code, i + 1, end)) != MS_SUCCESS)
                        return ctx.throwError(s, "failed to parse expression");

                    // assign resulting type
                    ent->dataType = expr.result.type;

                    // TODO: remove - only test
                    //printExpression(std::cout, code, expr, expr.tokenBegin);

                    // -- Push assignment operations
                    if (!expr.assignmentIncluded) {
                        if (stackAssignment)
                            ctx.getInstructions().append(Op::as);
                        else if (directAssignment && ent) {
                            if (!ctx.currentNamespace)
                                return ctx.throwError(MS_ERROR_NULL, "(internal) current namespace is null");

                            // Local variable, address relative to StackFrame.
                            if (ctx.currentNamespace->type != EntityType::MODULE && ctx.currentNamespace->isLocal(ent->name)) {
                                ctx.getInstructions().append(Op::alv, ent->localAddress);
                            } else {
                                ctx.getInstructions().append(Op::agv, ent->address);
                            }
                        }
                    }

                    i = anchor = end - 1;
                }

                else if (t.is("debug")) {
                    int end = findExpressionEnd(code.getTokens(), i + 1);
                    Expression expr;

                    s = parseExpression(ctx, expr, code, i + 1, end);

                    if (s != MS_SUCCESS)
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, "failed to parse expression for debugging");
                    
                    is.append(Op::debug);

                    i = anchor = end - 1;
                }

                else if (t.is("def")) {
                    Function* func = new Function;

                    i += 1;
                    s = parseFunction(ctx, code, *func, i);

                    if (s != MS_SUCCESS) {
                        delete func;

                        return ctx.throwd(s, "failed to parse function");
                    }

                    anchor = i;
                }

                else if (t.is("return")) {
                    if (!ctx.currentFunction)
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, "must be in a function to return");
                    
                    int end = findExpressionEnd(code.getTokens(), i + 1);
                    Expression expr;

                    // One simple 'return' without any expression.
                    if (end <= i + 1) {
                        ctx.currentFunction->resultType = DataType::VOID;
                        ctx.currentFunction->containsReturn = false; // TODO: check for every case

                        debug::printsf("{} -> empty return: VOID");
                    } else {
                        if ((s = parseExpression(ctx, expr, code, i + 1, end)) != MS_SUCCESS)
                            return ctx.throwd(s, "failed to parse return statement");

                        // Set the function's return type to the expression type of the return statement
                        ctx.currentFunction->resultType = expr.result.type;
                        ctx.currentFunction->containsReturn = true; // TODO: check for every case

                        // If 'return' is not on function scope level but rather
                        // inside another block (if, for, ...), just append a 'ret' instruction.
                        if (ctx.clause && ctx.clause->type != ClauseType::FUNCTION) {
                            // see FuncClause::leave()
                            if (!ctx.currentFunction->containsReturn || ctx.currentFunction->returnVoid)
                                ctx.getInstructions().append(Op::ret);
                            else
                                ctx.getInstructions().append(Op::retc);
                        }
                    }

                    i = anchor = end - 1;
                }

                else if (t.is("if")) {
                    int end = findExpressionEnd(code.getTokens(), i + 1);

                    if (code.getContent(end) != "then")
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, i, "missing 'then' after if-condition");

                    Expression expr;
                    expr.appendNegationInstruction = false; // do not allow a 'cmp; negate' since the appropiate jump handles this (-1 instruction)
                    
                    if ((s = parseExpression(ctx, expr, code, i + 1, end)) != MS_SUCCESS)
                        return ctx.throwd(s, "failed to parse expression for if-statement");

                    debug::printsf("$4if => negated: %%", expr.isNegated());

                    if (expr.operation == Operation::NOT || expr.operation == Operation::NOT_EQUALS)
                        ctx.instructionSet.appendJmp(Op::jz);
                    else if (expr.operation == Operation::EQUALS) {
                        if (expr.isNegated())
                            ctx.instructionSet.appendJmp(Op::jz);
                        else
                            ctx.instructionSet.appendJmp(Op::jnz);
                    } else
                    /*

                    TODO: -> wenn nur funktionsaufruf -> WARNING?!?

                    */
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, i + 1, end - 1, "condition must derive to boolean but the major operation is not logical (%%)", (int) expr.operation);

                    IfClause* clause = new IfClause(ctx.currentNamespace);

                    if ((s = ctx.enterClause(clause)) != MS_SUCCESS) {
                        delete clause;

                        return ctx.throwd(s, "failed to enter if-clause");
                    }

                    i = anchor = end - 1;
                }

                else if (t.is("for")) {
                    s = parseLoop(ctx, code, findLoopType(ctx, code, i), i); // automatically updates 'i'

                    if (s != MS_SUCCESS)
                        return s;

                    /*
                    std::pair<uindex, ForLoopType> bounds = findForLoopBounds(ctx, code, i + 1);

                    if (bounds.second == ForLoopType::INVALID)
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, i + 1, bounds.first - 1, "invalid for-loop syntax");

                    if (bounds.second == ForLoopType::UNTIL) {
                        if ((s = parseInitializerList(ctx, code, i + 1, bounds.first - 1)) != MS_SUCCESS)
                            return ctx.throwd(s, i + 1, bounds.first - 1, "failed to read initializer list");
                    }

                    int untilPos = bounds.first;
                    int exprEnd = findExpressionEnd(code.getTokens(), untilPos + 1);

                    if (code.getContent(exprEnd) != "do")
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, exprEnd + 1, "missing 'do' after expression");

                    // -- Condition; extracted from 'if'

                    Expression expr;

                    if ((s = parseExpression(ctx, expr, code, untilPos + 1, exprEnd)) != MS_SUCCESS)
                        return ctx.throwd(s, untilPos + 1, exprEnd, "failed to parse condition of the for-loop");

                    debug::printsf("$4for => negated: %%", expr.isNegated());

                    if (expr.operation == Operation::NOT || expr.operation == Operation::NOT_EQUALS)
                        ctx.instructionSet.appendJmp(Op::jz);
                    else if (expr.operation == Operation::EQUALS) {
                        if (expr.isNegated())
                            ctx.instructionSet.appendJmp(Op::jz);
                        else
                            ctx.instructionSet.appendJmp(Op::jnz);
                    } else
                    /*

                    TODO: -> wenn nur funktionsaufruf -> WARNING?!?

                    *//*
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, i + 1, exprEnd - 1, "condition must derive to boolean but the major operation is not logical (%%)", (int) expr.operation);

                    // -- Clause

                    ForClause* clause = new ForClause(ctx.currentNamespace);

                    if ((s = ctx.enterClause(clause)) != MS_SUCCESS) {
                        delete clause;

                        return ctx.throwd(s, "failed to enter for-loop clause");
                    }

                    i = anchor = exprEnd;
                    */
                }

                else if (t.is("while")) {
                    int begin = i;

                    s = parseLoop(ctx, code, LoopType::WHILE_DO, i);

                    if (s != MS_SUCCESS)
                        return ctx.throwd(s, begin, i, "failed to parse while loop");
                }
                
                else if (t.is("(")) {
                    debug::printsf("Possible invocation of a function or proto type.");
                    debug::printsf("Continuing to parse arguments and evaluate call.");

                    Expression expr;

                    /* Since all assignments are handled at another location
                     * this marks a simple function call. The return value MUST
                     * be void since a returned value would be still on the stack
                     * without being used by any assignment.
                    */
                    expr.flags.enable(ExpressionFlag::VOID_RESULT_ALLOWED);
                    expr.flags.enable(ExpressionFlag::VOID_RESULT_REQUIRED);

                    int begin = anchor + 1; //TODO: make it work: findExpressionStart(code.getTokens(), i);
                    int end = findExpressionEnd(code.getTokens(), i + 1);

                    if ((s = parseExpression(ctx, expr, code, begin, end)) != MS_SUCCESS)
                        return ctx.throwd(MS_ERROR_INVALID_SYNTAX, begin, end, "failed to invoke entity");

                    i = anchor = end - 1;
                }
                
                // No known keyword found and no invocation available.
                else {
                    debug::printsf("unknown token");
                }

                ctx.codePoint.column++;
            }

            return s;
        }

        // -- Language Access Functions

        /* This function reads the source file content and tokenizes it afterwards.
         * If a 'result' is given, this will be the target for the data. If not,
         * the 'activeCode' of the Context will be used here and further.
         * 
         * The file name is built as following: "{ctx.modulePath}/{fileName}"
         */
        state readInFile(Context& ctx, const std::string& fileName, Code* result) {
            Code* code = result;
            bool generated {false};
            state s = MS_SUCCESS;

            if (!result) {
                try {
                    code = new Code;
                    generated = true;
                } catch (...) {
                    return ctx.throwError(MS_FATAL);
                }
            }

            // Reading the file
            ctx.programState = ProgramState::FILE_READING;

            if ((s = code->readFile(ctx.modulePath + "/" + fileName)) != MS_SUCCESS) {
                if (generated)
                    delete code;

                return s;
            }

            // Tokenizing the file's content
            TokenizerState tstate;

            ctx.programState = ProgramState::TOKENIZING;

            if ((s = ctx.getTokenizer().tokenize(*code, tstate)) != MS_SUCCESS) {
                debug::printsf("\n$1[Error@TOKENIZING] failed to tokenize code (%%%%) due to $i$4%%$r$1 (see marker)", 100.0f * tstate.getProgress(), '%', debug::stateCode(s));
                debug::printsf("$1(%%|%%:%%) $7%%$4<\n", fileName, tstate.line + 1, tstate.realColumn + 1, ctx.getTokenizer().errInfo(*code));
                
                if (generated)
                    delete code;

                return s;
            }

            if (generated)
                ctx.makeActiveCode(code);

            MS_IF_DEBUG {
                std::cout << "\nRead " << code->getTokens().size() << " tokens of " << code->lineCount() << " line(s):\n";

                for (int i = 0; i < code->lineCount(); i++) {
                    std::cout << std::setfill(' ') << std::setw(static_cast<int>(std::log10(code->lineCount())) + 1) << (i + 1);
                    std::cout << "  " << code->getLine(i) << '\n';
                }

                std::cout << '\n';
            }

            if (s == MS_SUCCESS)
                ctx.programState = ProgramState::COMPILATION;

            return s;
        }

        struct CTF_typeof : public CompileTimeFunction {
            public:
                CTF_typeof() : CompileTimeFunction("typeof") {}

                state execute(Context& ctx, Expression& caller, Expression& param) {
                    if (param.result.type == DataType::REF) {
                        if (param.info.reference) {
                            caller.result.type = DataType::OBJECT;

                            int typeOfObjId = 0;

                            // TODO: Op::push_special <address>
                            ctx.getInstructions().append(Op::push, typeOfObjId);

                            debug::printsf("$b$u$5typeof$r$b$5(%%) -> %%", ctx.getActiveCode()->getContent(param.tokenBegin, param.tokenEnd), typeName(param.result.type, param.info.reference));
                        }
                    }

                    return MS_SUCCESS;
                }
        };

        state compile(Context& ctx, Code* code) {
            state s { MS_SUCCESS };

            ctx.programState = ProgramState::COMPILATION;

            if (!code && !(code = ctx.activeCode))
                return ctx.throwError(MS_ERROR_NULL, "failed to compile code; code is null and no fallback exists");

            // First invocation of compile(ctx, code) -> code <=> entry module
            if (!ctx.entryCode)
                ctx.entryCode = code;

            for (const auto& lm : ctx.loadedModules) {
                if (lm == code->getModuleName())
                    return ctx.throwError(MS_ERROR_CODE_ALREADY_READ, "cyclic import; '%%' is an existing compilation unit", code->getModuleName());
            }

            Module* module = createModule(*code);

            if (!module)
                return ctx.throwError(MS_ERROR_NULL, "could not create module for code");

            ctx.loadedModules.push_back(code->getModuleName());
            ctx.modules.insert(std::make_pair(module->name, module));
            ctx.currentNamespace = module;

            // All non-entry modules are single instruction layers.
            if (!ctx.isEntryModule() && (s = ctx.enterBlock(module)) != MS_SUCCESS)
                return ctx.throwd(s, "failed to enter layer for module '%%'", module->name);
            
            // TODO: ======= TEST =======

            ctx.compFunctions.insert(std::make_pair("typeof", new CTF_typeof));
            debug::printsf_ignore_debug_mode("Registered 'typeof' as a compile-time function.");

            // ==========================
            
            s = parse(ctx, *code);

            if (s != MS_SUCCESS)
                return s;

            // All level > 0 modules are layers to be closed after compilation.
            if (!ctx.isEntryModule() && ctx.instructionSet.currentLayer && (s = ctx.leaveBlock()) != MS_SUCCESS)
                return ctx.throwd(s, "could not leave module layer");

            if ((s = ctx.finalize()) != MS_SUCCESS)
                return ctx.throwd(s, "failed to finalize compilation");
            
            /* When we reach level 0 <=> last module is parsed,
             * all labels and references can be resolved and
             * the LIS instruction gets added.
             */
            if (ctx.isEntryModule())
                ctx.instructionSet.complete();

            MS_IF_DEBUG {
                debug::printsf("\n.. finished compilation of module '%%'", code->getModuleName());
                
                if (ctx.currentNamespace) {
                    std::cout << debug::Console::Modifier(debug::ChatColor::FG_YELLOW);
                    std::cout << ctx.currentNamespace->toStructureString() << '\n';
                    std::cout << debug::Console::_RESET_ALL;
                }
            }

            return s;
        }

    }

    namespace runtime {

        using namespace types;
        using namespace opcode;
        using namespace memory;
        using namespace traits;

        // -- Declarations

        struct InstructionSupplier;

        class VM;
        class vm_obj;
        class vm_function;

        namespace alu {

            using Combination = IData* (*)(VM&, IData*, IData*, Operation); // left, right

            IData* errorCombination(VM& vm, IData* left, IData* right, Operation op);

            struct DataCombinator {
                constexpr static inline unsigned int TOTAL_COMBINATIONS = (unsigned int) DataType::__count__ * (unsigned int) DataType::__count__;

                Combination mapping [TOTAL_COMBINATIONS];

                DataCombinator() {
                    for (unsigned int i = 0; i < TOTAL_COMBINATIONS; i++) {
                        mapping[i] = &errorCombination;
                    }
                }

                IData* combine(VM& vm, IData* left, IData* right, Operation op) {
                    //debug::printsf("combine(%% %% %%)", toString(left), traits::stringify::toString(op), toString(right));

                    return mapping[pos(left->getType(), right->getType())](vm, left, right, op);
                }

                void putCombination(DataType left, DataType right, Combination combination) {
                    mapping[pos(left, right)] = combination;
                }

                constexpr unsigned int pos(const DataType left, const DataType right) {
                    return (unsigned int) left * (unsigned int) DataType::__count__ + (unsigned int) right;
                }
            };

            IData* combine(VM& vm, IData* left, IData* right, Operation op);

        };

        state init_vm(Context& ctx, VM& vm);
        state eval(VM&, InstructionSupplier& is, operation);
        

        // -- Definitions

        struct InstructionSupplier {
            protected:
                int count {0}, index {0}, pindex {0} /* amount of read parameters */;

            public:
                InstructionSupplier(int baseOffset = 0) : index(baseOffset) {};
                virtual ~InstructionSupplier() {}

                virtual void jump(uindex index) {
                    this->index = index; // Validation must be done on 'next()' and 'pullParam()'
                }

                // Should be true as long the index is valid
                virtual bool hasNext() = 0;

                virtual operation next() = 0;

                virtual param pullParam() = 0;

                virtual std::string getVARG() = 0;

                virtual int paramCount() = 0;

                virtual int getIndex() {
                    return index;
                }

                virtual int paramIndex() {
                    return pindex;
                }

                virtual bool hasParam() {
                    return pindex < paramCount();
                }
        };

        struct DirectInstructionSupplier : public InstructionSupplier {
            private:
                InstructionSet& iset;
                Instruction current;

            public:
                DirectInstructionSupplier(InstructionSet& instructions) : InstructionSupplier(instructions.baseOffset), iset(instructions) {
                    count = iset.finalInstructions.size();
                }

                bool hasNext() {
                    return index < count;
                }

                operation next() {
                    if (!hasNext())
                        return Op::IIS; // out of instructions -> terminate
                    
                    current = iset.finalInstructions[index++];
                    pindex = 0;

                    return current.getOp();
                }

                param pullParam() {
                    //if (pindex < current.paramCount())
                        return current.getParam(pindex++);

                    //return opcode::Instruction::INVALID_PARAM;
                }

                int paramCount() {
                    return current.paramCount();
                }

                std::string getVARG() {
                    return current.getVARG();
                }
        };

        // struct CompiledInstructionSupplier : public InstructionSupplier {};

        struct IRegister {
            bool empty {true};
            DataType type {DataType::NIL};

            virtual IData* value() = 0;
        };

        template <typename T, typename W = typename DataWrapper<T>::type>
        struct Register : public memory::MemoryContainer, IRegister {
            W* intern {nullptr};

            explicit Register(const T& d) :
                // set MemoryContainer::containerName to Register<%type%>
                MemoryContainer(debug::sformat("Register<%%>", DataInfo<T>::data_type))
            {  
                // A register is only a temporaty data storage and data will be
                // assigned to static memory containers (owner override).
                MemoryContainer::transient = true;

                intern = new W {d};
                intern->owner = this;
                IRegister::type = DataInfo<T>::data_type;

                #if defined(MS_TRACK_IDATA) && defined(MS_DEBUG)
                    debug::printsf("%% with intern IData -> %%", containerName, intern->ID);
                #endif
            }
            ~Register() {
                // freeData() is not required since the 'intern' value
                // should never be exhanged to another IData*

                delete intern;
                intern = nullptr;
            }

            void assign(const T& value) {
                //debug::printsf("$3Register<%%> := %% .. was %%", traits::stringify::toString(DataInfo<T>::data_type), value, intern->value);
            
                intern->assign(value);
            }

            state readFrom(IData* source) {
                // since intern is only a transfer value, we dont
                // care about destructing or overriding values

                if (source)
                    intern->value = source->get<T>();

                //debug::printsf("$3Register<%%> := %%", traits::stringify::toString(DataInfo<T>::data_type), intern->value);

                return MS_SUCCESS;
            }

            state loadTo(IData* target) {
                return MS_SUCCESS;
            }

            IData* value() {
                return static_cast<IData*>(intern);
            }
        };

        struct Registry {
            // Fallback
            Register<NilData> nil { NIL };

            // Basic Types
            Register<int64> integral { MS_DEFAULT_INTEGRAL_VALUE };
            Register<double64> decimal { MS_DEFAULT_DECIMAL_VALUE };
            Register<std::string> string { MS_DEFAULT_STRING_VALUE };

            // Special Need
            Register<int64> comparison { 0 };

            // -
            DataType lastUsed { DataType::NIL };

            Registry() {
                comparison.containerName = "Comparison";
            }

            template <typename T> // with valid (DataType) T
            IRegister& getRegister(T type) {
                switch ((DataType) type) {
                    case DataType::INTEGRAL: return integral;
                    case DataType::DECIMAL: return decimal;

                    default:
                        return nil;
                }
            }

            IRegister& getLastUsedRegister() {
                return getRegister(lastUsed);
            }

            template <typename T, typename W = typename DataWrapper<T>::type, DataType D = DataInfo<T>::data_type>
            IData* store(const T& value) {
                IRegister* r { &getRegister(D) };

                if (!r || r == &nil)
                    return NIL;

                //debug::printsf("$3Register<%%> := %% .. was %%", traits::stringify::toString(DataInfo<T>::data_type), value, toString(r->value()));
                
                static_cast<W*>(r->value())->assign(value);
                lastUsed = D;

                return r->value();
            }

            state store(IData* data) {
                state s { MS_SUCCESS };

                const auto read = [&](auto& r) {
                    s = r.readFrom(data);
                };

                switch (data->getType()) {
                    case DataType::INTEGRAL: read(integral); break;
                    case DataType::DECIMAL: read(decimal); break;

                    default:
                        break;
                }

                return s;
            }

            IData* getRegistryData(DataType type) {
                switch (type) {
                    case DataType::INTEGRAL: return integral.intern;
                    case DataType::DECIMAL: return decimal.intern;

                    default:
                        return nullptr;
                }
            }

            state load(uindex id, IData* target) {
                state s { MS_SUCCESS };

                const auto write = [&](auto& r) {
                    s = r.loadTo(target);
                };

                switch (id) {
                    case 0: write(integral); break;
                    case 1: write(decimal); break;
                }

                return s;
            }
        };

        class StackFrame {
            public:
                LocalMemory mem;
                int retPos {-1};

            public:
                StackFrame(int returnPos, int index, usize localCount) : retPos(returnPos),
                    mem(localCount, debug::sformat("frame[%%]", index)) {
                    debug::printsf("ENTER_FRAME (%%) ...", mem.containerName);
                }

                ~StackFrame() {
                    debug::printsf("DELETE_FRAME (%%)", mem.containerName);
                }
        };

        // -- Error Codes --
        // TODO move to top

        MS_VALUE_MAP(state, const char*) vm_error_codes {
            {MS_EXCEPT_NULL, "null value"},
            {MS_EXCEPT_OUT_OF_BOUNDS, "out of bounds"},
            {MS_EXCEPT_STACK_OVERFLOW, "stack overflow"},
            {MS_EXCEPT_STACK_UNDERFLOW, "stack underflow"},
            {MS_EXCEPT_ARTITHMETIC, "arithmethic operation"},
            {MS_EXCEPT_CONST_ASSIGN, "const assignment"}
        };

        MS_MAP_LOOKUP(vm_get_error_name, vm_error_codes, state, const char*, "MS_EXCEPT")

        // -- VM --

        #define MS_CHECK_STACK(n, msg) if (!vm.checkStack(n)) return vm.throwException(MS_EXCEPT_STACK_UNDERFLOW, "StackUnderflow", msg);

        class VM {
            friend class ext::ExtHandler;
            friend class vm_obj;
            friend class vm_function;

            friend state eval(VM& vm, InstructionSupplier& is, operation op);
            friend state init_vm(Context& ctx, VM& vm);
            
            friend IData* alu::combine(VM& vm, IData* left, IData* right, Operation op);

            public:
                std::ostream& output = std::cout;

                // -- state

                long long runtime;
                state vmstate { MS_SUCCESS };

                // -- runtime

                Context* context {nullptr};
                ConstValuePool* constants;

                Stack<int64> intStack;
                Stack<double64> doubleStack;
                Stack<IData*> op;

                int cmp {0};

                //XMap<Register, IData*> registers;
                
                LocalMemory globalMemory;

            public:
                Registry registry;
                Stack<StackFrame*> frames;

                alu::DataCombinator combinator;

                explicit VM() : globalMemory(MS_GLOBAL_MEMORY_SIZE, "globals") {
                    debug::printsf("`4VM");
                }
                virtual ~VM() {

                    // Clear op stack
                    //for (const auto& entry : op) {
                    //    delete entry;
                    //}

                    debug::printsf("`4~VM");
                }
                
                // -- control

                void exit(state code) {}

                // -- error handling

                state throwException(state code, const std::string_view msg = "-") {
                    vmstate = code;

                    return code;
                }

                template <typename... Ts>
                state throwException(state code, const std::string_view signature, const std::string_view reason, Ts... args) {
                    std::string str { debug::sformat(reason.cbegin(), args...) };

                    debug::applyColor(output, debug::ChatColor::FG_RED);

                    output << '\n';
                    output << "Execution terminated with code [";
                    output << code << "]\n";
                    output << " -> code name: " << vm_get_error_name(code) << "\n";
                    output << " -> signature: " << signature << "\n";

                    debug::applyColor(output, debug::ChatColor::FG_RED); // in case 'sformat' is used in the signature

                    output << " -> reason: " << str << "\n";
                    output << '\n';

                    output << debug::Console::_RESET_ALL;

                    return vmstate = code;
                }

                // -- memory

                inline IData* getLiteral(int index) {
                    return constants->getValue(index);
                }

                // -- util

                bool checkStack(int n) {
                    //if (op.size() < n)
                    //    throwException(MS_EXCEPT_ARTITHMETIC);

                    return op.size() >= n;
                }

                void pushComparison(int value) {
                    registry.comparison.assign(value);

                    push(registry.comparison.value());
                }

                void push(IData* data) {
                    /* I) if isPrimitive(data->getType())
                     *
                     */
                    if (data && !op.push(data))
                        throwException(MS_EXCEPT_STACK_OVERFLOW, "push");

                    //debug::printsf("[STACK+%%] << %%", op.size() - 1, toString(data));
                }

                IData* pop() {
                    IData* val { op.pop(nullptr) };

                    //debug::printsf("[STACK+%%] >> %%", op.size() + 1, toString(val));

                    return val;
                }

                // -- frames

                state enterFrame(int instructionIndex, int localc, int paramc) {
                    if (frames.size() == MS_MAX_STACK_FRAMES)
                        return throwException(MS_FATAL, "call", "max frame count reached (%%)", MS_MAX_STACK_FRAMES);

                    if (paramc > localc)
                        return throwException(MS_FATAL, "call", "param count must be <= local count");
                    
                    if (!checkStack(paramc))
                        return throwException(MS_EXCEPT_STACK_UNDERFLOW, "call", "failed to enter frame: not enough params on stack (%%/%%)", op.size(), paramc);
                    
                    StackFrame* frame = new StackFrame(instructionIndex, frames.size(), localc);
                    state s { MS_SUCCESS };

                    if (!frames.push(frame))
                        return throwException(MS_FATAL, "frame-stack", "cannot push stack frame");

                    for (int p = paramc - 1; p >= 0; p--) {
                        IData* top = pop();

                        if ((s = frame->mem.write(p, top, true)) != MS_SUCCESS)
                            return throwException(MS_FATAL, "Memory Exception", "failed to write local memory cell %%/%%", p, frame->mem.size());
                    
                        debug::printsf(" %%[%%] := %%", frame->mem.containerName, p, toString(top, true));
                    }

                    return s;
                }

        };

        namespace alu {

            // -- combinations

            IData* errorCombination(VM& vm, IData* left, IData* right, Operation op) {
                vm.throwException(MS_EXCEPT_ARTITHMETIC,
                    debug::sformat("%% %% %%",
                        stringify::toString(left ? left->getType() : DataType::NIL),
                        stringify::toString(op, "#"),
                        stringify::toString(right ? right->getType() : DataType::NIL)),
                    "no known combination of following tuple available:\n   # %%\n   # %%\n   # %%", toString(left, true), op, toString(right, true));

                return NIL;
            }

            // --

            template <typename L, typename R>
            IData* combinePrimitives(VM& vm, const L& l, const R& r, Operation op) {
                IData* d { nullptr };

                const auto add = [&]() { return l + r; };
                const auto sub = [&]() { return l - r; };
                const auto mul = [&]() { return l * r; };
                const auto div = [&]() { return l / r; };

                const auto apply = [&](auto opfunc) {
                    d = vm.registry.store<decltype(opfunc())>(opfunc());
                };

                const auto combine = [&]() {
                    switch (op) {
                        case Operation::ADD:        apply(add); break;
                        case Operation::SUBSTRACT:  apply(sub); break;
                        case Operation::MULTIPLY:   apply(mul); break;
                        case Operation::DIVIDE:     apply(div); break;

                        default:
                            return NIL;
                    }

                    return d;
                };

                return combine();
            }

            IData* combine(VM& vm, IData* left, IData* right, Operation op) {
                return vm.combinator.combine(vm, left, right, op);
            }

        }

        // -- Functions

        #define MS_ALU_PRIMITIVE_COMBI(ltransform, rtransform) [](VM& vm, IData* left, IData* right, Operation op) { return alu::combinePrimitives(vm, ltransform(left), rtransform(right), op); }

        state init_vm(Context& ctx, VM& vm) {
            // TODO: check for mode
            // only do this on direct (ctx->vm) mode
            vm.context = &ctx;
            vm.constants = ctx.literals;
            // see VM::VM() -> vm.globalMemory.allocate(MS_GLOBAL_MEMORY_SIZE);

            vm.combinator.putCombination(DataType::INTEGRAL, DataType::INTEGRAL, MS_ALU_PRIMITIVE_COMBI(extractInt, extractInt));
            vm.combinator.putCombination(DataType::INTEGRAL, DataType::DECIMAL, MS_ALU_PRIMITIVE_COMBI(extractInt, extractDouble));
            
            return MS_SUCCESS;
        }

        int compare(IData* left, IData* right) {
            // 1) numerical: a - b
            // 2) string: full compare
            // 3) objects:
            //  a) identity: a.id == b.id
            //  b) content: for x in obj do compare(a.x, b.x) end

            if (memory::checkType(left, DataType::INTEGRAL) && memory::checkType(right, DataType::INTEGRAL)) {
                return extractInt(left) - extractInt(right);
            }

            return 0;
        }

        state push_meta(VM& vm, std::string_view attribute) {
            if (!vm.checkStack(1))
                return vm.throwException(MS_EXCEPT_STACK_UNDERFLOW, "op-stack", "failed to push meta since the stack is empty");

            IData* data = vm.pop();

            // TODO: do generic meta retrieval

            if (attribute == "type") {
                //memory::Container* c = new memory::Container(1);

                // memory::ProtoObject* po { link::instantiateProto(vm, __type_proto_id__, ...) };

                // c->set("value", data->getType())

                //vm.push(c);
            }

            return MS_SUCCESS;
        }

        state call_function(VM& vm, vm_function& f) {

        }

        state eval(VM& vm, InstructionSupplier& is, operation op) {
            state s { MS_SUCCESS };

            if (!vm.context)
                return vm.throwException(MS_EXCEPT_INVALID_STATE, "vm_state", "the VM was not initialized yet. Call 'init_vm' before!");

            MS_IF_DEBUG {
                OpInfo info { getOpInfo((Op) op) };

                debug::printsf("$b$5%%] %% $r$7%%", is.getIndex() - 1, info.a, is.getVARG().length() > 0 ? std::string("#" + is.getVARG()) : "");
            }

            // --- CONTROL ---

            if (op == Op::NOP) {

            }

            else if (op == Op::IIS) {
                vm.exit(MS_EXIT_IIS);
            }

            else if (op == Op::LIS) {
                vm.exit(MS_EXIT_LIS);
            }

            // special

            else if (op == Op::cmd) {
                param command = is.pullParam();

                switch (command) {
                    case MS_CMD_EXTRACT_META:
                        if ((s = push_meta(vm, is.getVARG())) != MS_SUCCESS)
                            return s;

                        break;

                    default:
                        return vm.throwException(MS_EXCEPT, "invalid command", "command '%%' does not exist", command);
                }
            }

            // debug

            else if (op == Op::debug) {
                if (!vm.checkStack(1))
                    return vm.throwException(MS_EXCEPT_STACK_UNDERFLOW, "op-stack", "operation 'debug' requires one parameter");

                //IData* data = vm.pop();

                //debug::printsf_ignore_debug_mode("$4$b$uDebug$r$4$b:$r %%", memory::toString(data));

                IData* d { vm.pop() };

                if (d) {
                    if (d->isType(DataType::INTEGRAL))
                        std::cout << static_cast<Integral*>(d)->value << '\n';
                }

                //std::cout << toString(vm.pop()) << '\n';

                //freeData(data);
            }

            // --- Function CONTROL ---

            else if (op == Op::call) {
                /* doMetaControl();
                    -> MetaInfo info = getMetaInfoForOpIndex(is.getIndex());
                 */

                param pos = is.pullParam();
                param locals = is.pullParam();
                param params = is.pullParam();

                debug::printsf(" locals: %%\n params: %%", locals, params);

                s = vm.enterFrame(is.getIndex(), locals, params);

                is.jump(pos);
                
            }

            else if (op == Op::extcall) {
                param id = is.pullParam();
                param pcount = is.pullParam();
                
                if ((s = vm.context->ext().call(*vm.context, vm, id, pcount)))
                    return vm.throwException(s, "failed operation 'extcall'");
            }

            else if (op == Op::retc) {
                if (vm.frames.size() > 0) {
                    StackFrame* frame = vm.frames.pop(nullptr);
                    int jmp = frame->retPos;

                    if (!vm.checkStack(1))
                        return vm.throwException(MS_EXCEPT_STACK_UNDERFLOW, "retc", "'retc' requires one element on the op-stack as the return result");
                    
                    IData* data = (*vm.op.top());

                    if (data) {
                        debug::printsf(" return value: %%", toString(data, true));

                        // Only clone data if it is a local variable and
                        // not a temporary (register) value.
                        if (data->owner == &frame->mem) {
                            debug::printsf(" locals.contains(value) == true -> copy");

                            vm.push(data->clone());
                        }
                    } else return vm.throwException(MS_EXCEPT, "retc", "return value is null");

                    is.jump(jmp);

                    delete frame;
                } else return vm.throwException(MS_EXCEPT, "retc", "no available stack frame to leave");
            }

            // --- MEMORY ---

            else if (op == Op::plit) {
                param index = is.pullParam();
                IData* val { vm.getLiteral(index) };

                ms_debug(" index: %% -> %%", index, toString(val, true))

                if (!val)
                    return vm.throwException(MS_EXCEPT_OUT_OF_BOUNDS, "constants[]", "invalid literal index -- %% exceeds [0..%%]", index, vm.constants->getCount() - 1);
                        
                // Clone the literal (literals are read-only) and
                // ignore all modifiers since the copy is mutable.
                vm.op.push(val);
            }

            else if (op == Op::pgv) {
                Address addr = is.pullParam();
                IData* data { nullptr };

                if (!vm.globalMemory.valid(addr))
                    return vm.throwException(MS_ERROR, "Invalid Memory Address", "invalid global address %%", addr);

                data = vm.globalMemory.read(addr);
                //data->addReference(); //

                // TODO: if primitiveType(data) -> clone()
                //     else                     -> use data and addReference()
                //if (checkType(data, DataType::OBJECT)) {

                //}

                // Just push the raw IData*. Any further handling
                // is done separately and has the ability to reduce
                // data consumption.
                vm.push(data);

                //debug::printsf(" {Stack} << %%", toString(data));
            }

            else if (op == Op::plref) {
                param index = is.pullParam();
                IData* data = (*vm.frames.top())->mem.read(index);

                vm.push(data);

                debug::printsf(" << %%", toString(data));
            }

            else if (op == Op::pattr) {
                // allow some certain default attributes like 'type'
                // so everything (even literals) have a X.type field
                // .. maybe name it a bit more special like '__type'
                // OR
                // just have something like '__info' to be a reference
                // to a reflection container to give more information
                // without storing all this in a single IData*
                // .. this might even refer to a meta pool
            }

            else if (op == Op::agv) {
                if (!vm.checkStack(1))
                    return vm.throwException(MS_EXCEPT_STACK_UNDERFLOW, "agv", "requires one element to store");

                Address addr = is.pullParam();
                IData* head { vm.pop() };

                if (!vm.globalMemory.valid(addr))
                    return vm.throwException(MS_ERROR, "Invalid Memory Address", "invalid global address %%", addr);

                if ((s = vm.globalMemory.write(addr, head, true)) != MS_SUCCESS)
                    return vm.throwException(s, "Memory Exception", "failed to write '%%' to cell %% (%%)", toString(head, true), addr, toString(vm.globalMemory.read(add), true));

                //debug::printsf(" globals[%%] := %%", addr, toString(vm.globalMemory.read(addr), true));
            }

            else if (op == Op::alv) {
                param index = is.pullParam();
                IData* data = vm.pop();

                debug::printsf(" %%[%%] := %%", (*vm.frames.top())->mem.containerName, index, toString(data, true));

                (*vm.frames.top())->mem.write(index, data, true);
            }

            // --- ARRAYS ---

            else if (op == Op::collect) {
                param size = is.pullParam();

                if (!vm.checkStack(size))
                    return vm.throwException(MS_EXCEPT_STACK_UNDERFLOW, "op-stack", "too many values requested to collect from stack [%%/%%]", size, vm.op.size());

                //memory::Container* c = new memory::Container(size);
                auto c = new Data<int64, DataType::CONTAINER>(size);

                for (int i = 0; i < size; i++) {
                    vm.pop();
                }

                vm.push(c);
            }

            // --- LOGICAL & JUMP ---

            else if (op == Op::cmp) {
                MS_CHECK_STACK(2, "'cmp' requires two values to compare")

                IData* right { vm.pop() };
                IData* left  { vm.pop() };
                
                int diff = compare(left, right);

                //debug::printsf(" compare: %% - %% => %%", toString(left), toString(right), diff);

                //vm.push(new Integral(diff));
                vm.pushComparison(diff);

                //freeData(left);
                //freeData(right);
            }

            else if (op == Op::jmp) {
                Address addr = is.pullParam();

                is.jump(addr);

                //debug::printsf(" jumped to address %%", addr);
            }

            else if (op == Op::jz) {
                MS_CHECK_STACK(1, "'jz' requires one argument")

                Address addr = is.pullParam();
                IData* x { vm.pop() };

                if (!x)
                    return vm.throwException(MS_ERROR, "Null", "argument cannot be nullptr");

                if (!checkType(x, DataType::INTEGRAL))
                    return vm.throwException(MS_ERROR, "Type Mismatch", "'jz' requires an memory::Integral as argument but was %%",
                        traits::stringify::toString((DataType) x->getType()));

                int y = extractInt(x);

                if (y == 0) {
                    is.jump(addr);

                    debug::printsf(" jz to %%", addr);
                } else {
                    debug::printsf(" jz failed since {%%} != 0", y);
                }

                memory::freeData(x);
            }

            else if (op == Op::jnz) {
                IData* cmp = vm.pop();

                
            }

            else if (op == Op::jgez) {
                MS_CHECK_STACK(1, "'jgez' requires one argument")

                Address addr = is.pullParam();
                IData* x { vm.pop() };

                if (!x)
                    return vm.throwException(MS_ERROR, "Null", "argument cannot be nullptr");

                if (x->type != (short) DataType::INTEGRAL)
                    return vm.throwException(MS_ERROR, "Type Mismatch", "'jgez' requires an memory::Integral as argument but was %%",
                        traits::stringify::toString((DataType) x->getType()));

                int y = extractInt(x);

                if (y >= 0) {
                    is.jump(addr);

                    ms_debug(" jgez to %%", addr)
                } else
                    ms_debug(" jgez failed since {%%} < 0", y)
            }

            // --- ARTITHMETIC ---

            else if (op == Op::add) {
                MS_CHECK_STACK(2, "expected two values to add")

                IData* right  { vm.pop() };
                IData* left   { vm.pop() };
                IData* result = alu::combine(vm, left, right, Operation::ADD);

                //DataType res = lang::getResultingType(left->getType(), right->getType());
                //IData* reg = vm.registry.getRegistryData(res);

                //s = vm.combinator.combine(vm, left, right, Operation::ADD);

                if (is.hasParam()) {
                    param addr = is.pullParam();

                    // When in function
                    if (vm.frames.size()) {
                        if ((s = (*vm.frames.peek())->mem.write(addr, result, true)) != MS_SUCCESS)
                            return vm.throwException(MS_EXCEPT, "add {target}", "failed to write function data: %% to address %% [code %%]", toString(result), addr, debug::stateCode(s));
                    } else
                        // global
                        if ((s = vm.globalMemory.write(addr, result, true)) != MS_SUCCESS)
                            return vm.throwException(MS_EXCEPT, "add {target}", "failed to write global data: %% to address %% [code %%]", toString(result), addr, debug::stateCode(s));
                
                    #ifdef MS_DEBUG
                        debug::printsf(" %%[%%] := %%", vm.frames.size() ? "local" : "global", addr, toString(result, true));
                    #endif
                } else
                    vm.push(result);

                //freeData(left);
                //freeData(right);
            }

            /*
            else if (op == Op::add) {
                MS_CHECK_STACK(2, "add");

                IData* a = vm.pop();
                IData* b = vm.pop();

                if (a->isType(DataType::INTEGRAL) && b->isType(DataType::INTEGRAL)) {
                    vm.push(new Integral(extractInt(a) + extractInt(b)));
                } else if (a->isType(DataType::INTEGRAL) && b->isType(DataType::DECIMAL)) {
                    vm.push(new Decimal(static_cast<double64>(extractInt(a)) + extractDouble(b)));
                } else if (a->isType(DataType::DECIMAL) && b->isType(DataType::INTEGRAL)) {
                    vm.push(new Decimal(extractDouble(a) + static_cast<double64>(extractInt(b))));
                }

                freeData(a);
                freeData(b);

                // ...

                if (a->isType(DataType::REF) && b->isType(DataType::REF)) {
                    Ref* ref = static_cast<Ref*>(a);

                    // recursive ... while a is Ref -> a = a->getReferenced()

                    //assignData(ref->getReference(), new Integral(0));

                    // freeData(ref)
                }

                // -- output

                // error: failed addition: empty operands
                //   [op+0] NIL
                //   [op+1] Integral*
            }
            */

            return s | vm.vmstate;
        }

    }

    namespace ext {

        state ExtHandler::call(comp::Context& ctx, runtime::VM& vm, int id, int pcount) {
            if (id < 0 || id >= linkings.size())
                return MS_ERROR_INVALID_EXT_LINK;

            ExtLink* lnk = linkings[id];

            if (!lnk)
                return MS_ERROR_NULL;

            if (!vm.checkStack(pcount))
                return vm.throwException(MS_EXCEPT_STACK_UNDERFLOW, "op-stack", "Not enough parameters for ext-call [%%/%%].", pcount, vm.op.size());

            Call call;
            IData* data {nullptr};

            debug::printsf("$2[EXT] Invoking external call for function $u%%$r$2 (%% parameter(s)).", lnk->getIdentifier(), pcount);

            for (int i = pcount - 1; i >= 0; i--) {
                data = vm.pop();

                debug::printsf("$2[EXT] <%%> %% := %%", i, lnk->getParam(i), toString(data, false));

                call.data.insert(std::make_pair(lnk->getParam(i), data));
            }

            return lnk->call(ctx, vm, call, pcount);
        }

    };

    namespace run {

        using namespace types;
        using namespace comp;
        using namespace runtime;

        class ProgramArgs {
            private:
                int cliArgCount;

            public:
                bool directExecution {false};

            public:
                explicit ProgramArgs(int argc, char ** argv) {
                    
                }

                bool isPresent(const std::string_view name) {
                    return false;
                }

            private:
                bool parse(int argc, char ** argv) {
                    return true;
                }
        };

        // ms -c --ac file.ms -x --step --analysis

        state loadArgs(Context& ctx, ProgramArgs& args) {
            if (args.isPresent("-step")) {

            }

            // disable auto create
            if (args.isPresent("-ac")) { // TODO: args.getArgState("ac") == ArgState::DISABLE
                // syntax: --ac -step -debug
                ms::ms_auto_create = false;
            }

            return MS_SUCCESS;
        }

        state execute(Context& ctx, const std::string& fileName, const ProgramArgs& args = ProgramArgs {0, nullptr}) {
            state s { MS_SUCCESS };

            // Read File & Compile
            try {

                // TODO: loadArgs(ctx, args);

                s = readInFile(ctx, fileName);
                // TODO: check that s == MS_SUCCESS
                s = compile(ctx);

            } catch (const MSException& e) {
                s = e.getCode();

                std::cout << e << "\n";
            } catch (const std::exception& e) {
                s = MS_FATAL;

                std::cout << "\nCaught Exception:\n";
                std::cout << "! Compilation failed due to a std::exception:\n";
                std::cout << "! " << e.what() << '\n';
                std::cout << "\n";
            }

            MS_IF_DEBUG {
                if (s == MS_SUCCESS) {
                    debug::printsf("$2== END OF COMPILATION ==");

                    std::cout << "\nFunction Call(s) (" << ctx.getFunctionCalls().size() << "):\n";

                    for (const auto& fcall : ctx.getFunctionCalls()) {
                        if (fcall)
                            std::cout << ' ' << *fcall << '\n';
                    }

                    std::cout << "\nLiteral(s) (" << ctx.literals->getCount() << "):\n";

                    for (int i = 0; i < ctx.literals->getCount(); i++) {
                        if (ctx.literals->getValue(i))
                            std::cout << std::hex << i << std::dec << ": " << toString(ctx.literals->getValue(i)) << '\n';
                    }

                    std::cout << "\n\n" << debug::toString(ctx.getInstructions()) << '\n';
                } else {
                    debug::printsf("$1== END OF COMPILATION <%%> ==\n", s);

                    return s; // exit on error state
                }
            }

            // -- if direct execution is enabled then do this

            long long executionTime = 0LL;

            if (!args.directExecution)
                return MS_SUCCESS;

            // Runtime & VM
            try {

                using ull = unsigned long long;

                VM vm;
                InstructionSupplier* supply = new DirectInstructionSupplier(ctx.getInstructions());

                if ((s = init_vm(ctx, vm)) != MS_SUCCESS) {
                    debug::printsf("$1failed to initialize VM: CODE %%", s);

                    return s;
                }

                debug::printsf("-- VM:started --\n");

                auto startTime = std::chrono::steady_clock::now();
                operation lop = Op::IIS; // longest operation
                ull lexet = 0; // longest execution time
                ull total = 0ull;
                ull steps = 0;

                if (is_vm_analysis && !ms_is_debug) {
                    std::cout << "     [ Longest instructions ]\n";
                    std::cout << "[   op   | time (" << (char) 230 << "s) |    step    ]\n";
                    std::cout << "> ------------------------------- <\n";
                }

                while (supply->hasNext()) {
                    auto istart = std::chrono::steady_clock::now();
                    operation op = supply->next();

                    if ((s = eval(vm, *supply, op)) != MS_SUCCESS)
                        return s;

                    auto iend = std::chrono::steady_clock::now();
                    auto idur = std::chrono::duration_cast<std::chrono::microseconds>(iend - istart);
                    auto idurnano = std::chrono::duration_cast<std::chrono::nanoseconds>(iend - istart);

                    total += idurnano.count();
                    steps++;

                    if (idur.count() > lexet) {
                        lexet = idur.count();
                        lop = op;

                        if (is_vm_analysis && !ms_is_debug) {
                            std::cout << "[ " << debug::stretch(opcode::getOpInfo((Op) op).a, 6);
                            std::cout << " | " << debug::stretch(std::to_string(lexet), 9);
                            std::cout << " | " << debug::stretch(std::to_string(steps), 10) << " ]\n";
                        }
                    }

                    #if defined(MS_VM_ANALYSIS) && defined(MS_DEBUG)
                        debug::printsf("$2 .. %% micros", idur.count());
                    #endif
                }

                auto endTime = std::chrono::steady_clock::now();
                auto duration = std::chrono::duration_cast<std::chrono::microseconds>(endTime - startTime);

                executionTime = duration.count();

                if (is_vm_analysis) {
                    std::cout << "> ------------------------------- <\n";
                    std::cout << "*   longest op: " << getOpInfo((Op) lop).a << " @ " << lexet << (char) 230 << "s\n";
                    std::cout << "*   total time: " << total << "ns\n";
                    std::cout << "*  total steps: " << steps << '\n';
                    std::cout << "* avg. op time: " << (total / steps) << "ns\n";
                    std::cout << "> ------------------------------- <\n";
                }

                delete supply;
                supply = nullptr;

            } catch (const MSException& e) {
                s = e.getCode();

                std::cout << e << "\n";
            } catch (const std::exception& e) {
                s = MS_FATAL;

                std::cout << "VM failed due to error: " << e.what() << " (std::exception)\n";
            }

            // error
            if (s != MS_SUCCESS) {
                debug::printsf("`1-- VM:terminated with code %% --$r\n", s);
            } else {
                debug::printsf("\n$2-- VM:terminated successfully (%%) --\n", s); // took xxxms
            }

            debug::printsf_ignore_debug_mode("\n`2Execution Time: %% %%seconds", executionTime < 1000000 ? executionTime : executionTime / 1000000, executionTime < 1000000 ? "micro " : "");

            return s;
        }

    }

    namespace debug {

        using namespace comp;

        void visualizeExpression(std::ostream& stream, Code& code, Expression* expression) {
            if (!expression)
                return;

            #ifdef MS_DEBUG

                Expression* left  {expression->left};
                Expression* right {expression->right};
                bool visualizing  {true};

                int t_begin {expression->tokenBegin};
                int t_end {expression->tokenEnd};
                int t_op {expression->tokenOperator};

                while (visualizing) {
                    if (!left && !right)
                        break;

                    int opOffset = t_op - t_begin; // offset for the /|\ symbols

                    // print sequence
                    stream << tokenContent(code.getTokens(), t_begin, t_end);
                    stream << '\n';
                    stream << std::string(opOffset - 1, ' ');
                    stream << "/|\\";
                }

            #endif
        }

    };

    // String formatting
    namespace traits::stringify {

        template <>
        struct string_format<DataType> {
            std::string operator ()(const DataType dt) {
                switch (dt) {
                    case DataType::INTEGRAL:
                        return std::string("integral");
                    case DataType::DECIMAL:
                        return std::string("decimal");
                    case DataType::STRING:
                        return std::string("string");

                    case DataType::REF:
                        return std::string("ref");
                    case DataType::VOID:
                        return std::string("void");

                    case DataType::INVALID:
                        return std::string("invalid");
                    case DataType::UNDEFINED:
                        return std::string("undefined");

                    default:
                        return std::string("<unknown data type (" + std::to_string(static_cast<int>(dt)) + ")>");
                }
            }
        };

        template <>
        struct string_format<lang::TypeSpec> {
            std::string operator ()(const lang::TypeSpec ts) {
                if (ts.isRef())
                    return debug::format("*%%", toString(ts.refType));

                if (ts.isFCall())
                    return debug::format("%%()", "TODO: fcall->fname(fcall->param[i]..)");

                return toString(ts.dataType);
            }
        };

    }

    namespace experimental {

        using namespace types;
        using namespace lang;

        // --

        

        // --

        struct DataMod {};

        struct Data {
            TypeSpec type;
            DataMod mods;
            Address address;
        };

        struct ControlBlock {
            Data* entries;
            size_t count;
        };

        struct MemoryContainer {
            ControlBlock control;
            void* data;

            ~MemoryContainer() {
                free(data);
            }

            bool valid(Address address) {}
        };

        template <typename T>
        struct Register {
            T value;
        };

        void init(MemoryContainer& c) {
            c.data = malloc(100);
        }

        template <typename T>
        void write(MemoryContainer& mc, Address address, const T& value) {
            ((T*) mc.data)[address] = value;
        }

        template <typename T>
        T read(MemoryContainer& mc, Address address) {
            return ((T*) mc.data)[address];
        }

        template <typename T>
        void fillRegister(Register<T>& target, MemoryContainer& src, Address address) {
            target.value = read<T>(src, src.control.entries[address].address);
        }

        void printStructure(MemoryContainer& c) {
            for (size_t s = 0; s < c.control.count; s++) {
                std::cout << std::hex << s << ": " << traits::stringify::toString(c.control.entries[s]) << " -> " << c.control.entries[s].address << std::dec << '\n';
            }
        }

        void test() {
            MemoryContainer mc;
            Register<int64> ireg;

            init(mc);

            mc.control.entries = new Data[1];
            mc.control.count = 1;
            mc.control.entries[0] = Data {TypeSpec(DataType::INTEGRAL), DataMod{}, 0};

            printStructure(mc);
            write(mc, 0, (int64) 10);
            fillRegister(ireg, mc, 0);

            std::cout << "Content of Register<int64>: " << ireg.value << '\n';
        }

    }

    #ifdef MS_ENABLE_GLOBAL_NAMESPACE

        using namespace types;
        using namespace traits;
        using namespace util;
        using namespace memory;
        using namespace lex;
        using namespace lang;
        using namespace comp;
        using namespace runtime;
        using namespace run;

    #endif

}

#endif