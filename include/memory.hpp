#ifndef MS_MEMORY_HPP
#define MS_MEMORY_HPP

#include <cstdint>
#include <iostream>
#include <string>
#include <variant>
#include <span>
#include <type_traits>

#include "env.hpp"
#include "misc.hpp"

/* ----- */

#define MS_MAKE_TYPE_INFO(typeId, typeName) template <> \
  struct TypeInfo<TypeId::typeId> : TypeInfo<TypeId::typeId, typeName> {}

#define MS_MAKE_TYPE_INFO_SIMPLE(typeId, typeName) template <> \
  struct TypeInfo<TypeId::typeId> : TypeInfo<TypeId::typeId, typeName, true> {}

// msx - ms execution (runtime part)
namespace msx {

  using Integral = long long;
  using Decimal = long double;
  using Address = long int;
  using String = std::string;

  struct Proto {};

  struct Object {};

  struct Function {};

  enum class DataType {

    UNSET,

    INTEGRAL,
    DECIMAL,
    STRING,

    OBJECT,
    PROTO,
    FUNCTION

  };

  using ValueUnion = std::variant<Integral, Decimal, String, Proto, Object, Function>;

  struct CompValue {

    DataType type { DataType::UNSET };
    ValueUnion value;

    template <typename T>
    T get() {
      switch (type) {
        case DataType::INTEGRAL:
          return std::get<Integral>(value);

        default:
          return "";
      }
    }

  };

  static CompValue makeIntegral(Integral i) {
    CompValue val { .type = DataType::INTEGRAL, .value = i };

    return val;
  }

}

// types
namespace ms::memory {

  using Byte = char;
  //using ByteBuf = Byte*;

  using Int = int32_t;
  using Int8 = int8_t;
  using Int16 = int16_t;
  using Int32 = int32_t;
  using Int64 = int64_t;

  using Dec = double;
  using Dec32 = float;
  using Dec64 = double;

  using Str = std::string;
  using WStr = std::wstring;

  struct IObject;

  /* --- */

  using TypeId_ = unsigned char;

  enum class TypeId : TypeId_ {

    NIL   = 0x0,
    UNDEF = 0x1,

    INT   = 0x10,
    INT8  = 0x11,
    INT16 = 0x12,
    INT32 = 0x13,
    INT64 = 0x14,

    DEC   = 0x20,
    DEC32 = 0x23,
    DEC64 = 0x24,

    STR   = 0x30,
    WSTR  = 0x31,
    ESTR  = 0x32, // encoded byte array -> ansi, utf8, ...

    OBJ   = 0x40,
    PROTO = 0x41, // annotated structure included
    FUNC  = 0x42,

    ARRAY = 0x50,

    ANY   = 0xFF

  };

  template <TypeId ID = TypeId::UNDEF, typename TYPE = void, bool _SIMPLE = false>
  struct TypeInfo {

    using type = TYPE;

    static constexpr TypeId TYPEID {ID};
    static constexpr bool SIMPLE {_SIMPLE};
    static constexpr unsigned char SIZE { (ID == TypeId::NIL || ID == TypeId::UNDEF) ? 0 : sizeof(TYPE) };

  };

  MS_MAKE_TYPE_INFO_SIMPLE(INT, Int);
  MS_MAKE_TYPE_INFO_SIMPLE(INT8, Int8);

  MS_MAKE_TYPE_INFO_SIMPLE(DEC, Dec);
  MS_MAKE_TYPE_INFO_SIMPLE(DEC32, Dec32);
  MS_MAKE_TYPE_INFO_SIMPLE(DEC64, Dec64);

  MS_MAKE_TYPE_INFO(OBJ, IObject);

  /* --- */

  struct MemUtil {

    template <
      TypeId TYPE,
      typename T = typename TypeInfo<TYPE>::type,
      unsigned char SIZE = TypeInfo<TYPE>::SIZE
    >
    static inline T read(
      Byte* ptr,
      size_t offset = 0)
    {
      T tmp {0};

      for (size_t i = 0; i < SIZE; i++) {
        tmp |= (*(ptr + offset * SIZE + i) & 0xFF) << ((i % SIZE) * 8);
      }

      return tmp;
    }

    /* Read an array of data.
     *
     * @param in Data buffer to read from
     * @param out Target buffer to write data to
     * @param offset Optional. Offset (in sizeof(T) units) in the input buffer
     */
    template <
      TypeId TYPE,
      typename T = typename TypeInfo<TYPE>::type,
      unsigned char SIZE = TypeInfo<TYPE>::SIZE,
      size_t N
    >
    static inline void read(
      Byte* in,
      std::span<T, N> out,
      size_t offset = 0)
    {
      T tmp {0};

      for (size_t i = 0, k = 0; i < SIZE * out.size(); i++) {
        tmp |= (*(in + offset * SIZE + i) & 0xFF) << ((i % SIZE) * 8);
        
        /* on last byte (when i+1 is new entry), write to output */
        if (((i + 1) % SIZE) == 0) {
          out[k++] = tmp;
          tmp = 0;
        }
      }
    }

    template <
      TypeId TYPE,
      typename T = typename TypeInfo<TYPE>::type,
      unsigned char SIZE = TypeInfo<TYPE>::SIZE
    >
    static inline void write(
      Byte* ptr,
      T data,
      size_t offset = 0)
    {
      for (size_t i = 0; i < SIZE; i++) {
        *(ptr + offset * SIZE + i) = (data >> ((i % SIZE) * 8)) & 0xFF;
      }
    }

    template <
      TypeId TYPE,
      typename T = typename TypeInfo<TYPE>::type,
      unsigned char SIZE = TypeInfo<TYPE>::SIZE,
      size_t N
    >
    static inline void write(
      Byte* target,
      std::span<T, N> data,
      size_t offset = 0)
    {
      for (size_t i = 0; i < SIZE * data.size(); i++) {
        *(target + offset * SIZE + i) = (data[i / SIZE] >> ((i % SIZE) * 8)) & 0xFF;
      }
    }

    static inline Int readInt(Byte* ptr) {
      return read<TypeId::INT>(ptr);
    }

    static inline void writeInt(Byte* ptr, Int i) {
      write<TypeId::INT>(ptr, i);
    }

  };

  struct ByteBuf {

    Byte* data    { nullptr };
    size_t size   { 0 };
    size_t offset { 0 };

    ByteBuf(Byte* _data, size_t _size) : data(_data), size(_size) {}

    ByteBuf(ByteBuf&& another) {
      this->data = std::move(another.data);
      this->size = another.size;
      this->offset = another.offset;
    }

    bool canRead(size_t bytes);

    size_t bytesLeft();

    // Int

    Int readInt();

    Int16 readInt16();

    Int32 readInt32();

    Int64 readInt64();

    void writeInt(Int);

    void writeInt16(Int16);

    void writeInt32(Int32);

    void writeInt64(Int64);

    // Decimal

    Dec readDec();

    Dec32 readDec32();

    Dec64 readDec64();

    void writeDec(Dec);

    void writeDec32(Dec32);

    void writeDec64(Dec64);

  };

  struct AByteBuf : ByteBuf {

    AByteBuf(size_t count) : ByteBuf(nullptr, count) {
      this->data = new Byte[count];
      this->size = count;
      this->offset = 0;
    }

    ~AByteBuf() {
      delete[] this->data;
    }

  };

  template <TypeId TYPE, typename T = typename TypeInfo<TYPE>::type>
  struct DataBus {

    static void write(ByteBuf target, T data);

    static T read(ByteBuf source);

  };

  struct IData {

    static const IData NIL;
    static const IData UNDEF;

    TypeId typeId { TypeId::UNDEF };
    Byte* ptr     { nullptr };

    ~IData() {
      // std::cout << "deleting IData " << this << std::endl;
    }

    friend inline std::ostream& operator <<(std::ostream& s, IData data) {
      switch (data.typeId) {
        case TypeId::INT:
          s << MemUtil::read<TypeId::INT>(data.ptr);
          break;

        default:
          break;
      }

      return s;
    }

  };

  const inline IData IData::NIL { .typeId = TypeId::NIL, .ptr = nullptr };
  const inline IData IData::UNDEF { .typeId = TypeId::UNDEF, .ptr = nullptr };

  template <TypeId TYPE, typename T = typename TypeInfo<TYPE>::type>
  struct RData : IData {

    T& self;
    T* _ref;

    RData(T& ref) : self(ref), _ref(&ref) {
      typeId = TYPE;
      ptr = reinterpret_cast<Byte*>(&ref);
    }

    T* operator->() {
      return reinterpret_cast<T*>(ptr);
    }

    operator T&() {
      return self; // TODO: ptr
    }

  };

  template <TypeId TYPE, size_t COUNT = 1, typename T = typename TypeInfo<TYPE>::type>
  struct XData : IData {

    T buf[COUNT];
    size_t size {COUNT};

    XData() {
      typeId = TYPE;
      ptr = reinterpret_cast<Byte*>(buf);
    }

    XData(const T& ini) {
      static_assert(COUNT == 1, "expected count 1");

      typeId = TYPE;
      ptr = reinterpret_cast<Byte*>(buf);
      buf[0] = ini;
    }

    XData(const std::vector<T>& ini) {
      if (ini.size() != COUNT) {
        // TODO: throw error
      } else {
        typeId = TYPE;
        ptr = reinterpret_cast<Byte*>(buf);

        for (size_t i = 0; i < COUNT; i++) {
          buf[i] = ini[i];
        }
      }
    }

    T* operator ->() {
      static_assert(COUNT == 1, "expected count 1");

      return buf; // TODO: ptr
    }

    T& operator [](size_t index) {
      if (index >= 0 && index < COUNT)
        return buf[index]; // TODO: ptr
      
      throw "out of bounds";
    }

  };

  /* Static IData variant.
   * Has an internal buffer of size 1*sizeof(T).
   */
  template <TypeId TYPE, typename T = typename TypeInfo<TYPE>::type>
  struct SData : IData {

    static constexpr inline unsigned char SIZE = TypeInfo<TYPE>::SIZE;

    Byte buf[SIZE];

    SData() { ptr = buf; typeId = TYPE; }
    SData(T ini) { ptr = buf; typeId = TYPE; write(ini); }
    ~SData() {
      // std::cout << "deleting SData" << iniVal << "\n";
    }

    inline void write(T data) {
      if constexpr (TypeInfo<TYPE>::SIMPLE) {
        MemUtil::write<TYPE>(buf, data);
      } else {
        DataBus<TYPE>::write(ByteBuf { buf, SIZE }, data);
      }
    }

    inline T read() const {
      if constexpr (TypeInfo<TYPE>::SIMPLE) {
        return MemUtil::read<TYPE>(buf);
      } else {
        return DataBus<TYPE>::read(ByteBuf { buf, SIZE });
      }
    }

  };

  /* TODO: allocated data with auto deletion
   * ...
   */
  struct AData : IData {

    using IData::typeId;
    using IData::ptr;

    ~AData() {
      delete ptr;
    }

  };

  /* --- */

  template <typename T>
  concept Serializable = requires (T t, ByteBuf buf) {
    { t.ser_(buf) } -> std::same_as<T&>;
    { t.deser_(buf) } -> std::same_as<void>;
  };

  struct IArray {
    protected:
      size_t size {0};
      Byte* data {nullptr};

    public:
      IArray() {}

      inline IArray& deserialize(ByteBuf buf) {
        Int32 size = buf.readInt32();

        return *this;
      }
  };

  struct IObject {

    protected:
      HMap<std::string, IData> data;

    public:
      // TODO: remove, will be used in proto + ProtoDef* with proto structure and type defs
      std::string protoName;

      inline void put(const std::string& key, const IData& value) {
        data[key] = value; // not emplace since this would not update the value

        std::cout << "put: " << key << " = " << value << std::endl;
      }

      inline const IData& get(const std::string& key) const {
        if (data.find(key) == data.end()) {
          return IData::UNDEF;
        }

        return data.at(key);
      }

      inline void dump(int d = 0) {
        const std::string spacer(d * 2, ' ');
        const std::string margin(d * 2 + 1, ' ');

        std::cout << spacer << protoName << "#{\n";

        for (const auto& e : data) {
          if (e.second.typeId == TypeId::OBJ) {
            IObject other = *reinterpret_cast<IObject*>(e.second.ptr);

            std::cout << margin << '"';
            std::cout << e.first << "\":\n";
            other.dump(d + 1);
          } else {
            std::cout << margin << '"';
            std::cout << e.first << "\" (" << (int) e.second.typeId << "): ";
            std::cout << e.second << ",\n";
          }
        }

        std::cout << spacer << "}\n";
      }

  };

}

// structs
namespace ms::memory {

  template <typename T>
  struct Memory {

    size_t capacity {0};
    size_t size {0};
    T* data {nullptr};

    Memory(size_t capacity) {
      this->allocate(capacity);
    }

    virtual ~Memory() {
      if (data) {
        delete[] data;
      }
    }

    void allocate(size_t capacity) {
      this->data = new T[capacity];
      this->capacity = capacity;

      for (size_t i = 0; i < capacity; i++) {
        this->data[i] = 0;
      }
    }

    const T* const at(size_t index) const {
      return (data + index);
    }

    T read(size_t index) const {
      if (index >= capacity)
        throw "out of bounds";

      return data[index];
    }

    int write(size_t index, const T& data) {
      if (index >= capacity)
        return 0;
      
      this->data[index] = data;

      return 1;
    }

  };

  template <typename T>
  struct Stack : Memory<T> {

    using Memory<T>::capacity;
    using Memory<T>::size;
    using Memory<T>::data;

    size_t pos {0};

    const T* const pop() {
      if (pos == 0)
        return nullptr;
      
      return data[pos--];
    }

    void push(const T& data) {
      if (pos < capacity)
        this->data[pos++] = data;
    }

  };

  template <typename T>
  struct DataSerializer {

    static int read(const Memory<char>& memory, size_t index, T& result) {
      return -2;
    }

    static int write(Memory<char>& memory, size_t index, const T& data) {
      return -2;
    }

  };

  template <>
  struct DataSerializer<msx::Integral> {

    static int read(const Memory<char>& memory, size_t index, msx::Integral& i) {
      char b0 = memory.read(index);
      char b1 = memory.read(index + 1);
      char b2 = memory.read(index + 2);
      char b3 = memory.read(index + 3);

      i = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);

      return 1;
    }

    static int write(Memory<char>& memory, size_t index, const msx::Integral& i) {
      memory.write(index,      i        & 0xFF);
      memory.write(index + 1, (i >>  8) & 0xFF);
      memory.write(index + 2, (i >> 16) & 0xFF);
      memory.write(index + 3, (i >> 24) & 0xFF);

      return 1;
    }

  };

  template <typename T>
  struct DataCell {

    char type;
    T data;

  };

  struct CellMemory {

    Memory<char> container;

    CellMemory(size_t capacity) : container(capacity) {}

    template <typename T, typename S = DataSerializer<T>>
    DataCell<T> read(size_t index) {
      char type = container.read(index);
      T value;

      S::read(container, index + 1, value);

      return DataCell<T> { .type = type, .data = value };
    }

    template <typename T, typename S = DataSerializer<T>>
    void write(size_t index, char type, const T& value) {
      container.write(index, type);
      S::write(container, index + 1, value);
    }

  };

  struct Block {

    Block* previous {nullptr};
    Block* next {nullptr};
    Block* nextEmpty {nullptr};
    void* start {nullptr};
    size_t size {0};
    bool used {false};

  };

  template <typename T>
  struct Single {

    Block* block;

    Single() {}

    Single(Block* b) : block(b) {}

    inline operator T() const {
      return *reinterpret_cast<T*>(block->start);
    }

    inline T& operator ()() const {
      return *reinterpret_cast<T*>(block->start);
    }

    inline T& operator =(const T& rhs) const {
      return ((*reinterpret_cast<T*>(block->start)) = rhs);
    }

  };

  template <typename T>
  struct Array {

    Block* block;
    size_t length {0};

    Array() {}

    Array(Block* b) : block(b), length(b->size / sizeof(T)) {}

    inline T& operator [](size_t index) const {
      return *(reinterpret_cast<T*>(block->start) + index);
    }

  };

  struct BlockMemory {

    Block* first {nullptr};
    Block* last  {nullptr};
    Block* firstEmpty {nullptr};
    size_t used;
    size_t unused;

    Memory<char> container;

    BlockMemory(size_t bytes) : container(bytes), unused(bytes), used(0) {}

    virtual ~BlockMemory();

    /* Allocates a new block of given size. This block will be
     * either a split sublock of a greater or the first available
     * empty block with sufficient size.
     * 
     * @param bytes Size of the new block
     * @return The newly allocated block or nullptr on fail
     */
    Block* allocate(size_t bytes);

    Block* resize(Block* block, size_t newSize);

    /* Splits an unused block into a left and right part.
     * The left block is newly allocated at the given size
     * while the original block becomes the right one with
     * reduced size. This operation behaves like an insertation
     * of the new block before the given one.
     *
     * @param block Block to be split
     * @param size Size in bytes of the new block
     * @retun Newly created block or nullptr on fail
     */
    Block* split(Block*, size_t);

    void merge(Block* left, Block* right);

    void free(Block* block);

    // Check field and type; https://stackoverflow.com/a/62162411
    template <typename T>
      requires requires (T t) {
        { t.block } -> std::same_as<Block*&>; // & is necessary!
      }
    inline void free2(const T& container) {
      this->free(container.block);
    }

    template <typename T>
      requires MS_REQUIRE_FIELD(block, Block*)
    inline void free2_(const T& container) {
      this->free(container.block);
    }

    inline void free3(const auto& container)
      requires requires { container.block; }
    {
      this->free(container.block);
    }

    /*
     * @param size 0 means any
     */
    Block* findNextEmpty(const Block* start, size_t size);

  };

  template <typename T>
  Single<T> allocate(BlockMemory& memory) {
    return memory.allocate(sizeof(T));
  }

  template <typename T>
  Array<T> allocate(BlockMemory& memory, size_t count) {
    return memory.allocate(count * sizeof(T));
  }

  /*
  int test() {
    BlockMemory mem(1024);
    Block* chunk = mem.allocate(64);

    Single<int> single = allocate<int>(mem);

    single = 20;
    int f = single();
    int g = single;
    
    Array<int> arr = allocate<int>(mem, 4);
    arr[1] = 12;

    mem.free2(arr);
    mem.free3(single);

    struct S { int i; int block; } s;
    s.i = 20;

    // mem.free2(s);
  }
  */

  struct Data {

    char type;
    void* data;

    Data() {}

    Data(void* buffer) {}

  };

  struct DataPos {

    void* data;
    size_t offset;

  };

  static inline bool same_type(IData lhs, IData rhs) {
    return ((char) lhs.typeId ^ (char) rhs.typeId) == 0;
  }

  struct ProgramStack {

    void* data;
    size_t ptr;

    ProgramStack() {}

    template <typename T>
    T pop();

    template <typename T>
    void push(const T&);

    void advance(size_t bytes);

  };

  /* Registers */

  template <typename T>
  struct Register
  {
    using value_type = T;

    static constexpr int size = sizeof(T);

    protected:
      T value;

    public:
      Register() {}
      Register(T initial) : value(initial) {}
      ~Register() {}

      inline void store(const T data) {
        value = data;
      }

      inline void storeFrom(const T* data) {
        value = *data;
      }

      inline T read() const {
        return value;
      }

      void readTo(T* receiver) const {
        *receiver = value;
      }
  };

}

/* Arithmetics */
namespace ms::memory {

  template <typename T>
  concept DataStreamable = requires (T t) {
    { t.data } -> std::same_as<void*&>;
  };

  template <typename T>
  static inline T* atByteOffset(void* data, size_t bytes) {
    return reinterpret_cast<T*>((void*) (((char*) data) + bytes));
  }

  template <typename T>
  size_t write(DataPos target, char type, const T& data) {
    if (type == 1) {
      *atByteOffset<char>(target.data, target.offset) = type;
      *atByteOffset<int>(target.data, target.offset + 1) = data;

      return 1 + sizeof(T);
    }
  }

  // TODO: check, does not work like MemUtil::write
  static Int readInt(Byte* ptr) {
    Byte b0 = (*ptr) & 0xFF;
    Byte b1 = (*(ptr + 1)) & 0xFF;
    Byte b2 = (*(ptr + 2)) & 0xFF;
    Byte b3 = (*(ptr + 3)) & 0xFF;

    return 0 | b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);
  }

  static void writeInt(Byte* ptr, Int i) {
    *ptr = i & 0xFF;
    *(ptr + 1) = (i >> 8) & 0xFF;
    *(ptr + 2) = (i >> 16) & 0xFF;
    *(ptr + 3) = (i >> 24) & 0xFF;
  }

  template <>
  struct DataBus<TypeId::INT, Int> {

    static void write(ByteBuf target, Int data) {
      if (target.bytesLeft() >= TypeInfo<TypeId::INT>::SIZE)
        target.writeInt(data);

      // Todo: throw error?
    }

    static Int read(ByteBuf source) {
      if (source.bytesLeft() >= TypeInfo<TypeId::INT>::SIZE)
        return source.readInt();
      
      return 0; // Todo: throw ERROR ?
    }

  };

  template <>
  struct DataBus<TypeId::OBJ, IObject> {

    static void write(ByteBuf target, const IObject& data) {
      *reinterpret_cast<IObject*>(target.data) = data;
    }

    static IObject& read(ByteBuf source) {
      return *reinterpret_cast<IObject*>(source.data);
    }

  };

  /* Instrinsic add operation.
   * Writes the result back to the output data.
   */
  inline void add(IData lhs, IData rhs, IData out) {
    if (same_type(lhs, rhs)) {
      if (lhs.typeId == TypeId::INT) {
        Int _lhs = readInt(lhs.ptr);
        Int _rhs = readInt(rhs.ptr);

        writeInt(out.ptr, _lhs + _rhs);
      }
    }

    /* OBJECT */
    if (lhs.typeId == TypeId::PROTO) {
      /*
      const ProtoDef pdef = 
       */
    }
  }

  /*

  x + y -> (x, y)
  (x, y) -> z

  e.g.:

  1 + 2 -> (1, 2) -> int + dec
  2 + 1 -> (2, 1) -> dec + int
  

  
  */

  static Int addInts(Int lhs, Int rhs) {
    constexpr static auto size = TypeInfo<TypeId::INT>::SIZE;

    Byte buf[size * 2];
    Byte out[size];

    writeInt(buf, lhs);
    writeInt(buf + size, rhs);

    IData _lhs { .typeId = TypeId::INT, .ptr = buf };
    IData _rhs { .typeId = TypeId::INT, .ptr = buf + size };
    IData _res { .typeId = TypeId::INT, .ptr = out };

    add(_lhs, _rhs, _res);

    return readInt(out);
  }

  inline size_t add(Data lhs, Data rhs, DataPos out) {
    // same type
    if ((lhs.type ^ rhs.type) == 0) {
      if (lhs.type == 1 /* integral */) {
        int i0 = *reinterpret_cast<int*>(lhs.data);
        int i1 = *reinterpret_cast<int*>(rhs.data);
        int iX = i0 + i1;

        size_t bytesWritten = write(out, lhs.type, iX);

        return bytesWritten;
      }
    }

    // return Status::SUCCESS;
  }

}

#undef MS_MAKE_TYPE_INFO

#endif