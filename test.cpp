#include <iostream>
#include <string>
#include <chrono>
#include <span>

#include "memory.hpp"

void testByteBuf() {
    using namespace ms::memory;

    AByteBuf buf {64};

    buf.writeInt(200);
    buf.writeInt(19);

    Int i1 = buf.readInt();
    Int i2 = buf.readInt();
    Int result = i1 + i2;

    std::cout << "ByteBuf Result: " << i1 << " + " << i2 << " = " << result << std::endl;

    MemUtil::write<TypeId::INT>(buf.data, 123456);
    Int i = MemUtil::read<TypeId::INT>(buf.data);

    std::cout << "ByteBuf MemUtil: " << i << std::endl;

    // MemUtil

    Int ibuf[4];
    Int ibuf2[] = {11, 7, 5, 3};

    /*
    MemUtil::write<TypeId::INT>(buf.data, 3, 0);
    MemUtil::write<TypeId::INT>(buf.data, 5, 1);
    MemUtil::write<TypeId::INT>(buf.data, 7, 2);
    MemUtil::write<TypeId::INT>(buf.data, 11, 3);
    */
    MemUtil::write<TypeId::INT>(buf.data, std::span {ibuf2});
    MemUtil::read<TypeId::INT>(buf.data, std::span {ibuf});

    std::cout << "ByteBuf MemUtil Array Single: " << MemUtil::read<TypeId::INT>(buf.data, 2) << std::endl;
    std::cout << "ByteBuf MemUtil Array Multiple: " << ibuf[0] << ", " << ibuf[1] << ", " << ibuf[2] << ", " << ibuf[3] << std::endl;
}

void testObject() {
    using namespace ms::memory;

    IObject o;
    o.protoName = "Layer 0";

    IObject obuf[1];
    obuf[0].put("hello", SData<TypeId::INT>{16});
    IData objField { .typeId = TypeId::OBJ, .ptr = reinterpret_cast<Byte*>(obuf) };

    IObject o4;
    o4.put("next", SData<TypeId::INT>{12});
    IData objField3 { .typeId = TypeId::OBJ, .ptr = reinterpret_cast<Byte*>(&o4) };

    IObject o3;
    o3.put("test", SData<TypeId::INT>{17});
    o3.put("xx", objField3);
    XData<TypeId::OBJ> objField2 {o3};

    IObject o5;
    o5.put("zz", SData<TypeId::INT>{99});
    o5.put("yy", SData<TypeId::INT>{17});
    RData<TypeId::OBJ> objField4 {o5};

    static_cast<IObject&>(objField4).put("xyz", SData<TypeId::INT>{77});
    objField4->protoName = "objField4";
    objField4->dump();
    objField4->put("abc", SData<TypeId::INT>{91});
    objField4->dump();
    objField4->put("abc", SData<TypeId::INT>{911});
    o5.dump();

    XData<TypeId::OBJ> objField5;
    objField5[0].put("gg", SData<TypeId::INT>{91});
    objField5[0].put("gg", SData<TypeId::INT>{791919191});
    objField4->put("__", objField5);
    //objField5.self.put("99", SData<TypeId::INT>{88});
    //objField4->put("__", objField5);

    SData<TypeId::INT> intField1 {12};
    SData<TypeId::INT> intField2 {3};

    std::cout << "-- 1\n";

    //writeInt(intField1.buf, 12);
    //writeInt(intField2.buf, 3);

    //o.data.emplace(std::make_pair("123", intField1));
    //o.data.emplace(std::make_pair("456", intField2));

    o.put("123", intField1);
    o.put("123", SData<TypeId::INT>{123456789});
    o.put("456", intField2);

    o.put("obj", objField);
    o.put("obj2", objField2);
    o.put("obj4", objField4);

    //writeInt(intField1.ptr, 19);

    //o.put("123", intField2);

    //o.data.emplace(std::make_pair("123", intField1));

    std::cout << "-- 2\n";

    Int field1 = readInt(o.get("123").ptr);
    //field1 = MemUtil::readInt(o.get("123").ptr);
    std::cout << "-- 3\n";
    Int field2 = readInt(o.get("456").ptr);

    std::cout << "Field1 = " << field1 << " (" << (int) o.get("123").typeId << "), Field2 = " << field2 << ", nil= " << (int) o.get("nil").typeId << std::endl;

    o.dump();
}

void testIntAdd() {
    using namespace ms::memory;

    const auto start = std::chrono::steady_clock::now();

    Byte buf[32];
    Byte out[16];

    writeInt(buf, 14);
    writeInt(buf + TypeInfo<TypeId::INT>::SIZE, 3);

    IData lhs { .typeId = TypeId::INT, .ptr = buf };
    IData rhs { .typeId = TypeId::INT, .ptr = buf + TypeInfo<TypeId::INT>::SIZE };
    IData res { .typeId = TypeId::INT, .ptr = out };

    add(lhs, rhs, res);
    Int result = readInt(out);

    result = addInts(29, 2);

    const auto end = std::chrono::steady_clock::now();
    const auto diff = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start);

    std::cout << "Result = " << result << ". Took " << diff.count() << "ns " << std::endl;
}

int main(int argc, char** argv) {
    testByteBuf();
    testObject();
    testIntAdd();

    return 0;
}