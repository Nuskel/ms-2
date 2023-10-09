#include "memory.hpp"

namespace ms::memory {

    size_t ByteBuf::bytesLeft() {
        return size - offset;
    }

    Int ByteBuf::readInt() {
        offset -= 4;

        Byte b0 = *(data + offset);
        Byte b1 = *(data + offset + 1);
        Byte b2 = *(data + offset + 2);
        Byte b3 = *(data + offset + 3);

        return b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);
    }

    void ByteBuf::writeInt(Int i) {
        *(data + offset) = i & 0xFF;
        *(data + offset + 1) = (i >> 8) & 0xFF;
        *(data + offset + 2) = (i >> 16) & 0xFF;
        *(data + offset + 3) = (i >> 24) & 0xFF;

        offset += 4;
    }

}

namespace ms::memory {

    /* Block Memory */

    BlockMemory::~BlockMemory() {
        Block* b = first;

        while (b) {
            b = b->next;

            delete b;
        }

        container.~Memory();
    }

    Block* BlockMemory::findNextEmpty(const Block* start, size_t size) {
        if (size > unused) {
            return nullptr;
        }

        Block* e = firstEmpty;

        if (!e) {
            e = first;
        }

        while (e && (e->size < size || e->used)) {
            e = e->next;
        }

        return e;
    }

    Block* BlockMemory::split(Block* b, size_t size) {
        /* Can only split when at least one more byte is usable. */
        if (!b || b->size <= size || b->used) {
            return nullptr;
        }

        Block* n = new Block;

        n->size = size;
        n->used = false;
        n->previous = b; // next ...
        n->next = b->next;

        b->size = b->size - size;
        b->next = n;

        return n;
    }

    Block* BlockMemory::allocate(size_t bytes) {
        if (bytes > unused) {
            return nullptr;
        }

        Block* empty = findNextEmpty(firstEmpty, bytes);

        if (!empty) {
            return nullptr;
        }

        const size_t sdiff = empty->size - bytes;
        Block* p = empty->previous;
        Block* n = empty->next;

        if (sdiff > 0) {
            Block* relink = split(empty, bytes);

            empty->next = relink;
            relink->next = n;
            relink->previous = empty;

            if (n) n->previous = relink;

            firstEmpty = relink;
        } else {
            firstEmpty = findNextEmpty(empty, 0);
        }

        used += bytes;
        unused -= bytes;

        empty->used = true;

        return empty;
    }

}