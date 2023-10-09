proto status:
    enum ACTIVE
    enum INACTIVE
end

proto player:
    member name = 'name'
    member status = status.ACTIVE
end

def print(x):
    __inbuilt_log(x.name, x.status)
end

p = player()
p.name = 'Max'

print(p)
print(1)

---

STRUCT player (
    0:hash(name)
    1:hash(status)
)

@fun(print, x: any)
LEA 0, hash(name)
LEA 0, hash(status)
CALL @fun(__inbuilt_log)
RET

@fun(__implicit_main__)
ALLOC ref(player)
MOV 0
LEA 0, hash(name)
PUSH static('Max')
ASSIGN
PUSH 0
CALL @fun(print)
PUSH intermediate(1)
CALL @fun(print)

---

% target, lhs, rhs
add G[0], L[0], L[:16]

(A) MOV + Stack

push global(0x2)
push local(0x1)
add
pop local(0x2)

(B) One call

add local(0x2), local(0x1), global(0x2)