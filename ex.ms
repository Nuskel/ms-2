let a = {f: 2, b: {i: 2 + f()}}
let o = {}
let x = o.a.c.d + 2

--*
let a = {f: 2, b: {i: 2 + f()}}

CREATE OBJ
PUSH INT 2
ASSIGN HASH f
CREATE OBJ
PUSH INT 2
CALL FUNC f
ADD
ASSIGN HASH i
ASSIGN b
*--


