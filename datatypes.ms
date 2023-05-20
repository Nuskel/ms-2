let i = 0 -- i has type int
let d = 0.3 -- d has type decimal
let s = 'hello' -- s has type string

i = i + d -- i now has type decimal
i = i + s -- i now has type string

--

let a = [] -- a has type array<any>
let o = {} -- o has type object
let p = Proto -- p has type proto
let t = [i, d, s] -- t has type tupel<int, decimal, string>

a = a + t -- a now has type array<tupel<int, decimal, string>>

--

def reduce(array, initial, combiner):
    let res = initial -- res has type of initial

    for e in array do -- array MUST be a collection (array, tupel, object, proto)
        combiner(res, e) -- combiner MUST be a function with 2 params = type of res and type of nth-element in array
    end

    return res
end

def add(x...):
    return reduce(x, 0, lambda(l, n): l + n end)
end

def add(x, y, z):
    return x + y + z
end

let fail = reduce(9, 'init', 7)
-- error: invalid param type <int>; expected <array>

let NOTfail = add(1, 's', 9.3)

---


Type reduction tree:

-------------> type reduction order!

x + y * a + b
 \ /     \ /
  z       w
   \     /
    \   /
     \ /
      q