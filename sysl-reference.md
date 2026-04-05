# Sysl Language Reference Manual

Sysl is a systems programming language targeting the TRISC architecture. It combines Go-style syntax with Swift-inspired memory management (value types, reference-counted refs, raw pointers) and C-level control over memory layout.

---

## Program Structure

```sysl
module path.to.module

import path.to.other.{name1, name2}
import path.to.all.*

// Top-level declarations: functions, variables, structs, enums, type aliases, externs
```

### Modules and Imports

```sysl
module oskit.kernel              // module declaration (one per file)

import posix.stdlib.{malloc, free}   // named imports
import posix.string.*                // wildcard import
import posix.io.{open => fopen}      // aliased import
```

### Visibility

```sysl
private myHelper() -> int = 42      // not exported
myPublicFunc() -> int = 0           // public by default
```

---

## Types

### Scalar Types

| Type | Alias | Size | Description |
|------|-------|------|-------------|
| `i8` | | 1 byte | signed 8-bit integer |
| `i16` | | 2 bytes | signed 16-bit integer |
| `i32` | `int` | 4 bytes | signed 32-bit integer |
| `i64` | | 8 bytes | signed 64-bit integer |
| `u8` | `byte` | 1 byte | unsigned 8-bit integer |
| `u16` | | 2 bytes | unsigned 16-bit integer |
| `u32` | `char` | 4 bytes | unsigned 32-bit integer (Unicode codepoint) |
| `u64` | | 8 bytes | unsigned 64-bit integer |
| `f64` | `double` | 8 bytes | 64-bit floating point |
| `bool` | | 1 byte | `true` or `false` |
| `void` | | 0 bytes | no value |
| `string` | | 16 bytes | fat pointer: `{ptr: *u8, len: i64}` |

### Composite Types

```sysl
*T              // raw pointer (8 bytes, unmanaged)
&T              // ref-counted reference (8 bytes, auto-freed at rc=0)
[n]T            // fixed-size array (n * sizeof(T) bytes, stack-allocated)
[]T             // slice: {ptr: *T, len: i32, cap: i32} (16 bytes)
&[]T            // ref-counted heap array from new [n]T
(T1, T2, T3)   // tuple (desugars to anonymous struct)
func(P1, P2) -> R  // function pointer (8 bytes)
```

### Struct Types

```sysl
struct Point
    x: int
    y: int

struct Node
    value: int
    next: *Node       // recursive via pointer
```

### Enum Types (Simple)

Simple enums are integer constants with auto-incrementing values:

```sysl
enum Color
    Red               // 0
    Green             // 1
    Blue = 10         // explicit value
    Yellow            // 11 (auto-increment)
```

Access via `Color.Red`, `Color.Blue`, etc. At runtime, simple enum members are plain `i32` values.

### Tagged Unions (Data Enums)

Enums can carry data in each variant (Rust-style tagged unions):

```sysl
enum Shape
    Circle(radius: int)
    Rect(w: int, h: int)
    Empty                   // no-data variant
```

**Construction:**
```sysl
s = Circle(5)              // variant with data
e = Empty                  // no-data variant (bare name)
e2 = Shape.Empty           // qualified name also works
```

**Pattern matching:**
```sysl
s match
    Circle(r) -> r * r * 3    // destructure fields
    Rect(w, h) -> w * h       // bind multiple fields
    Empty -> 0                 // match no-data variant
```

**As function parameters and return values:**
```sysl
area(s: Shape) -> int
    s match
        Circle(r) -> r * r * 3
        Rect(w, h) -> w * h
        Empty -> 0

make_shape(kind: int) -> Shape
    if kind == 0 then Circle(5)
    else Rect(3, 4)
```

**Guards on variant patterns:**
```sysl
s match
    Circle(r) if r > 10 -> 1   // guard with binding
    Circle(r) -> 2
    Rect(w, h) -> 3
```

**Memory layout:** `{tag: i32, padding, data: union of variant fields}`. The tag is a small integer (0, 1, 2...) identifying the variant. Data is overlapping storage sized to the largest variant. `sizeof(Shape)` returns the total size including tag and padding.

### Type Aliases

```sysl
type IntPtr = *int
type Callback = func(int) -> int
```

---

## Three Allocation Modes

The same struct definition supports three usage modes at the use site:

| Declaration | Type | Semantics |
|---|---|---|
| `var v = Point(10, 20)` | `Point` (value) | stack-allocated, bitwise copy, no refcount |
| `val r = new Point(10, 20)` | `&Point` (ref) | heap-allocated, ref-counted, auto-freed at zero |
| `var p: *Point = &v` | `*Point` (pointer) | raw, unmanaged, kernel-safe |

### Conversion Rules

- `ref -> value`: not implicit (use `.copy()`)
- `value -> ref`: `new Point(v)`
- `ref -> ptr`: `&r` (unsafe, no refcount change)
- `ptr -> ref`: **always an error** (can't manufacture a refcount)
- `value -> ptr`: `&v` (address-of)
- `ptr -> value`: `*p` (dereference)

---

## Variables

```sysl
// Immutable
val x = 42
val y: int = 42

// Mutable
var x = 42
var y: int = 42
x = 100              // reassignment

// Uninitialized (zero-initialized)
var x: int
var arr: [10]int
var p: *Node

// Inferred type (mutable by default in blocks)
x = 42               // inferred as int
name = "hello"       // inferred as string
```

### Global Variables

```sysl
var count = 0         // module-level mutable
val MAX = 100         // module-level immutable

main() -> int
    count += 1
    count
```

---

## Functions

```sysl
// Expression body
add(a: int, b: int) -> int = a + b

// Block body
factorial(n: int) -> int
    if n <= 1
        return 1
    return n * factorial(n - 1)

// Void function (no return type)
greet(name: *byte)
    puts(name)

// Inferred return type
double(x: int) = x * 2

// No parameters
getAnswer() -> int = 42
```

### Methods

Methods are functions named `StructName_methodName` with a `self` parameter:

```sysl
struct Point
    x: int
    y: int

Point.magnitude(self: *Point) -> int
    self.x * self.x + self.y * self.y

main() -> int
    var p: Point
    p.x = 3
    p.y = 4
    p.magnitude()     // desugars to Point_magnitude(&p)
```

### Deinit Blocks

```sysl
struct Buffer
    data: *byte
    size: int

Buffer.deinit(self: *Buffer)
    free(self.data)   // called automatically when &Buffer refcount hits 0
```

### Defer

```sysl
main() -> int
    f = open("file.txt", O_RDONLY)
    defer close(f)     // runs when function exits
    // ... use f ...
    42                 // close(f) runs after return value is computed
```

Multiple defers execute in LIFO order.

### Function Pointers

```sysl
dbl(x: int) -> int = x * 2

main() -> int
    f: func(int) -> int = dbl
    f(21)                         // indirect call → 42

    var funcs: [2]func(int) -> int
    funcs[0] = dbl
    funcs[1] = triple
    funcs[0](10) + funcs[1](10)  // call through array
```

### Extern Declarations

```sysl
extern putchar(ch: int)
extern sbrk(increment: int) -> *i8
extern var errno: int
```

---

## Expressions

### Literals

```sysl
42                    // int (i32)
0xFF                  // hex literal
100u32                // typed literal suffix
3.14                  // double (f64)
'A'                   // char literal (u32, value 65)
'\n'                  // escape char
"hello"               // string literal
true, false           // bool
[1, 2, 3]            // array literal
```

### Operators (by precedence, lowest to highest)

| Precedence | Operators | Associativity |
|---|---|---|
| 1 | `\|\|` | left |
| 2 | `&&` | left |
| 3 | `==` `!=` `<` `>` `<=` `>=` | left (chainable) |
| 4 | `\|` | left |
| 5 | `^` | left |
| 6 | `&` | left |
| 7 | `<<` `>>` | left |
| 8 | `+` `-` | left |
| 9 | `*` `/` `%` | left |
| 10 | `-` `!` `~` `*` `&` `++` `--` (prefix) | right |
| 11 | `[]` `.` `()` `++` `--` (postfix) | left |

Note: bitwise operators bind tighter than comparisons (unlike C). `x & mask == 0` works as expected.

### Chained Comparisons

```sysl
if 1 <= x <= 10 then ...    // equivalent to: 1 <= x && x <= 10
if a < b < c < d then ...   // all pairs checked, short-circuits
```

### Increment/Decrement

```sysl
++x       // prefix: increments x, returns new value
x++       // postfix: returns old value, then increments
--x       // prefix decrement
x--       // postfix decrement
```

### Compound Assignment

```sysl
x += 5    x -= 3    x *= 2    x /= 4    x %= 7
x &= 0xFF   x |= 0x01   x ^= 0xAA   x <<= 2   x >>= 1

// Also works on pointers (scaled by element size)
p += 2    p -= 1
```

### Casts

```sysl
int(true)         // bool -> int: 1
bool(42)          // int -> bool: true (nonzero)
byte(0x1FF)       // truncate to u8: 255
char(65)          // int -> u32: 65
*i8(address)      // int -> pointer
i64(ptr)          // pointer -> int
```

### sizeof

```sysl
sizeof(int)        // 4
sizeof(*int)       // 8
sizeof(Point)      // sum of fields + padding
sizeof([10]int)    // 40
```

### If Expression

```sysl
x = if cond then a else b
result = if x > 0 then x else -x
```

---

## Statements

### Control Flow

```sysl
// if/elif/else
if x > 0
    positive()
elif x == 0
    zero()
else
    negative()

// if-then (inline)
if x > 0 then positive()

// match (value matching, no fallthrough)
x match
    1 -> doA()
    2, 3 -> doB()              // multiple values per arm
    _ -> doDefault()           // wildcard (matches anything)
    else -> doDefault()        // alternative to wildcard

// match as expression
y = x match
    1 -> "one"
    2, 3 -> "few"
    else -> "many"

// match with guards
x match
    _ if x > 10 -> "big"
    _ if x > 0 -> "positive"
    else -> "non-positive"

// range matching (inclusive)
x match
    1..10 -> "small"
    11..100 -> "medium"
    else -> "large"

// struct destructuring in match
p match
    Point(x, y) -> x + y      // binds x and y from fields
    Point(_, y) -> y           // wildcard ignores field
    Point(x, y) if x == 0 -> y  // guard with bindings

// tagged union (data enum) matching
s match
    Circle(r) -> r * r * 3    // match variant, bind fields
    Rect(w, h) -> w * h       // each variant checked by tag
    Empty -> 0                 // no-data variant
    Circle(r) if r > 5 -> 1   // guard with variant binding

// match with block bodies
x match
    1 ->
        a = compute()
        doSomething(a)
    else -> fallback()

// while
while cond
    body

// while-do (inline)
while i < 10 do i++

// do-while
do
    body
while cond

// for (C-style)
for i = 0; i < 10; i++
    body

// for-do (inline)
for i = 0; i < 10; i++ do sum += i

// break and continue
while true
    if done then break
    if skip then continue
    process()
```

### Destructuring and Parallel Assignment

Tuples can be destructured with or without parentheses (Go/Python style):

```sysl
// Declaration (new variables)
q, r = divmod(17, 5)           // Go-style, creates q and r as var
(q, r) = divmod(17, 5)         // parenthesized form also works
val q, r = divmod(17, 5)       // immutable
var q, r = divmod(17, 5)       // explicit mutable

// Parallel assignment (existing variables)
a = 10
b = 20
a, b = b, a                    // swap: RHS fully evaluated before assignment

// Works on named structs too (not just tuples)
p = Point(10, 20)
x, y = p                      // x = p.x, y = p.y (field order)

// And ref structs
r = new Point(3, 4)
a, b = r                      // a = 3, b = 4

// Mixed declared/undeclared is an error
a = 10
a, b = 20, 30                  // ERROR: a exists but b doesn't
```

Rules for `a, b = ...` without `val`/`var`:
- All names new → declaration as `var`
- All names exist as `var` → parallel assignment
- Mixed → error

### Return

```sysl
return              // void return
return expr         // return single value
return a, b         // return tuple (no parens needed)
// or: last expression in block is implicit return
```

### Inline Assembly

```sysl
asm("halt")
asm("trap 0")
```

---

## Arrays, Slices, and Pointers

### Fixed Arrays

```sysl
var arr: [5]int           // zero-initialized
arr[0] = 42
arr: [3]int = [10, 20, 30]  // array literal

// Array decays to pointer when passed to *T parameter
sum(arr: *int, n: int) -> int = ...
sum(myArr, 5)             // myArr decays to *int
```

### Dynamic Arrays (Heap)

```sysl
a = new [5]int            // type: &[]int, ref-counted
a[0] = 42
len(a)                    // 5 (from heap header)
cap(a)                    // 5
// automatically freed when refcount reaches 0
```

### Slices (Sub-slicing)

```sysl
a = new [5]int
s = a[1:4]                // type: []int, shares backing array
s = a[:3]                 // s = a[0:3]
s = a[2:]                 // s = a[2:len]
s = a[:]                  // s = a[0:len]
len(s)                    // hi - lo
cap(s)                    // original_cap - lo
```

### Append

```sysl
s = a[:0]                 // empty slice with capacity
s = append(s, 42)         // returns new slice value
s = append(s, 99)         // Go semantics: may grow if len == cap
```

### Pointers

```sysl
x = 42
p = &x                    // p: *int
*p = 100                  // dereference and assign
val y = *p                // dereference and read

// Pointer arithmetic (scaled by element size)
p = &arr[0]
val second = *(p + 1)     // pointer + offset
p++                       // advance by one element
p += 3                    // advance by 3 elements
p--                       // retreat by one element
p -= 2                    // retreat by 2 elements

// Array + offset decays to pointer
q = arr + 2               // q: *int (not [n]int)
```

---

## Structs

```sysl
struct Point
    x: int
    y: int

// Value construction
var p: Point              // zero-initialized
p.x = 10
p.y = 20

// Constructor syntax
p = Point(10, 20)

// Heap-allocated (ref-counted)
r = new Point(10, 20)    // type: &Point
r.x = 30                 // access through ref

// Pointer to struct
ptr = &p                  // type: *Point
ptr.x = 50               // auto-deref: (*ptr).x = 50
```

### Struct Return and Tuples

```sysl
makePoint(x: int, y: int) -> Point = Point(x, y)

// Tuple return — parens optional in return and expression bodies
divmod(a: int, b: int) -> (int, int) = a / b, a % b
swap(a: int, b: int) -> (int, int)
    return b, a

// Destructure — parens optional
q, r = divmod(17, 5)
(q, r) = divmod(17, 5)         // also works
```

---

## Strings

Strings are fat pointers: `{ptr: *u8, len: i64}` with a ref-counted heap buffer.

```sysl
s = "hello"
len(s)                    // 5
s[0]                      // 104 ('h' as byte value)
t = s + " world"          // concatenation → new string
s == t                    // structural equality
s != t                    // structural inequality

// String decays to *u8 / *i8
puts(s: *byte)            // can pass string directly
```

### String Interpolation

Prefix a string with `s` to enable interpolation. Use `$name` for variables and `${expr}` for expressions:

```sysl
x = 42
s = s"value is $x"          // "value is 42"
puts(s"${x + 1}")           // prints "43"
name = "world"
puts(s"hello $name")        // prints "hello world"
puts(s"cost is $$5")        // prints "cost is $5" ($$ = literal $)
```

Plain strings (`"..."`) are never interpolated — `$` is just a regular character.

Non-string expressions are automatically converted via `str()`. Only integer and boolean types are currently supported for interpolation.

### `str()` Builtin

Converts a value to its string representation:

```sysl
str(42)                   // "42"
str(-5)                   // "-5"
str(0)                    // "0"
str("hello")              // "hello" (identity for strings)
```

### String Construction from Bytes

```sysl
// From pointer + length (copies the bytes)
var buf: [5]byte
buf[0] = 'h'
buf[1] = 'e'
buf[2] = 'l'
buf[3] = 'l'
buf[4] = 'o'
s = string(&buf[0], 5)   // s = "hello"

// From byte slice (copies the bytes)
data = new [10]byte
// ... fill data ...
s = string(data[:5])      // string from []byte slice
```

---

## Builtin Functions

| Function | Signature | Description |
|---|---|---|
| `putchar` | `(c: u32) -> u32` | Output single character |
| `print` | `(n: int)` | Print integer |
| `println` | `(n: int)` | Print integer with newline |
| `puts` | `(s: string)` | Print string |
| `len` | `(x) -> int` | Length of string, array, slice, or `&[]T` |
| `cap` | `(x) -> int` | Capacity of slice or `&[]T` |
| `append` | `(s: []T, elem: T) -> []T` | Append to slice (Go semantics) |
| `str` | `(x) -> string` | Convert int/bool to string representation |
| `string` | `(ptr: *T, len: int) -> string` | Construct string from pointer + length |
| `string` | `(s: []byte) -> string` | Construct string from byte slice |
| `malloc` | `(size: i64) -> *i8` | Allocate heap memory |
| `free` | `(ptr: *i8)` | Free heap memory |
| `calloc` | `(count: i64, size: i64) -> *i8` | Allocate zeroed memory |
| `realloc` | `(ptr: *i8, size: i64) -> *i8` | Resize allocation |
| `sbrk` | `(increment: i32) -> *i8` | Extend heap (POSIX) |
| `abort` | `()` | Terminate execution (trap 1, error code 3) |

User-defined functions shadow builtins of the same name.

---

## Type Compatibility and Coercion

### Implicit Widening

- Signed: `i8` -> `i16` -> `i32` -> `i64`
- Unsigned: `u8` -> `u16` -> `u32` -> `u64`
- Cross-sign: `u8` -> `i16` (unsigned fits in wider signed)
- Int to float: any integer -> `f64`

### Mixed Signed/Unsigned Rules

Operations between signed and unsigned types are allowed when the unsigned value fits entirely within the signed type's range:

```sysl
var b: byte = 200       // u8
var x: int = b + 1      // OK: u8 fits in i32
if b == 0 then ...       // OK: u8 compared with i32 literal

var big: u32 = 100
var y: int = big + 1     // ERROR: u32 doesn't fit in i32
```

### Array/Pointer Decay

- `[n]T` -> `*T` (array decays to pointer)
- `string` -> `*u8` or `*i8`
- `&T` -> `*U` (ref decays to raw pointer)
- Any `*T` -> any `*U` (permissive pointer casting)

### Explicit Casts Required

- `bool` <-> `int`: use `int(flag)` or `bool(n)`
- `int` <-> pointer: use `*i8(addr)` or `i64(ptr)`

---

## Runtime Safety

The codegen emits `trap 1` for runtime errors. On the OS, the trap handler terminates the faulting thread and outputs `!N` where N is the error code. On bare metal, execution halts.

| Error Code | Condition |
|---|---|
| 1 | Array/slice index out of bounds |
| 2 | Null pointer (malloc returned null) |
| 3 | `abort()` called |

---

## Conditional Compilation

```sysl
#if DEBUG
    var verbose = true
#else
    var verbose = false
#endif

#if !BARE_METAL
    import posix.stdlib.*
#endif
```

Conditions support: symbols, negation (`!`), equality (`==`), inequality (`!=`), numeric values.

---

## Calling Convention (TRISC ABI)

| Register | Purpose |
|---|---|
| r0 | Zero register (hardwired to 0) |
| r1 | First argument / return value |
| r2-r3 | Scratch (caller-saved) |
| r4 | Call address temp |
| r5 | Frame pointer |
| r6 | Link register (return address) |
| r7 | Stack pointer |

- At most one scalar argument in r1; additional arguments pushed right-to-left on the stack.
- Struct/string return: caller allocates return slot, passes hidden pointer as first arg in r1.
- String arguments: 16 bytes `{ptr, len}` pushed on stack.
- `mul Rd, Rs1, Rs2` writes high bits to `r((d+1) & 7)` — never use `mul r4`/`r5`/`r6` as destination.
