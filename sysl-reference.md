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

### Name Mangling

Functions and global variables in modules are mangled with the module path to avoid cross-module name collisions. The mangled name format is `modpart1_modpart2__funcname` (module parts joined by `_`, separated from the function name by `__`).

```
module std.strings → trim_space → std_strings__trim_space
module mylib       → helper     → mylib__helper
```

**Never mangled:** `main`, `extern` functions, builtin functions, functions in files without a `module` declaration, and any function whose name matches an `extern` declaration in the compilation.

Source code always uses the short name — the compiler resolves it to the mangled name automatically.

---

## Types

### Scalar Types

| Type | Alias | Size | Description |
|------|-------|------|-------------|
| `i8` | | 1 byte | signed 8-bit integer |
| `i16` | `short` | 2 bytes | signed 16-bit integer |
| `i32` | `int` | 4 bytes | signed 32-bit integer |
| `i64` | `long` | 8 bytes | signed 64-bit integer |
| `u8` | `byte` | 1 byte | unsigned 8-bit integer |
| `u16` | `ushort` | 2 bytes | unsigned 16-bit integer |
| `u32` | `char`, `uint` | 4 bytes | unsigned 32-bit integer (Unicode codepoint) |
| `u64` | `ulong` | 8 bytes | unsigned 64-bit integer |
| `f32` | `float` | 4 bytes | IEEE-754 single-precision floating point |
| `f64` | `double` | 8 bytes | IEEE-754 double-precision floating point |
| `bool` | | 1 byte | `true` or `false` |
| `unit` | | 0 bytes | no value |
| `string` | | 16 bytes | fat pointer: `{ptr: *u8, len: i64}` |

### Integer Overflow

All integer arithmetic wraps at the declared type width. There is no implicit integer promotion — `u8 + u8` produces `u8`, not `int`.

- **Unsigned types** wrap via modular arithmetic (zero-extension): `u8(255) + u8(1)` → `0`
- **Signed types** wrap via two's complement (sign-extension): `int(2147483647) + 1` → `-2147483648`
- **64-bit types** (`i64`, `u64`) use the full register width and do not truncate

To avoid wrapping, widen operands explicitly before arithmetic: `int(a) + int(b)`.

This matches Go, Rust, and Swift. C-style implicit integer promotion is not used.

### Overflow Intrinsics

When you want explicit, intent-marked overflow behavior, use the polymorphic
intrinsics. All take two operands of the same integer type and return the
same integer type:

| Intrinsic | Behavior |
|-----------|----------|
| `wrapping_add(a, b)` | Two's-complement wrap on overflow (current default for `+`) |
| `wrapping_sub(a, b)` | Two's-complement wrap on underflow |
| `wrapping_mul(a, b)` | Low bits of the true product |
| `saturating_add(a, b)` | Clamp to the type's MAX (or MIN for signed underflow) |
| `saturating_sub(a, b)` | Clamp to the type's MIN (0 for unsigned) |
| `saturating_mul(a, b)` | Clamp to the type's MAX/MIN on overflow |

```sysl
var a: u8 = 200
var b: u8 = 100
wrapping_add(a, b)     // 44   (300 & 0xFF)
saturating_add(a, b)   // 255  (clamped to u8 MAX)
saturating_sub(b, a)   // 0    (clamped to u8 MIN, would have been -100)
```

> **TRISC backend:** `saturating_*` on 64-bit types and `saturating_mul` on
> `u32` are not yet supported (would require explicit overflow detection or
> 128-bit intermediate). LLVM backend supports all widths.

### Composite Types

```sysl
*T              // raw pointer (8 bytes, unmanaged)
*T not null     // raw pointer constrained to be non-null at produce sites
&T              // ref-counted reference (8 bytes, auto-freed at rc=0)
[n]T            // fixed-size array (n * sizeof(T) bytes, stack-allocated)
[]T             // slice: {ptr: *T, len: i32, cap: i32} (16 bytes)
&[]T            // ref-counted heap array from new [n]T
(T1, T2, T3)   // tuple (desugars to anonymous struct)
(P1, P2) -> R  // function pointer / closure (16 bytes: {func_ptr, env_ptr})
```

**`not null` pointers.** `*T not null` is a subtype of `*T` with a runtime check: every
assignment, parameter bind, return, or cast that produces a `*T not null` value verifies the
pointer is non-null. A null value traps at the produce site. The check is inserted via the
same `where`-predicate mechanism used for user-defined predicates (a synthesized checker
function per inner type). `*T not null` is pointer-compatible with `*T`, so it can be passed
anywhere a `*T` is expected.

### Struct Types

```sysl
struct Point
    x: int
    y: int

struct Node
    value: int
    next: *Node       // recursive via pointer
```

**Struct invariants.** A struct may declare one or more `invariant <bool>` clauses
among its fields. Each invariant is checked after every field assignment or compound
assignment on a value of that struct type. Bare field names are in scope in the
invariant; so are module-level consts and globals. Non-bool invariants are rejected
at declaration time.

```sysl
struct Account
    balance: int
    limit: int
    invariant balance >= -limit

struct Range
    lo: int
    hi: int
    invariant lo <= hi
    invariant hi - lo <= 100      // multiple clauses: all must hold
```

A violating mutation traps via the standard contract-check path. The invariant is
re-evaluated at each check site — so an invariant that refers to an expression
with side effects re-runs those side effects. Checks fire on:

- var init with a struct-typed value: `var a: Account = Account(...)`
- whole-struct reassignment: `a = Account(...)`
- field assignment: `s.field = v` (including through a pointer/ref: `(*p).field = v`)
- field compound assignment: `s.field op= v`

### Enum Types (Simple)

Simple enums are integer constants with auto-incrementing values:

```sysl
enum Color
    Red               // 0
    Green             // 1
    Blue = 10         // explicit value
    Yellow            // 11 (auto-increment)
```

Access via `Color.Red`, `Color.Blue`, etc. At runtime, simple enum members are plain `i32` values. Simple enums can also be used as distinct types in type positions (e.g. `Result[int, ParseError]`); their bare variant names work as constructors:

```sysl
enum ParseError
    EmptyInput
    BadDigit
    Overflow

parse(s: string) -> Result[i64, ParseError]
    if len(s) == 0 then return Err(EmptyInput)   // bare variant name
    Ok(42)
```

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

**Exhaustiveness.** A `match` on a data-enum value must cover every variant, or include a
wildcard `_ -> ...` or `else -> ...` default. Missing variants produce a compile error
listing them. Guarded arms (`Circle(r) if r > 0 -> ...`) do not count toward exhaustiveness
since the guard may be false. (Non-enum matches, e.g. on integers or strings, do not require
exhaustiveness — the user is responsible for covering their own domain.)

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

**Heap-allocated enums (`new` on variants):**

`new VariantName(args)` heap-allocates an enum value and returns a ref-counted
`&EnumType`. This enables recursive data structures like AST trees:

```sysl
enum Expr
    Lit(value: int)
    Add(left: &Expr, right: &Expr)

eval_expr(e: &Expr) -> int
    *e match
        Lit(v) -> v
        Add(l, r) -> eval_expr(l) + eval_expr(r)

main() -> int
    val tree = new Add(new Lit(1), new Add(new Lit(2), new Lit(3)))
    eval_expr(tree)    // 6
```

`*e` dereferences the ref to a value enum for pattern matching. The ref is
automatically freed when the refcount reaches zero, just like `&Struct`.

**Recursive types:** Structs and enums may reference themselves (or each other)
through pointers (`*T`) or refs (`&T`):

```sysl
struct Node
    value: int
    next: *Node       // recursive via pointer

enum Tree
    Leaf(value: int)
    Branch(left: &Tree, right: &Tree)   // recursive via ref
```

**Memory layout:** `{tag: i32, padding, data: union of variant fields}`. The tag is a small integer (0, 1, 2...) identifying the variant. Data is overlapping storage sized to the largest variant. `sizeof(Shape)` returns the total size including tag and padding.

### Type Declarations

Two orthogonal modifiers compose, plus optional runtime checks. Forms:

```sysl
type Callback = (int) -> int                // plain alias (transparent)
type Age      = int within 0..150            // subtype: base-compatible, range-checked
type Meters   = new f64                      // derived: nominally distinct, no cast mixing
type SafeAge  = new int within 0..150        // derived + constrained
type Even     = int where value % 2 == 0     // arbitrary predicate on value
type PosEven  = int within 0..100 where value % 2 == 0   // within + where combined
```

| Form                              | Base-compatible? | Runtime check? |
|-----------------------------------|------------------|----------------|
| `type A = B`                      | yes              | no             |
| `type A = B within r`             | yes              | range          |
| `type A = B where p`              | yes              | predicate      |
| `type A = new B`                  | no               | no             |
| `type A = new B within r`         | no               | range          |
| `type A = new B where p`          | no               | predicate      |
| `type A = [new] B within r where p` | …              | both           |

**Range syntax.** Bounds must be numeric literals (including `char`, which is `u32`) or
references to a `const`; optional unary sign is allowed.

| Syntax     | Meaning                     | Example                                     |
|------------|-----------------------------|---------------------------------------------|
| `lo..hi`   | Inclusive: `[lo, hi]`        | `type Age = int within 0..150`              |
| `lo..<hi`  | Exclusive upper: `[lo, hi)`  | `type Prob = f64 within 0.0..<1.0`          |

**Where predicates.** `where <bool-expr>` attaches an arbitrary boolean predicate. Inside the
predicate, `value` binds to the value being checked. The predicate runs at every produce site
(assignment, parameter bind, return, explicit cast). A dedicated synthetic function
`__pred_<TypeName>(value) -> value` is emitted and called at each check site; unlike
`within` bounds, `where` predicates are not compile-time folded even for literals.

**Compatibility.** Subtypes (without `new`) are transparently compatible with their base; no
cast is needed, and runtime checks (range and/or predicate) fire on each assignment, parameter
bind, return, or explicit cast that produces a value of the constrained type. Derived types
(with `new`) are nominally distinct from both their base and other derived types over the same
base — mixing them with the base in arithmetic or assignment is a compile error; use an
explicit cast (`Meters(3.0)` to wrap, `f64(m)` to unwrap). Arithmetic between two values of
the same derived type yields that derived type. Out-of-range literal bounds are caught at
compile time; any runtime violation traps.

### Type Aliases

Plain aliases are the first form above — a transparent name for a type:

```sysl
type IntPtr = *int
type Callback = (int) -> int
```

### Type Attributes (`T::Attr`)

Range-constrained types and simple enums expose their metadata through `::`-suffixed
attributes. They work like Ada's `'Attr` notation, retargeted to sysl's `::` separator.

```sysl
type Age = int within 0..150
enum Day { Mon; Tue; Wed; Thu; Fri; Sat; Sun }

Age::First     // 0
Age::Last      // 150
Age::Range     // used only in `for i in Age::Range` — iterates 0..150 inclusive

Day::First     // Mon (value 0)
Day::Last      // Sun (value 6)
Day::Image(d)  // "Tue" for d = Day.Tue
Day::Pos(d)    // 1    for d = Day.Tue
Day::Val(2)    // Day.Wed
Day::Succ(d)    // Wed  for d = Day.Tue
Day::Pred(d)    // Mon  for d = Day.Tue
Age::Succ(a)    // a+1, traps if a is already 150
Age::Pred(a)    // a-1, traps if a is already 0
Day::Value("Tue")  // Day.Tue — parses a string back to its variant
Age::Valid(raw)    // bool — true iff `raw` is in range, never traps
```

| Attribute     | Applies to                              | Result                                              |
|---------------|-----------------------------------------|-----------------------------------------------------|
| `T::First`    | `within`-constrained int, simple enum   | lower bound / first variant's value                 |
| `T::Last`     | `within`-constrained int, simple enum   | upper bound (minus 1 if `..<`) / last variant       |
| `T::Range`    | same                                    | only valid in `for i in T::Range` — inclusive scan  |
| `T::Image(x)` | simple enum, constrained numeric type   | variant name string / `str(x)` for numerics         |
| `T::Value(s)` | simple enum                             | variant whose name equals `s`; traps on no match    |
| `T::Valid(x)` | `within`-constrained int, simple enum   | bool — does `x` satisfy the constraint? never traps |
| `T::Pos(x)`   | simple enum                             | 0-based declaration position                        |
| `T::Val(n)`   | simple enum                             | variant at position `n`; traps on out-of-range      |
| `T::Succ(x)`  | `within`-constrained int, simple enum   | next value; traps at the upper end                  |
| `T::Pred(x)`  | `within`-constrained int, simple enum   | previous value; traps at the lower end              |

`::First` and `::Last` fold to compile-time constants; `::Valid` on a `within`-int type
folds to an inline `x >= lo && x <= hi` (or `< hi` for `..<`). The rest lower to synthesized
helper functions (`__image_T`, `__value_T`, `__valid_T`, `__pos_T`, `__val_T`, `__succ_T`,
`__pred_T`) generated once per target type. `::Pos` / `::Value` on an unknown input,
`::Val` on an out-of-range position, `::Succ` past the upper bound, and `::Pred` past the
lower bound all trap via the standard contract-check path. `::Valid` is the non-throwing
complement — it returns a bool so the caller can branch. Typical guard-style use:

```sysl
if Age::Valid(raw) then
    var a: Age = raw          // safe: the range check will pass
```

`::Value` and `::Image` round-trip: `T::Value(T::Image(x)) == x` for every variant `x`.

`::Range` is syntactic sugar: `for i in T::Range body` parses as
`for i in T::First..T::Last body`. `for i in reverse T::Range` desugars the other way,
`for i in T::Last downTo T::First`. Using `::Range` outside a for-loop is a compile error.

Float-based `within` types do not yet support `::First` / `::Last`.

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
- `ptr -> value`: `*p` (dereference); implicit for struct function arguments

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

// Volatile — prevents load/store optimization (MMIO, shared memory)
volatile var status: u32 = 0
volatile var flag: int
```

### Volatile

The `volatile` qualifier prevents the compiler from optimizing away, reordering, or coalescing loads and stores. Use it for memory-mapped I/O registers and shared-memory variables.

Variables:
```sysl
volatile var mmio_status: u32 = 0
volatile var shared_flag: int
```

Struct fields:
```sysl
struct UartRegs
    volatile status: u32
    volatile data: u32
    baud: int            // non-volatile, normal optimization allowed
```

In the LLVM backend, `volatile` emits `load volatile` and `store volatile` instructions. The TRISC backend is unaffected (it does not optimize loads/stores).

### Discard Binding (`_`)

`_` is a write-only binding (Go/Rust style). You can bind to it; you cannot reference it; multiple `_` bindings in the same scope don't collide.

```sysl
val _ = foo()              // evaluate for side effects, discard result
val _ = bar()              // fine — no collision with the first _
val _: int = 7             // type annotations allowed
var _ = baz()              // var form also works
```

In destructuring patterns, `_` discards the corresponding field:

```sysl
_, y = pair()              // discard first, bind second
val x, _ = pair()          // discard second
val a, _, c = triple()     // discard middle element
_, _ = pair()              // discard all (evaluate for side effects)
```

### Global Variables

```sysl
var count = 0         // module-level mutable
val MAX = 100         // module-level immutable

main() -> int
    count += 1
    count
```

### Compile-Time Constants

Use `const` to declare a compile-time integer constant. The initializer must be evaluable at
compile time; the declaration emits no storage and references are replaced with the folded
literal value.

```sysl
const BASE = 0x1000
const STATUS = BASE + 4        // folded to 0x1004
const DATA = BASE + 8          // folded to 0x1008
const MASK = 0xFF & (1 << 4)   // folded to 0x10

type Age = int within 0..MAX_AGE   // const can be used in `within` bounds
```

Supported in initializers: `+`, `-`, `*`, `/`, `%`, `<<`, `>>`, `&`, `|`, `^`, unary `-`/`~`,
numeric/char/bool literals, and references to other `const` names. An initializer that
cannot be folded is a compile error. Values are truncated to the target type's width.

`const` is valid at both module and function scope. Currently only integer types are
supported — `const PI: f64 = 3.14` is not yet accepted.

Note: `val` is also folded when the initializer happens to be constant, but unlike `const`
it additionally allocates storage (and accepts non-const initializers). Prefer `const` when
you want the guarantee and zero-storage behaviour.

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

// Statement body — for/while/do-while loops can appear after `=`
// as a single-line void body
uart_puts(s: string) = for c in s do uart_putc(int(c))
wait_ready() = while !ready() do noop()
```

### Design by Contract — `require` / `ensure`

A block-body function can declare preconditions and postconditions at the top of its body:

```sysl
sqrt(x: f64) -> f64
    require x >= 0.0
    ensure result >= 0.0
    ensure result * result <= x + 1.0e-6
    var r = x / 2.0
    for _ in 0 downTo 20 step 1 do r = 0.5 * (r + x / r)
    r
```

- **`require <bool> [, "message"]`** — evaluated once on function entry. Traps if false.
- **`ensure <bool> [, "message"]`** — evaluated before every return site (including the
  implicit fall-through return of a trailing expression). Traps if false.
- Multiple `require` and `ensure` clauses are allowed, in any order. All clauses must appear
  before the first regular statement.
- Both run-time checks go through the standard trap path (same as range checks).

An optional string message can follow the condition, comma-separated (like Scala's
`require(cond, msg)`). The message appears in the runtime error for debugging:

```sysl
pos(x: int) -> int
    require x >= 0, "x must be non-negative"
    ensure result > 0, "pos() result must be positive"
    x + 1
```

On failure: `"precondition check failed: x must be non-negative"`. The message is emitted
by the LLVM backend and the interpreter; the TRISC and SVM backends currently trap with a
fixed error code.

**`result` in `ensure` clauses.** Inside an `ensure` expression, the identifier `result`
refers to the function's return value. Outside `ensure` — in `require` or in the body —
`result` is just a normal identifier and may be used for your own variables. The analyzer
aliases `result` → `__result__` only while typechecking ensure expressions (same pattern
used for `self` → `__self__` in methods).

**`old(expr)` in `ensure` clauses.** Captures the value of `expr` at function entry, before
any body statement runs. Essential for contracts about mutation:

```sysl
increment(p: *int)
    ensure *p == old(*p) + 1
    *p = *p + 1
```

`old()` may only appear inside `ensure` clauses; using it elsewhere is a normal undefined-
function error. Each `old(expr)` call allocates a hidden snapshot local that is initialized
at the top of the function body — so later mutations of the underlying variable or pointee
do not affect what `old()` sees. `old()` accepts any expression (pointer derefs, field
accesses, arithmetic, calls), but nested `old(old(...))` is rejected.

Contracts are not yet supported on expression-body functions or on closures.

**`assume <bool> [, "msg"]`.** Statement-level Ada/SPARK `pragma Assume` equivalent:
states a property the programmer asserts is true at this point. At runtime it is checked
exactly like `assert` and traps if false; statically it tells a future prover to take the
predicate as an axiom rather than a proof obligation. Allowed anywhere a statement is.

```sysl
ptr_size(p: *byte) -> int
    var len = strlen(p)
    assume len >= 0, "strlen returns non-negative"
    int(len)
```

The runtime trap message is `assume check failed[: msg]`. Stripped under `--no-contracts`.

**Quantifier expressions: `for all` / `for some`.** Ada-style universal and existential
quantifiers over an integer range. Bool-typed expressions; can appear anywhere a bool
expression is valid (require/ensure/invariant/assume, `if`/`while` conditions, &&/||
chains, etc.).

```sysl
for all i in 0..<n => a[i] > 0           // ∀ i ∈ [0, n) : a[i] > 0
for all i in 0..n  => a[i] > 0           // ∀ i ∈ [0, n] : a[i] > 0
for some k in 0..<n => a[k] == target    // ∃ k ∈ [0, n) : a[k] == target
```

- The bound variable is visible only inside the predicate; it shadows any outer-scope
  name and is restored afterward.
- The body extends greedily to the end of the surrounding expression:
  `for all x => P(x) && Q(x)` reads as `for all x => (P(x) && Q(x))`.
- Empty range: `for all` is **vacuously true**, `for some` is **false** (no witness).
- Both forms short-circuit (`for all` stops on first counterexample; `for some` stops
  on first witness).
- The range bounds must be integral; `..<` excludes the upper bound, `..` includes it.
- Quantifiers are most useful inside contracts:

```sysl
sorted(a: *int, n: int) -> bool
    require n >= 0
    for all i in 0..<n - 1 => a[i] <= a[i + 1]

binsearch(a: *int, n: int, target: int) -> int
    require for all i in 0..<n - 1 => a[i] <= a[i + 1]   // input is sorted
    ensure result == 0 - 1 || a[result] == target          // valid index or "not found"
    // ...
```

- `all` and `some` are **contextual** keywords — they have meaning only directly after
  `for` in expression position, so user identifiers named `all` / `some` continue to
  work elsewhere.
- Backend support: interpreter and LLVM are wired end-to-end. The TRISC backend traps at
  codegen time with a clear "not yet supported" message — use `--no-contracts` to strip
  quantifiers (along with the surrounding contract) when targeting TRISC.

**Function-level `variant <expr>` — recursion termination witness.** A SPARK-style
`Subprogram_Variant` clause: declares an integer expression that strictly decreases at
every direct recursive call. Used by a future verifier to discharge termination obligations
on recursive functions, and at runtime as a guard against unbounded recursion.

```sysl
fact(n: int) -> int
    variant n
    if n <= 1 then return 1
    return n * fact(n - 1)

gcd(a: int, b: int) -> int
    variant b
    if b == 0 then return a
    return gcd(b, a % b)
```

- The expression must be integral; it is cast to `i64` for the snapshot.
- Snapshotted at function entry into a hidden local `__variant_entry__`.
- At every **direct recursive call** (a TCall to the enclosing function), the variant is
  re-evaluated with the call's arguments substituted for the function's parameters; the
  result must be strictly less than the entry snapshot AND ≥ 0. Failure traps with
  `"<fn> variant decreased fail"`.
- Lives at the contract-clause position alongside `require` / `ensure` (must precede the
  first regular statement). At most one `variant` per function.
- **Mutual recursion** (a calls b, b calls a, both annotated): each function's variant
  catches only its own direct self-calls at runtime, so a mutual-recursion divergence
  *would* slip past the runtime check. A future verifier sees the obligation across calls.
- Stripped under `--no-contracts` — neither the snapshot nor the per-call check is emitted.

```sysl
// Variant alongside other contracts. Standard pattern.
fact(n: int) -> int
    require n >= 0
    variant n
    ensure result >= 1
    if n <= 1 then return 1
    return n * fact(n - 1)
```

Lowering: each recursive call `f(args)` is rewritten to a TIfExpr whose body binds the
args to fresh temps (so each is evaluated exactly once), computes the substituted variant,
asserts the decrease, and then performs the actual call. Because TIfExpr's last expression
is its value, the wrapper is transparent to the surrounding expression — `n * fact(n - 1)`
keeps its meaning.

### Default Parameter Values

Parameters can have default values, given with `= expr` after the type. Any
parameter with a default must come at the end of the parameter list; once a
parameter has a default, all later parameters must too.

```sysl
val BASE = 100

greet(x: int, y: int = 10) -> int = x + y
compute(x: int, k: int = BASE * 2) -> int = x + k

main() -> int
    greet(32)          // 42 — uses default y=10
    greet(32, 100)     // 132 — explicit y=100
    compute(42)        // 242 — k defaults to 200
```

Default expressions are evaluated at each call site (re-evaluated per call,
not cached). They can reference module-level vals and constants, but not
other parameters or local variables. Constant defaults are folded by the
analyzer.

Default values are not yet supported on generic functions.

### Named Arguments

Function call arguments can be passed by name using `name = expr`. Named
arguments can appear in any order, can be mixed with positional arguments
(positional must come first), and work with default values — including
skipping middle defaults:

```sysl
greet(x: int, y: int, z: int = 0) -> int = x * 100 + y * 10 + z

main() -> int
    greet(1, 2, 3)                  // positional
    greet(x = 1, y = 2, z = 3)      // all named
    greet(y = 2, x = 1, z = 3)      // named, any order
    greet(1, z = 3, y = 2)          // mixed: positional first, then named
    greet(1, z = 3)                 // named skips middle — y uses default if it had one
```

Errors:
- Positional argument after a named argument.
- Unknown parameter name.
- Duplicate named argument.
- Named argument that conflicts with a positional one (same slot).

Named arguments are currently supported for regular function calls,
struct constructors, and builtins — not yet for generic function
instantiation, method calls, or trait methods.

### Parameter Modes — `in` / `out` / `inout`

Each parameter can carry an Ada-style mode prefix that states **how** the argument
is passed:

- **`in x: T`** (default) — pass-by-value. The body sees a local copy; the caller's
  value is not affected by writes inside the function. This is the same as a plain
  `x: T` declaration.
- **`out x: T`** — caller passes an **lvalue** (a variable, field, or array element).
  The body writes into it through a hidden pointer; whatever value the body last
  assigned is visible at the call site after the call returns. The caller doesn't
  need to have initialized the lvalue beforehand.
- **`inout x: T`** — same as `out`, but the body also reads the initial value the
  caller supplied. Useful for accumulating / transforming a variable in place.

```sysl
// Out: initialize a caller-supplied variable.
set_to(out x: int, v: int)
    x = v

// Inout: read initial, write updated.
inc_by(inout x: int, by: int)
    x = x + by

main() -> int
    var v: int = 0
    set_to(v, 42)   // v is now 42
    inc_by(v, 5)    // v is now 47
    return v
```

Writes inside the body use plain assignment — the body always sees the parameter
as type `T`, not `*T`. The hidden pointer indirection is invisible:

```sysl
swap(inout a: int, inout b: int)
    val t: int = a
    a = b
    b = t

split(x: int, out q: int, out r: int)
    q = x / 10
    r = x % 10
```

At a call site, the compiler auto-takes the address of the argument — you do
not write `&v`:

```sysl
inc(inout n: int)
    n += 1

struct Point
    x: int
    y: int

main() -> int
    var p: Point = Point(10, 20)
    inc(p.x)              // field lvalue — OK
    var arr: [3]int = [0, 0, 0]
    inc(arr[1])           // index lvalue — OK
    // inc(p.x + 1)       // error: not an lvalue
    // inc(42)            // error: not an lvalue
    return p.x + p.y + arr[1]
```

Errors:
- Passing a literal, arithmetic expression, or call result to an `out`/`inout` param.
- A default value on an `out`/`inout` parameter (only `in` can have defaults).
- `out` or `inout` on the implicit method receiver `self`.
- `out` or `inout` parameters on a generic function (not yet supported in V1).

`out` and `inout` interact with `#pure`: writing to such a parameter is a write to
the caller's memory and will be rejected by the purity checker. Read-only
(`in`) parameters are fine in pure functions.

Internally the body auto-dereferences reads and writes: `x` in the body lowers to
`*ptr_x`, and `x = v` to `*ptr_x = v`. Both behaviors mean there is **no early-return
write-back**: every assignment commits immediately. Contextual-keyword rules: `in`
is already reserved; `out` and `inout` are contextual, so user identifiers with
those names still work outside parameter position.

### `def` — Auto-Call Functions

`def` declares a zero-argument function that is automatically called when
referenced by bare name. Unlike `val`, a `def` is re-evaluated on every
reference, and supports forward references (enabling mutual recursion).

```sysl
var counter = 0
def next_id = counter++      // return type inferred from body

def pi -> int = 314           // explicit return type

def greeting -> string        // block body
    "hello"

main() -> int
    val a = next_id           // auto-called: returns 0
    val b = next_id           // auto-called: returns 1
    a + b + pi                // 0 + 1 + 314 = 315
```

**Function pointer:** `&name` gives the function pointer for a `def`:

```sysl
apply_thunk(f: () -> int) -> int = f()

main() -> int
    counter = 0
    apply_thunk(&next_id)     // passes next_id as a function pointer
```

**On parametric functions:** `def` is also accepted before functions with
parameters, where it is purely documentary (no behavior change):

```sysl
def add(a: int, b: int) -> int = a + b   // same as: add(a: int, b: int) -> int = a + b
```

### Generic Functions

Functions may declare type parameters in square brackets after the name, with
optional trait bounds using `:` and `+`. The
compiler monomorphizes each instantiation — one specialized copy per unique set
of type arguments, just like Go or C++. Type arguments are inferred from the
call-site argument types.

```sysl
// Identity — works for any type
id[T](x: T) -> T = x

// Swap via pointers — works for any T
swap[T](a: *T, b: *T)
    var tmp: T = *a
    *a = *b
    *b = tmp

// Multiple type parameters
pair_first[K, V](k: K, v: V) -> K = k

// Instantiation-time checking: operations on T are checked when T is pinned.
// max[int] works; max[bool] errors at the call site because > is not defined.
max[T](a: T, b: T) -> T
    if a > b then a else b

main() -> int
    var x = 10
    var y = 20
    swap(&x, &y)        // T inferred as int
    max(1.5, 2.5)       // T inferred as f64
    id(42)              // T inferred as int
```

**Trait bounds.** A type parameter may be constrained to types that implement
one or more traits:

```sysl
maxOf[T: Ord](a: T, b: T) -> T       // T must implement Ord
bothCheck[T: Ord + Eq](a: T, b: T)   // T must implement Ord AND Eq
```

Bounds are checked at each call site when the concrete type arguments are known.
An unsatisfied bound produces a clear error naming the missing trait and the
type parameter. Inside the generic body, operators like `a > b` and `a == b`
route through the bounded trait's methods.

**Rules:**
- Type parameters may appear in parameter types, return type, and local variable
  type annotations.
- Type arguments are **inferred** from argument types (explicit type arguments
  come in a later phase).
- Each unique `(function, type-args)` combination produces one specialized copy
  (cached; name-mangled to e.g. `swap_i32`).
- Operations on a type parameter that are invalid for the concrete type produce
  an error at the call site where the instantiation happens.

### Generic Structs

Structs may declare type parameters in square brackets after the name. Each
distinct instantiation gets its own monomorphized struct layout and `sizeof`.

```sysl
struct Pair[T]
    a: T
    b: T

struct Tuple[K, V]
    key: K
    value: V

main() -> int
    p = Pair(10, 20)            // T inferred as int from arg types
    q: Pair[i64] = Pair(1i64, 2i64)
    t = Tuple(5, 'A')
    p.a + p.b + int(q.a) + t.key
```

**Rules:**
- Type parameters appear in square brackets after the struct name.
- Field types may reference the type parameters.
- Constructor calls infer type arguments from the argument types.
- Explicit type annotations (`Pair[int]`) may also be used in variable
  declarations and parameter types.
- Each `(struct, type-args)` pair produces one monomorphized struct type with a
  mangled name (e.g. `Pair_i32`, `Tuple_i32_u32`).
- Generic functions and generic structs compose: a function like
  `swapPair[T](p: *Pair[T])` is fully supported — `T` is inferred from the
  concrete `Pair[i32]` passed in.

### Generic Tagged Unions

Tagged unions (data enums) may declare type parameters — the foundation for
`Option[T]`, `Result[T, E]`, and similar sum types.

```sysl
enum Option[T]
    Some(value: T)
    None

enum Result[T, E]
    Ok(value: T)
    Err(error: E)

safeDiv(a: int, b: int) -> Option[int]
    if b == 0 then None
    else Some(a / b)

main() -> int
    r = safeDiv(20, 4)
    r match
        Some(v) -> v
        None -> -1
```

**Rules:**
- Type parameters in square brackets after the enum name.
- Variant field types may reference the type parameters.
- Each `(enum, type-args)` pair produces one monomorphized `EnumType` with a
  mangled name (e.g. `Option_i32`, `Result_i32_string`).
- Pattern matching uses the scrutinee's concrete enum type to look up variants.

**Type inference:** variant constructors prefer to infer type args from
argument types (`Some(42)` infers `T=int`). When a variant doesn't pin all
type parameters — e.g. `Ok(42)` for `Result[T, E]` leaves `E` unknown — the
analyzer consults the **expected type** from context:

| Context | Expected type source |
|---|---|
| `var x: Option[int] = None` | the declared variable type |
| `fn f() -> Result[int, string] { Ok(42) }` | the function's return type |

Without an expected type and incomplete argument-based inference, the compiler
errors with a message asking for an explicit type annotation.

### `?` Operator (Try)

The postfix `?` operator on an enum value unwraps the success variant or
early-returns the failure variant from the enclosing function. It's the
standard ergonomic for working with `Option[T]` and `Result[T, E]`.

```sysl
enum Option[T]
    Some(value: T)
    None

parseAndDouble(s: string, start: int) -> Option[int]
    x = parseInt(s, start)?         // unwrap Some(x), or early-return None
    Some(x * 2)
```

**Rules:**
- Applies only to monomorphized generic enum values where the enum has exactly
  two variants and the first variant has exactly one field (the success type).
- The enclosing function's return type must be the **same** enum type as the
  value being `?`-unwrapped (no error-type conversion yet).
- `expr?` desugars at analyze time to:
  ```
  match expr
      Success(v) -> v
      Failure(...) -> return Failure(...)
  ```
  where `Success` is variant 0 and `Failure` is variant 1.
- The result type of the whole `expr?` is the success variant's field type.

**Chainable:** `a?.field` works if `a?` returns a struct; multiple `?`s across
separate statements also work (e.g. `x = a?` followed by `y = b?`).

### Traits and `impl` blocks

Traits describe a set of methods a type may implement. Each trait is parameterized
by a subject type `T` (the type that will conform). Methods may have default
bodies; implementers override or inherit them. No orphan rule — any `impl` may
be written anywhere.

```sysl
trait Ord[T]
    cmp(a: T, b: T) -> int                  // required (no body)
    lt(a: T, b: T) -> bool = cmp(a, b) < 0  // default body
    le(a: T, b: T) -> bool = cmp(a, b) <= 0
    gt(a: T, b: T) -> bool = cmp(a, b) > 0
    ge(a: T, b: T) -> bool = cmp(a, b) >= 0

impl Ord[int]
    cmp(a: int, b: int) -> int = a - b

main() -> int
    if Ord.lt(3, 5) then 1 else 0
```

**Rules:**
- A trait method with a body is a **default**; implementers may override it.
- A trait method without a body is **required**; every impl must provide it.
- `impl Trait[T]` for the same `(trait, type)` pair may appear only once.
- Calls via `Trait.method(args)` infer the concrete target type from argument
  types and dispatch to the matching impl's method.
- Inside a default body, unqualified calls to sibling trait methods (like
  `cmp(a, b)` inside `lt`) resolve to the current impl's methods.

**Monomorphization:** each impl method — whether provided or synthesized from a
default — compiles to a mangled top-level function like `Ord_cmp_i32`,
`Ord_lt_i32`. There is no runtime dispatch; trait calls are resolved statically.

### Operator Overloading via Traits

Operators on user-defined struct and enum types desugar to trait method calls.
The compiler maps each operator to a fixed `(trait, method)` pair and dispatches
through the impl registered for the operand type.

| Operator | Trait | Method | Signature |
|---|---|---|---|
| `<` `<=` `>` `>=` | `Ord` | `lt` `le` `gt` `ge` | `(T, T) -> bool` |
| `==` `!=` | `Eq` | `eq` `ne` | `(T, T) -> bool` |
| `+` | `Add` | `add` | `(T, T) -> T` |
| `-` | `Sub` | `sub` | `(T, T) -> T` |
| `*` | `Mul` | `mul` | `(T, T) -> T` |
| `/` | `Div` | `div` | `(T, T) -> T` |

```sysl
struct Vec2
    x: int
    y: int

trait Add[T]
    add(a: T, b: T) -> T

impl Add[Vec2]
    add(a: Vec2, b: Vec2) -> Vec2 = Vec2(a.x + b.x, a.y + b.y)

main() -> int
    a = Vec2(1, 2)
    b = Vec2(10, 20)
    c = a + b                  // desugars to Add.add(a, b) → Add_add_Vec2(a, b)
    c.x * 100 + c.y
```

Built-in numeric operators are unaffected — `3 + 4` on `int` still uses the
native instruction. Dispatch through a trait only applies when the left operand
is a struct or enum type.

Operator sugar composes with generic functions. Inside `max[T](a: T, b: T)`,
writing `a > b` works for any `T` that has an `Ord` impl, checked at
instantiation time.

### Methods

Methods are declared with the `StructName.methodName(...)` syntax. The parser
automatically prepends a hidden `__self__: *StructName` parameter, so you do
**not** write `self` in the parameter list — just refer to `self` inside the
method body:

```sysl
struct Point
    x: int
    y: int

Point.magnitude() -> int
    self.x * self.x + self.y * self.y

main() -> int
    var p: Point
    p.x = 3
    p.y = 4
    p.magnitude()     // desugars to Point_magnitude(&p)
```

Inside the method body, `self` is an alias for the implicit receiver — it
has type `*StructName` (raw pointer to the instance).

#### Methods on generic structs

Generic structs can have methods too. Include the type parameters
after the struct name:

```sysl
struct MinHeap[T]
    data: []T
    less: (T, T) -> bool

MinHeap[T].len() -> int = len(self.data)

MinHeap[T].push(v: T)
    self.data = append(self.data, v)
    self._sift_up(len(self.data) - 1)
```

The parser desugars `MinHeap[T].push(v: T)` into a generic function
`MinHeap_push[T](__self__: *MinHeap[T], v: T)`. When you call `h.push(42)`
on a `MinHeap[int]`, the compiler instantiates `MinHeap_i32_push` with
`T = int` — the same monomorphization used for any generic function.

Type parameter bounds work on generic methods just as on generic functions:

```sysl
MinHeap[T: Ord].sorted_push(v: T)
    // T must implement the Ord trait
```

### Deinit Blocks

```sysl
struct Buffer
    data: *byte
    size: int

Buffer.deinit()
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
    f: (int) -> int = dbl
    f(21)                         // indirect call → 42

    var funcs: [2](int) -> int
    funcs[0] = dbl
    funcs[1] = triple
    funcs[0](10) + funcs[1](10)  // call through array
```

### Closures

Closures are anonymous functions that can capture variables from their enclosing scope. They use the `->` arrow syntax:

```sysl
// Single parameter (no parens needed)
f = x -> x + 1

// Multiple parameters
g = (x, y) -> x + y

// Zero parameters
h = () -> 42

// With type annotations
f = (x: int) -> x * 2

// Multi-line body (indentation block)
transform = x ->
    val doubled = x * 2
    doubled + 1
```

**Capture semantics:** Closures capture variables **by value** (copy at creation time). Mutations to the original variable after the closure is created do not affect the captured value:

```sysl
var a = 10
f = x -> x + a      // captures a = 10
a = 100
f(32)                // 42 (uses captured a = 10, not 100)
```

To share mutable state, capture a pointer (`*T`) or ref (`&T`).

**Type inference:** Closure parameter types are inferred from context when the closure is passed to a function expecting a specific `(...) -> T` type:

```sysl
apply(f: (int) -> int, x: int) -> int = f(x)

main() -> int = apply(x -> x + 1, 41)    // x inferred as int
```

**Higher-order patterns:**

```sysl
// Closure as argument
apply(f: (int) -> int, x: int) -> int = f(x)
apply(x -> x * 2, 21)           // 42

// Closure as return value (requires captures)
make_adder(n: int) -> (int) -> int
    val captured = n
    x -> x + captured

add10 = make_adder(10)
add10(32)                        // 42

// Closure assigned to variable
val f: (int) -> int = x -> x * 2
f(21)                            // 42
```

**Escaping closures:** Function parameters are **non-escaping by default** — the closure's captured environment is stack-allocated. Use `@escaping` to mark parameters where the callee may store the closure beyond the call's lifetime:

```sysl
// Non-escaping (default): env lives on caller's stack frame
sort_by(arr: []int, cmp: (int, int) -> bool)

// Escaping: env is heap-allocated via malloc
on_click(handler: @escaping () -> unit)
```

Non-escaping closures are more efficient (no heap allocation) but the compiler trusts the annotation — storing a non-escaping closure into a global, struct field, or returning it is undefined behavior. Closures with no expected type context (e.g., `val f = x -> x + 1`) default to escaping.

**Implementation:** All function values (including plain function pointers) are 16-byte fat pointers: `{func_ptr: i64, env_ptr: i64}`. Plain function pointers have `env_ptr = 0`. The environment allocation strategy depends on capture types:

- **Non-escaping, captures all non-rc-bearing** (ints, raw pointers, etc.): the environment is allocated on the caller's stack frame — no malloc, no free. This is what makes closures usable in no-allocator (kernel/bare-metal) contexts.
- **Escaping, OR any rc-bearing capture** (string, ref, struct-with-string, enum-with-string, …): the environment is heap-allocated with a `[rc:i64 @ -16 | deinit_ptr:i8* @ -8 | data]` header. Closure descriptor scope-exit decrements the env's refcount; at zero, a per-closure-id deinit walks the captures (decr'ing rc-bearing entries) and `free` reclaims the env block.

The `env_ptr` is passed to the closure function via register r3 in the TRISC calling convention (LLVM passes it as the first hidden parameter `i8* %env`).

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
1.5e10                // scientific notation
'A'                   // char literal (u32, value 65)
'\n'                  // escape char
"hello"               // string literal
true, false           // bool
[1, 2, 3]            // array literal
1_000_000             // underscore separators (decimal, hex, float, exponent)
0xDEAD_BEEF           // grouping for readability
0xFF_00_FF_00u32      // combined with type suffix
3.141_592             // underscores in fractional part
```

**Type suffixes** on integer literals force a specific type:

```sysl
42i8                  // i8
42i16                 // i16
42i32                 // i32 (same as plain `42`)
42i64                 // i64
200u8                 // u8
1000u16               // u16
100u32                // u32
0xFFu64               // u64
```

Float literals (`3.14`, `1e5`) default to `f64`, but coerce to `f32` when the
context demands it (`var x: f32 = 1.5` works without a cast). Mixed-width float
arithmetic widens to the wider operand; `f64 -> f32` requires explicit `f32(x)`.

> **Backend note:** TRISC stores `f32` as 4 bytes in memory but works with it as
> `f64` in registers (using `f32tof64`/`f64tof32` at memory boundaries). LLVM
> uses native `float` throughout. Both backends are correct; TRISC's approach
> trades 4 bytes per `f32` register slot for simpler arithmetic codegen.

**Escape sequences** in string and char literals:

```
\n    newline
\t    tab
\r    carriage return
\0    null (0x00)
\\    literal backslash
\'    literal single quote
\"    literal double quote
\xNN  hex byte (e.g., \x1b for ESC, \x00 for null)
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
// Numeric
int(true)         // bool -> int: 1
bool(42)          // int -> bool: true (nonzero)
byte(0x1FF)       // truncate to u8: 255
char(65)          // int -> u32: 65
i64(3.14)         // float -> int: 3
f32(3.14)         // f64 -> f32 (precision narrowing)
f64(x: f32)       // f32 -> f64 (lossless widening, also implicit)

// Pointer / int conversions
*i8(address)      // int -> pointer
*Point(address)   // int -> struct pointer
i64(ptr)          // pointer -> int
bool(ptr)         // pointer -> bool (null = false)

// Pointer-to-pointer
*byte(charPtr)    // *T -> *U (any pointer to any pointer)
*i8(refVal)       // &T -> *i8 (ref to raw pointer)

// Function pointers
i64(funcPtr)      // func -> int (address)
bool(funcPtr)     // func -> bool (non-null = true)

// Array decay (address of first element)
*byte(arr)        // [N]T -> *byte
*i64(arr)         // [N]T -> *i64
i64(arr)          // [N]T -> i64 (address as integer)
string(arr, len)  // [N]byte + len -> string
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

### If-Is (Pattern Matching in If)

`if expr is Pattern then ...` is sugar for a single-arm `match`. The pattern
binds variables in the then-branch. Like Rust's `if let`.

```sysl
// Extract value or use default
val v = if r is Ok(x) then x else -1

// Guard with pattern
if o is Some(x) then
    process(x)

// With block body and else
if parse(s) is Ok(val, pos) then
    handle(val, pos)
else
    report_error()
```

Desugars to `match` at parse time — no new analyzer or runtime machinery.

---

## Statements

Statements are separated by newlines. Semicolons (`;`) can also be used to place multiple statements on a single line:

```sysl
a[0] = 1; a[1] = 2; a[2] = 3
val x = 10; val y = 20
```

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

// loop — Ada-style infinite loop; exit only via break (or return).
loop
    if done then break
    body

// for (C-style)
for i = 0; i < 10; i++
    body

// for-do (inline)
for i = 0; i < 10; i++ do sum += i

// for-in range (inclusive — includes upper bound)
for i in 1..5
    body                       // i takes 1, 2, 3, 4, 5

// for-in range (exclusive — excludes upper bound)
for i in 0..<5
    body                       // i takes 0, 1, 2, 3, 4

// for-in with do inline
for i in 0..<n do print(i)

// for-in counting down (inclusive of both bounds)
for i in 10 downTo 0
    body                       // i takes 10, 9, ..., 0

// for-in with step
for i in 0..100 step 5          // 0, 5, 10, ..., 100
for i in 0..<30 step 3          // 0, 3, 6, ..., 27
for i in 20 downTo 0 step 4     // 20, 16, 12, 8, 4, 0

// Iterate values over arrays/slices/strings
for v in arr
    body                       // v = each element

// Iterate backward — over a T::Range or over a collection
for i in reverse Day::Range    // last variant down to first
for v in reverse arr           // index len-1 down to 0

// Iterate with index and value
for i, v in arr
    body                       // i = index, v = arr[i]

// `in` as range membership operator
x in 1..4                       // true if 1 <= x <= 4 (inclusive)
x in 1..<4                      // true if 1 <= x < 4  (exclusive)
x !in 1..4                      // negated membership
if score in 90..100 then grade = 'A'

// break and continue
while true
    if done then break
    if skip then continue
    process()

// `variant <expr>` — loop termination witness. The expression must strictly decrease
// between iterations and stay >= 0. On the first iteration nothing is checked (there's
// no prior value); on every subsequent one the analyzer-emitted check traps if either
// condition is violated. Must appear at the top level of a loop body.
var remaining = 100
while remaining > 0
    variant remaining          // monotonic-decrease witness
    remaining = remaining - step()

// `invariant <bool> [, "msg"]` — Ada/SPARK-style loop invariant. Must appear in the
// leading "header" of a loop body (variants may interleave); the analyzer hoists the
// check to the loop's *cut point* and runs it at the top of every iteration, regardless
// of how the source was laid out. Multiple invariants are allowed; all are checked. A
// false invariant traps with `loop invariant check failed[: msg]`. An invariant placed
// after a non-invariant statement, nested inside an `if`/`match`, or outside any loop
// is a static error.
for i = 0; i < n; i++
    invariant i >= 0
    invariant i <= n, "i in range"   // optional message like require/ensure
    body()

// Invariants and variants can be freely interleaved in the leading header.
while remaining > 0
    invariant total >= 0
    variant remaining
    process()
    remaining = remaining - 1

// `loop_entry(expr)` — Ada/SPARK-style loop-entry snapshot, valid only inside a loop
// invariant. Captures the value of `expr` once, at the moment control first reaches the
// loop (after for-loop init, before the first cond check), so subsequent invariant
// evaluations can compare against the entry value. Each enclosing loop has its own
// snapshot scope, so nested `loop_entry(...)` always refers to the innermost loop.
// Snapshot exprs must be visible in the surrounding scope (or, for `for`, after init).
var x = 0
for i = 0; i < n; i++
    invariant x >= loop_entry(x)            // monotonic non-decrease
    invariant loop_entry(i) == 0            // i started at 0
    x = x + i

// `loop_entry(expr)` outside a loop invariant — including in an `ensure` clause or in
// loop-body code — is a static error. Use `old(expr)` for the function-entry snapshot.

// Labeled loops — break / continue can target an outer loop by name.
// A label is an identifier followed by `:` immediately before `for`, `while`, `do`, or `loop`.
outer: for i in 0..<n
    for j in 0..<m
        if grid[i][j] == target then break outer      // exits both loops
        if grid[i][j] == 0 then continue outer        // next iteration of outer
        use(grid[i][j])

// Unlabeled `break` / `continue` always target the innermost enclosing loop,
// regardless of whether that loop has a label.
// A label cannot be reused on a nested loop (would make `break label` ambiguous),
// but the same name can appear on sibling (non-nested) loops.

// Optional `end <kw>` terminators (Scala 3 style) — every block construct accepts
// an optional matching `end <keyword>` after its body. Useful for long blocks
// where the matching indentation is hard to see; always optional.
if cond
    big_body()
end if

while running
    tick()
end while

for i = 0; i < n; i++
    process(i)
end for

loop
    if done then break
    work()
end loop

x match
    1 -> "one"
    else -> "other"
end match

struct Point
    x: int
    y: int
end struct

enum Color
    Red
    Green
    Blue
end enum

trait Eq[T]
    eq(self: T, other: T) -> bool
end trait

impl Eq[int]
    eq(self: int, other: int) -> bool = self == other
end impl

interface Closer
    close() -> int
end interface
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
return              // unit return
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

// Byte arrays from string and char literals
var buf: [5]byte = "hello"          // copies string bytes into array
var msg: [3]byte = ['H', 'i', '!'] // char literals coerce to bytes

// Array decays to pointer when passed to *T parameter
sum(arr: *int, n: int) -> int = ...
sum(myArr, 5)             // myArr decays to *int
```

### Chained Indexing

Indexing is a repeatable postfix operator — `arr[i][j]` works on arrays of arrays, slices of slices, etc:

```sysl
val grid = new [3][]int     // array of int slices
grid[0] = row0[:]
val v = grid[1][2]          // chain: grid[1] returns []int, then [2] indexes it

// Address-of with chained index
val p = &stacks[slot][0]    // address of first element of stacks[slot]
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

// Expression lvalues (C-style)
(*p).field = 10           // deref pointer, assign field
(*p)[i] = 42              // deref pointer, index, assign
(arr + 2)[0] = 99         // pointer arithmetic, index, assign

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

Non-string expressions are automatically converted via `str()`. Integer, boolean, and float (`f32`/`f64`) types are supported.

### Format Strings (f-strings)

Prefix a string with `f` for printf-style format specifiers. Each interpolation can be followed by `%` and a format spec:

```sysl
val n = 255
puts(f"hex: $n%x")               // "hex: ff"
puts(f"HEX: $n%X")               // "HEX: FF"
puts(f"padded: $n%08x")          // "padded: 000000ff"
puts(f"decimal: $n%d")           // "decimal: 255"
puts(f"binary: ${26}%b")         // "binary: 11010" (expression needs braces)
puts(f"octal: ${511}%o")         // "octal: 777"
```

Format specifiers:

| Spec | Meaning |
|------|---------|
| `%d` | Decimal integer |
| `%x` | Hexadecimal (lowercase) |
| `%X` | Hexadecimal (uppercase) |
| `%b` | Binary |
| `%o` | Octal |
| `%s` | String (default if no spec given) |
| `%+d` | Decimal with explicit sign |
| `%%` | Literal `%` |

Width and padding:

```sysl
val n = 42
puts(f"$n%08d")                   // "00000042" (zero-padded, width 8)
puts(f"0x${10}%04x")             // "0x000a"   (literal needs braces)
val s = "hi"
puts(f"[$s%10s]")                 // "[        hi]"  (right-aligned, width 10)
puts(f"[$s%-10s]")                // "[hi        ]"  (left-aligned, width 10)
```

Mixed example:

```sysl
val cp = 65
val count = 3
val name = "LATIN"
puts(f"U+$cp%04X count=$count%d name=$name%s")
// "U+0041 count=3 name=LATIN"
```

Without a format spec, `f"..."` works like `s"..."` — values are converted via `str()`.

### `str()` Builtin

Converts a value to its string representation:

```sysl
str(42)                   // "42"
str(-5)                   // "-5"
str(0)                    // "0"
str("hello")              // "hello" (identity for strings)
str(3.14)                 // "3.140000" (codegen: fixed 6-digit fractional)
str(Circle(5))            // "Circle" (variant name of a data-enum value)
```

Float formatting uses fixed 6-digit fractional precision in TRISC codegen
(`3.14 -> "3.140000"`). The interpreter uses the host's default float
formatting (`3.14 -> "3.14"`).

`str()` on a data-enum (tagged union) value returns the variant name as a string, regardless
of the variant's field contents. Each enum type gets one synthesized `__str_<EnumName>`
helper the first time it's referenced. Simple integer enums and struct values are not yet
supported — use field formatting manually.

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
| `panic` | `(msg: string) -> unit` | Halt with message (trap 1, error code 4) |
| `assert` | `(cond: bool, msg: string) -> unit` | Panic with `msg` if `cond` is false |
| `expect` | `(actual: i64, expected: i64, msg: string) -> unit` | Panic with `"msg: expected N, got M"` if values differ |
| `abort` | `()` | Terminate execution (trap 1, error code 3) |

User-defined functions shadow builtins of the same name.

---

## Type Compatibility and Coercion

### Implicit Widening

- Signed: `i8` -> `i16` -> `i32` -> `i64`
- Unsigned: `u8` -> `u16` -> `u32` -> `u64`
- Cross-sign: `u8` -> `i16` (unsigned fits in wider signed)
- Float: `f32` -> `f64`
- Int to float: any integer -> `f32` or `f64`

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

### Pointer Dereference Is Explicit

Passing `*T` to a function parameter of type `T` is a **type error**. Implicit
deref-and-copy was removed because it hides cost: a pointer-passing site that
*looks* like pass-by-reference silently becomes a `memcpy` of the entire pointee.
For a small struct that's free; for a 4 KB packet it isn't. Write the deref:

```sysl
struct Point
    x: int
    y: int

sum(p: Point) -> int = p.x + p.y

main() -> int
    var p = Point(20, 22)
    val ptr: *Point = &p
    sum(*ptr)              // explicit: sum receives a copy of *ptr
```

Equivalently, take the pointer's pointee directly: `sum(p)` (no `&`/`*` at all).

The reverse direction (`T` -> `*T`) is also not implicit — it would create
a dangling pointer to a temporary.

### Exception: `self` In Methods

Inside a method body `self` has type `*StructName`. Passing `self` to a function
that expects the value type auto-derefs, because the method-call sugar already
hides the pointer:

```sysl
Point.total() -> int = sum(self)   // self is *Point; sum gets a copy of *self
```

This is the **only** implicit `*T -> T` allowed. Local variables of pointer
type, function parameters, struct fields — all require explicit `*ptr`.

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
| 4 | `panic()` or `assert()` failure |

### Disabling Contracts

Pass `--no-contracts` to `sysl compile` or `sysl run` to strip every contract check at compile time:

- `require` / `ensure` clauses
- `invariant` statements in loops
- `variant` statements in loops (entire hoisted check state is elided)
- `assume` statements
- struct `invariant` clauses (no per-assignment check)
- `where`-predicate bodies (synthesized predicate function still runs but performs no check)
- `within`-range checks (compile-time literal check + runtime range check both skipped)
- enum `::Pos` / `::Val` / `::Value` / `::Succ` / `::Pred` and within `::Succ` / `::Pred` traps (helper still returns a value, but invalid input yields garbage: `-1` for enum helpers, `v+1` / `v-1` past the bound for within helpers)

This is Ada's `pragma Assertion_Policy(Disable)` equivalent — the user takes responsibility for correctness in exchange for no runtime overhead. Contract clauses still **type-check** at compile time regardless of the flag; only the runtime traps are elided.

`::Valid(x)` is *not* a contract — it is non-throwing introspection — and is never stripped.

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

#if TARGET == "trisc"
    extern halt()
#endif

#if VERSION != "1.0"
    import new_api.*
#endif
```

**Condition forms:**
- `#if SYMBOL` — true if the symbol is defined and not `"false"`, `"0"`, or `""`
- `#if !SYMBOL` — negation
- `#if SYMBOL == "value"` — string equality
- `#if SYMBOL != "value"` — string inequality

---

## Attributes

Attributes are annotations prefixed with `#` that attach to the following declaration. They appear on their own line(s) immediately before the declaration:

```
#test
test_copy_basic() -> unit
    0

#inline
#deprecated("use foo2")
foo() -> int = 1
```

**Forms:**
- Flag: `#name`
- With arguments: `#name(arg1, arg2, ...)` — arguments are literals (string, int, bool), bare identifiers, or `key: value` pairs

Multiple attributes stack on separate preceding lines. Unknown attribute names are stored as-is (no error), so new attributes can be introduced incrementally.

`#if` / `#else` / `#endif` (conditional compilation) use `#` but are not attributes — they work the same as before.

### `#test` — unit tests

Functions marked `#test` are unit tests. Requirements:
- zero parameters,
- returns `unit` (or no return type),
- not generic,
- not a method.

A test **passes** iff it does not panic. A panic (`panic("msg")`, `abort()`, or any runtime trap) fails the test.

```
#test
test_trivial() -> unit
    assert(1 + 1 == 2, "math is broken")

#test("descriptive name shown in output")
test_with_display_name() -> unit
    0
```

**`should_panic`** — the test is expected to panic:

```
#test(should_panic)
test_guard() -> unit
    panic("this must fire")

#test(should_panic: "out of range")
test_bounds() -> unit
    // substring match: panic message must contain "out of range"
    panic("index 42 is out of range")
```

`#test` functions are **excluded from non-test builds** — `sysl compile` and `sysl run` strip them, so they don't contaminate normal execution and aren't emitted to `.asm` / `.tof` / `.ll` output.

### `sysl test` — running tests

```
sysl test <path>                      # file or directory (recursive)
sysl test --filter <pattern> <path>   # substring match on test/display name
sysl test --backend interpreter|trisc|all <path>
sysl test --fail-fast <path>
sysl test --verbose <path>
```

Output groups tests by source file with pass/fail markers and timings:

```
running 6 tests
std/mem/mem.lsysl
  ✓ test_copy_basic              (0.2ms)
  ✗ test_cmp_prefix              (0.1ms)
      panic: expected -1, got 1
  ✓ test_index_byte_found        (0.1ms)
...
5 passed, 1 failed, 0 skipped — 0.6ms
```

Exit code is 0 iff all tests pass. Failing tests print the source file and line of the `#test` attribute (`at file:line`).

**Builtins useful in tests:**
- `panic(msg: string) -> unit` — halts with the given message. Primary failure signal inside tests.
- `assert(cond: bool, msg: string) -> unit` — panics with `msg` if `cond` is false; returns otherwise.
- `expect(actual: i64, expected: i64, msg: string) -> unit` — panics with `"msg: expected N, got M"` if values differ. Better diagnostics than `assert(a == b, ...)`.

**Test output capture:** Any output from `print`, `println`, `puts`, or `puti` inside a test function is captured and displayed below the failure message if the test fails. This is useful for debugging intermediate values.

### `#address(N)` — map a var to a fixed physical address

Binds a module-level `var` declaration to a fixed physical address. Reads and writes become direct loads and stores at that address. No storage is emitted for the variable — it is just a typed handle on hardware. Intended for MMIO device registers:

```sysl
#address(0x1000_0000)
var uart_data: u32

#address(0x1000_0004)
var uart_status: u32

main() -> int
    uart_data = 0x41       // write 'A' to the transmit register
    while uart_status & 1 == 0 do ()  // poll the ready bit
    return 0
```

- The var must have an explicit type (no inferred type).
- `#address` cannot be combined with `const`.
- The attribute argument must be a single integer literal (decimal or hex).
- Compound assignment (`reg += v`, `reg |= mask`, …) lowers to a read-modify-write: `*(N as *T) = *(N as *T) op v`.
- In the interpreter, writes persist in a virtual MMIO map for the duration of the run; reads from untouched addresses return 0. On real hardware (LLVM / TRISC output), the load/store goes straight to the physical address.

Ada-equivalent: `for X use at 16#1000_0000#` in a representation clause.

### `#pure` — mark side-effect-free functions

A function marked `#pure` is checked by the compiler to have no observable side effects. Pure functions are a discipline enforcement tool: any violation is a compile-time error, not a warning.

```
#pure
square(x: int) -> int = x * x

#pure
fact(n: int) -> int
    if n <= 1 then return 1
    return n * fact(n - 1)
```

**What a `#pure` function may do:**
- Read its parameters and module-level `const`s
- Declare and mutate **local** variables (can't escape)
- Call other `#pure` functions (same or different file)
- Recurse (including mutually)
- Use arithmetic, comparison, casts, control flow (if/while/for/match/break/continue)
- Call `assert(cond, msg)` — termination is the only side effect, consistent with Ada `pragma Assert` policy

**What a `#pure` function may NOT do:**
- Call any non-`#pure` user function
- Call IO builtins (`puts`, `print`, `println`, `putchar`, `puti`)
- Call allocation builtins (`malloc`, `free`, `calloc`, `realloc`, `sbrk`)
- Call `panic` / `abort` / `expect` (side-effecting traps with observable output)
- Write to module-level `var`s
- Write through a pointer (`*p = v`), indexed slot (`arr[i] = v`), or struct field (`p.f = v`) — callers might see the write
- Increment/decrement struct fields (`p.f++`)
- Heap-allocate (`new`), append to a slice, construct closures
- Make indirect (function-pointer) calls or interface-dispatch calls
- Contain `asm` blocks

**Cross-module propagation:** `#pure` is carried through `.smeta` files. A `#pure` function in one module can call a `#pure` function in another module. Imported functions without `#pure` are impure, so annotate library functions you intend to call from pure code.

**Interaction with `--no-contracts`:** `#pure` checking is not a contract — it is a static enforcement and always runs. Only the runtime verification that contracts describe is elided by `--no-contracts`.

Future work: allow `#pure` calls inside `const` initializers and as default-parameter expressions, so that `const TABLE = build_table(16)` becomes legal at compile time.

### `#reads(...)` / `#writes(...)` — declare module-level effects

A looser sibling of `#pure`. The two attributes declare which module-level (file-scope or imported) mutable variables a function may read or write. They are the Sysl equivalent of SPARK's `Global => (Input => ..., Output => ..., In_Out => ...)` aspect, and are the static foundation a future verifier (Why3 / Boogie) needs to do sound weakest-precondition reasoning across function calls.

```
var config_root: int = 0
var io_buffer: [256]byte
var io_pos = 0i32

#reads(config_root)
get_max_threads() -> int = config_root

#reads(io_buffer)
#writes(io_pos)
write_byte(b: byte)
    io_buffer[io_pos] = b
    io_pos += 1

#reads()
#writes()
double(x: int) -> int = x * 2     // no module state; allocation/IO still allowed
```

**Syntax.** Each attribute goes on its own line above the declaration (matching `#pure` / `#deprecated` style). Either may be omitted; absence-of-both keeps the function in the unannotated default — see "Strict closure" below. Identifiers must resolve to module-level mutable `var`s (or `#address(N)` MMIO vars). `val`s and `const`s are immutable and cannot appear; pass them around freely without declaring an effect.

**The three rules the compiler enforces:**

1. **Body conformance.** In a function with `#reads(R)` and `#writes(W)`:
   - Every read of a module-level var V requires V ∈ R ∪ W.
   - Every write to a module-level var V requires V ∈ W.
   - Reads inside `require` / `ensure` / `invariant` / `assume` / `variant` clauses, and inside `for all` / `for some` predicates, count as reads.
2. **Call-site subset.** A call to a function with `#reads(R')` `#writes(W')` requires R' ⊆ R ∪ W and W' ⊆ W. The compiler computes both subsets at the call site and reports the offending variable name on mismatch.
3. **Strict closure.** An annotated function may only call other annotated functions (or `#pure` functions, which count as `#reads() #writes()`) and the pure builtins. Indirect calls, interface dispatch, `new`, `asm`, and unannotated functions are rejected. The intent is leaves-up adoption: annotate the bottom of the call graph first, then work upward.

**Relationship to `#pure`.** `#pure` keeps its existing stricter discipline — no allocation, no I/O, no indirect calls, no closure construction — and is shorthand for `#reads() #writes()` plus those extra bans. Combining `#pure` with explicit `#reads`/`#writes` is rejected as redundant.

**Compound assignment.** `counter += n` is a read-then-write of `counter`. By Rule 1, the implicit read is permitted whenever `counter` is in `#reads ∪ #writes`, so declaring `#writes(counter)` alone is sufficient. (SPARK's stricter `Output` vs `In_Out` distinction is deliberately not modeled in v1.)

**Indirect calls and interfaces.** Function-pointer types and interface types do not yet carry effect annotations, so they have no callable bound on what they touch. Calls through them inside an annotated function are rejected for v1. v2 will lift this once `FuncType` / `InterfaceType` grow optional effect signatures.

**Allocation.** `new`, `new []T`, slice `append`, and closure construction are all rejected from annotated function bodies. Allocation is an effect that the v1 model does not track; a future `#allocates(...)` attribute will handle it.

**Adoption strategy.** Add annotations from the leaves upward. Existing code is untouched (no annotations means "effects unknown" — exactly today's behavior, with no new restrictions). The first time you mark a leaf function `#reads() #writes()`, every annotated caller must follow suit; this is the point — it propagates the discipline up to the surfaces of your program at your own pace.

### Effect signatures on function types and interface methods

`#pure`, `#reads(...)`, and `#writes(...)` are also accepted as a suffix on function types and on interface methods, so callbacks and interface-dispatched code can participate in the same effect tracking as direct calls.

```
sort(arr: &[]int, cmp: (int, int) -> bool #pure)   // pure comparator

interface Sink
    push(x: int) #writes(buffer)                    // writes one global

#writes(buffer)
drain(s: Sink, arr: []int)
    for i in 0..<len(arr) do s.push(arr[i])         // allowed — iface effect is a subset
```

**Rules at an indirect-call site.** Calling through a function-typed value `f` from a function with `#reads(R)` `#writes(W)` requires `f`'s effect signature to be one of:

- `#pure` — always allowed (no module effects).
- `#reads(Rf)` / `#writes(Wf)` — allowed iff `Rf ⊆ R ∪ W` and `Wf ⊆ W`.
- *No signature* — **rejected**: the compiler cannot prove the call stays within the caller's declared effects.

Taking a function reference (`&fn_name`) carries the function's declared effects into the produced `FuncType`. `#pure` functions yield a pure-typed function pointer; `#reads(...)`/`#writes(...)` functions yield the corresponding `RW` type; unannotated functions yield an Unknown type and can only be invoked from unannotated callers.

**Rules at a boxing site.** Assigning a value of struct type `S` to a slot of interface type `I` requires every `I`-declared method's effect signature to be satisfied by the corresponding `S` method. "Satisfied" means the impl's effects are no wider than the interface declares — `#pure` impls satisfy any slot, and `#reads(Rs)` / `#writes(Ws)` impls satisfy `#reads(Ri)` / `#writes(Wi)` iff `Rs ⊆ Ri` and `Ws ⊆ Wi`. Concrete calls that violate this fail at boxing time, not at dispatch.

**Rules at an interface dispatch site.** Dispatching `iface.method(...)` from an annotated function uses the interface method's *declared* effects — not the impl's — for the subset check. This means the caller's static check is unaffected by which impl is currently boxed, matching the modular-reasoning discipline every verifier expects.

**Cross-module.** Effect signatures round-trip through `.smeta`, so `&imported_pure_fn` in a dependent unit produces the same effect-typed reference as `&local_pure_fn`.

**Closure effect inference.** Lambda expressions synthesize a `FuncType` whose effects are inferred from the closure body. The inference walks the typed body and produces one of three outcomes:

- **`#pure`** — no module-level reads/writes, no allocation, no impure calls, no writes to captured outer locals.
- **`#reads(R)` / `#writes(W)`** — specific module-level mutable globals were read or written, and every called function is itself annotated so its effects can be absorbed into the closure's signature. Reads/writes are unioned with the called functions' declared sets.
- **`Unknown`** — the body contains an un-summarizable construct (`new`, `append`, asm, an unannotated impure call, an indirect call through an Unknown-typed callable, or a write to a captured outer local). Such closures can only be passed to unannotated callback slots.

```
sort(arr, (a: int, b: int) -> a < b)            // pure → fits any #pure slot
find(arr, (v: int) -> v > threshold)            // reads `threshold` → fits #reads(threshold) slot
each(arr, (v: int) -> count = count + v)        // writes `count` → fits #writes(count) slot
```

Reads of captured outer locals don't contribute to the inferred sets — captures are opaque dataflow dependencies, not module-level effects. Writes through captures, by contrast, force the inference to `Unknown` (you can't summarize a write to an arbitrary outer-scope local as a fixed set of global names).

When a closure is passed to a callback slot, the slot's declared effects are checked against the inferred ones via the same subset rule used everywhere else: closure effects must be a subset of the slot's `#reads ∪ #writes` for reads, and a subset of the slot's `#writes` for writes.

### `#ghost` — verification-only declarations

A `#ghost` annotation marks a declaration as visible to the verifier but invisible at runtime. Ghost code lets contracts and proofs talk about state that doesn't exist in the executable — snapshots, counters, abstract collection state, "is this slice a permutation of the input" predicates — without paying any runtime cost. Three places `#ghost` may appear:

```
#ghost
var seen_count: int = 0          // module-level ghost var

#ghost
is_sorted(s: &[]int) -> bool     // module-level ghost fn — body free to read real state
    for i in 0..<len(s)-1 do
        if s[i] > s[i+1] then return false
    return true

sort(s: &[]int)
    require true
    ensure is_sorted(s)
    #ghost var input_len = len(s)  // ghost local — captured for use in `ensure`
    ensure len(s) == input_len
    ...
```

**The discipline.** The compiler enforces two rules:

1. **Real code cannot read ghost state.** Reading a `#ghost` variable, or calling a `#ghost` function, from real (non-ghost, non-contract) code is a static error. Ghost state has no runtime existence to read; the rule prevents accidental dependence.
2. **Ghost code cannot write real state.** A `#ghost` function may not assign to a non-ghost module-level var (writes to its own locals are fine — they're scoped to the function). This keeps the runtime behaviour independent of whether ghost code is present.

Contract clauses (`require` / `ensure` / `invariant` / `variant` / `assume` / `for all` / `for some` predicates) sit in *contract context* and may freely read both real and ghost state — that's the whole point of ghost code. The same is true for ghost var initializers, ghost-target assignment RHSes, and ghost function bodies.

**The strip pass.** After analysis, the compiler removes every ghost declaration before codegen. Ghost vars produce no storage; ghost functions emit no code. Inside real-function bodies it also drops:

- Statements that declare a ghost local (`#ghost var x = ...`).
- Plain or compound assignments to a ghost name (real-code assignments to ghost are implicitly ghost statements).
- Any `require` / `ensure` / `invariant` / `assume` clause whose expression touches a ghost name or calls a ghost function. The whole clause is dropped — there's no fallback runtime check that just covers "the real part."

The runtime sees a program identical to one written without `#ghost` at all. The verifier sees the full ghost-aware AST.

**Interaction with other attributes.** `#ghost` is mutually exclusive with `#pure`, `#reads(...)`, `#writes(...)` (ghost code doesn't run, so its runtime effects are irrelevant), `#address(N)` (ghost vars have no storage), and `const` (constants are inlined, not stored).

**v1 limitations.** Ghost parameters, ghost struct fields, and ghost return values are not yet supported; for now, model them by lifting the relevant state into a module-level `#ghost var`. Ghost code may not yet be referenced through function pointers or interface dispatch (the strip pass would have to descend into indirect-call targets).

### `#deprecated` — warn on use

Marks a function as deprecated. Calls to the function emit a warning to stderr during analysis (once per callee per compilation):

```
#deprecated("use foo2 instead")
foo() -> int = 1

#deprecated
old_api() -> int = 2
```

Warnings look like:
```
warning: 'foo' is deprecated: use foo2 instead
warning: 'old_api' is deprecated
```

Calls still compile and run normally — `#deprecated` only reports usage.

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

---

## Literate Sysl (`.lsysl` files)

`.lsysl` files are **literate programming** sources — full Markdown documents that contain Sysl code as indented blocks. The compiler extracts (tangles) the code and discards the prose; the documentation toolchain renders (weaves) the prose with syntax-highlighted code.

### Format

- **Prose** starts at column 0 — it's standard Markdown.
- **Code** is indented (4+ spaces or 1+ tabs) — extracted as Sysl source.
- Fenced code blocks (triple backticks) in the prose are **not** extracted as code — they're documentation-only examples.

```
This is prose explaining the module.

    module std.mem

    copy(dst: []byte, src: []byte) -> int
        // ... implementation ...

More prose describing the next function.

    set(dst: []byte, val: byte)
        // ...
```

### Markdown Features

The `.lsysl` renderer supports:

- **Headings** (`#` through `######`)
- **Paragraphs**, **bold** (`**text**`), **italic** (`*text*`)
- **Inline code** (backtick-delimited)
- **Fenced code blocks** with syntax highlighting (15+ languages including `sysl`, `python`, `javascript`, `rust`, `c`, `bash`, `json`, and more)
- **Indented code blocks** (default to `sysl` highlighting)
- **Unordered and ordered lists** (with nesting)
- **Block quotes** (`>`)
- **Tables** (GFM-style pipe tables)
- **Horizontal rules** (`---`)
- **Links** (`[text](url)`)
- **HTML comments** (`<!-- -->`)
- **LaTeX math** via KaTeX — inline `$x^2$` and display `$$equation$$` (CommonMark/Pandoc convention; the markdown processor emits the right `\(..\)` / `\[..\]` delimiters for KaTeX automatically). Do not write `\(..\)` or `\[..\]` directly in source — the markdown parser strips the backslashes before KaTeX sees them.

### Commands

```
sysl doc <file.lsysl>                   # render one file to HTML
sysl doc <directory>                     # render all .lsysl in directory + index
sysl doc --output <dir> <file.lsysl>    # specify output directory
```

### Tangling

When compiling, the `.lsysl` parser extracts all indented blocks as Sysl source, concatenating them in order. The extracted code is then compiled identically to a `.sysl` file. Module declarations, imports, functions, and `#test` annotations all work inside `.lsysl` code blocks.
