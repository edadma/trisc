# Sysl Audio-Workload Feature Roadmap

Three language features motivated by porting `musicbox` (a ~2000-line C Q1.31 fixed-point additive synth at `~/dev/musicbox/`) to sysl, and by the goal of eventually running it on SLIX as the first non-trivial userspace audio program. The features are also useful well beyond audio — they exist on this roadmap because the synth port is a small, real test bed that surfaces ergonomic gaps that pure-compiler tests can't.

This roadmap covers three features in the recommended add order. A fourth idea — inline tail-array structs — exists in the design conversation but isn't on this roadmap; its payoff is narrower (one `Event` struct in musicbox plus a few SLIX kernel spots) and the layout-implementation work is the heaviest of the four.

| # | Feature | Order | Status |
|---|---|---|---|
| 1 | `#realtime` / `#no_alloc` effect annotation | First | In progress |
| 2 | `const fn` compile-time evaluation | Second | Planned |
| 3 | Const-int-generics + `Fixed[I, F]` library + Q-format literal syntax | Third | Planned |

The order is "smallest novel-but-self-contained thing first, biggest dependency-bundle last." Each section ends with hazards — places where the design has a real choice to make or where a known trap is waiting.

---

## 1. `#realtime` effect annotation

### Motivation

An audio render callback, an interrupt handler, a kernel signal walker, an IPC fast path — all share the same constraint: do not touch the allocator. Today this is maintained by review and discipline. A compiler-checked property turns it into a property the codebase can rely on, not a thing reviewers have to remember to enforce.

### Design

`#realtime` is a new effect axis, orthogonal to `#pure` / `#reads` / `#writes`. A function can be:

- Both `#pure` and `#realtime` (pure mathematics that never grows an array)
- Just `#realtime` (a render callback that writes through a buffer pointer)
- Just `#pure` (a pure function that internally uses growable arrays — not realtime-safe, but observably side-effect-free)
- Neither (the default)

What `#realtime` forbids in a function body:

- `new T(...)` / `new T { ... }` (`TNew`, `TNewEnum`)
- `new [n]T` (`TNewArray`)
- `arr.push(x)` / `arr.append(x)` (`TAppend`, any growable-array op)
- Closure construction (`TClosure` — closures live on the heap)
- Calls to non-`#realtime` callees, including builtins that allocate
- Indirect calls through a `FuncType` whose `effects.isRealtime` is false
- Interface dispatch where the interface method's effect signature lacks `#realtime`
- ASM expressions are *allowed* (user controls them; let them break the rule if they need to)

What `#realtime` allows that `#pure` doesn't:

- Pointer writes, field writes, index writes
- Calls to other `#realtime` functions that have side effects (mutations through `*T`)
- Mutation of module-level state (if declared via `#writes`)

### `&T` and refcount drops — the subtle case

In sysl's three-mode model, `&T` parameters increment the refcount on call entry and decrement on call exit. The inc/dec themselves are atomic ops, not allocations, so they're realtime-safe. The hazard is when a decrement reaches zero: the runtime calls the type's `deinit` and then `free`. That's allocator activity.

For v1, the conservative rule:

- Receiving `&T` as a parameter is *fine* (caller still holds a ref during the call; dec-on-return cannot reach zero).
- Reading a `&T` local is *fine*.
- *Writing* to a `&T` local is *forbidden* (the overwrite drops the old value's refcount, possibly to zero).
- Returning a `&T` you constructed is *forbidden* (would have required `new`, which is already forbidden).
- Passing a `&T` you received through to another `#realtime` callee is *fine*.

Document this as a known limit. Users who need the hot loop to mutate ref-typed locals can convert to `*T` raw pointers; that's already the kernel idiom.

### Work breakdown

1. **Extend `FuncEffects`** (`SyslType.scala`): add `isRealtime: Boolean = false` field. Update `Unknown`/`Pure` constants and the pretty-printer (` #realtime` suffix). New constant `FuncEffects.Realtime = FuncEffects(isRealtime = true)` for convenience but most usages will combine flags.

2. **Parser** (`SyslParser.scala`):
   - Add `"realtime"` case to `funcTypeEffects` (line ~578).
   - Wire `#realtime` on declaration-side parsing (mirrors how `#pure` already attaches to `def`/`fn` declarations — same mechanism).
   - Allow `#realtime` to combine with `#pure`, `#reads`, `#writes`. Forbid only obvious nonsense.

3. **Analyzer — `validateRealtimeFn`** (`SyslAnalyzerContracts.scala`): new method, modeled on `validatePureFn`. Reject the forbidden ops listed above. Transitively check callees via `FunInfo.isRealtime` (which gets populated in the same pre-collection pass that populates `isPure`).

4. **Call-site effect checks**: extend `effectsSatisfy` so a realtime caller may only call realtime callees, and so interface boxing requires impl methods to satisfy interface methods' realtime annotation (the existing pure check at SyslAnalyzerContracts.scala:119 is the template).

5. **Tests** under `sysl/tests/lang_features/effects/realtime_*.lsysl`. Coverage matrix:
   - Basic realtime fn passes
   - `new` rejected; `[]T.push` rejected; closure construction rejected
   - Realtime calling non-realtime rejected; realtime calling realtime accepted
   - Interface dispatch through realtime method accepted; through non-realtime method rejected
   - Indirect call through realtime `FuncType` accepted
   - `#realtime #pure` combination works
   - `&T` parameter received and forwarded works; `&T` local reassignment rejected
   
   Run on all 7 backends per CLAUDE.md rule 13. The checker is purely in the analyzer so all backends will see the same diagnostics; the per-backend sweep is to confirm nothing else regresses.

6. **`reference.md`** — new subsection under Runtime Safety / Effects. Document the forbidden ops, the transitive check, interface compatibility, the `&T`-assignment limit, the orthogonality with `#pure`, and give two examples (a synth render callback and a kernel signal walker).

### Hazards

- **Allocator-touching builtins must be classified.** Anything in `builtinFunctions` that potentially allocates needs `isRealtime = false` in its FunInfo. `assert` should be `#realtime` (just compares and traps). `panic`/`abort` are tricky — they don't allocate, but they call into runtime support that might. Conservative call: not realtime in v1.
- **String concatenation.** `s"interpolated ${x}"` may allocate a new string. Reject from `#realtime` context. Static `"literal"` strings are fine. Format strings (`f"..."`) probably allocate — reject.
- **The transitive check needs a closure of `isRealtime`.** Existing `isPure` propagation is the model: pre-collection sets `FunInfo.isRealtime` based on declaration; body analysis confirms the property holds; cross-module reads from `.smeta`.
- **`&T` parameter inc/dec emission.** Need to confirm the codegen emits atomic ops, not function calls to a runtime helper. If a helper *is* called, mark it `#realtime` (it doesn't allocate, just counts).

---

## 2. `const fn` compile-time evaluation

### Motivation

Musicbox builds its sine table at runtime startup with floating-point `sin()` calls. On a Pico-class target you want that table in ROM; on desktop you want one less initialization step. Both fall out of letting pure functions run at compile time:

```sysl
const fn build_sine_table(size: i32) -> [size]i32 {
    var table: [size]i32
    for i in 0..<size {
        let angle = 2.0 * PI * (i as f64) / (size as f64)
        table[i] = (sin(angle) * 0x7FFFFFFF as f64) as i32
    }
    table
}

const SINE_TABLE: [1024]i32 = build_sine_table(1024)
```

Beyond audio: kernel jump tables, CRC tables, Gray codes, log/exp/atan lookup, character classification tables, hash function precomputation. SLIX would use this immediately.

### Design choices

The two design ancestors are Rust (`const fn`, explicit opt-in, restricted to pure) and Zig (`comptime`, pervasive, anything can be comptime). Sysl's spirit prefers explicit-over-magic: choose the Rust shape.

- **`const fn` keyword** on the function declaration (or `@const` attribute — pick the form that fits sysl's existing syntax best).
- **Pure-only**: no I/O, no mutable globals, deterministic. Reuses the existing `#pure` discipline plus the additional restriction that all inputs must be compile-time-known.
- **Float math is fine**: IEEE 754 is deterministic across platforms.
- **No compile-time allocator in v1**: stack arrays and fixed-size returns only. Sized arrays returned by value are the main use case. Strings constructed at compile time would need a compile-time string heap — defer.
- **`const SINE_TABLE: [1024]i32 = build_sine_table(1024)`**: the `const` keyword at the value-binding level triggers evaluation. If the RHS expression's call graph is all `const fn`, evaluate and embed the result as a literal initializer.

### Implementation strategy

The compiler already has an interpreter — `SyslInterpreter*.scala`. Compile-time evaluation can *reuse* it: serialize the typed AST of the `const fn`, run the interpreter against compile-time-known argument values, capture the result, embed it as a typed literal in the IR before codegen.

This is much cheaper than building a separate `constexpr` evaluator from scratch — the interpreter is already a tested artifact (the test runner uses it as backend #1) and it already understands every node the analyzer emits.

### Work breakdown

1. **Parser**: recognize `const fn` declarations and `const` value bindings. Reject `const fn` with non-pure bodies at declaration time.

2. **Analyzer**: mark `FunInfo.isConst = true`. Reuse pure-discipline checks plus a "no closure captures of non-const values" check. At `const SINE_TABLE = ...` sites, verify the expression's call graph is all `const fn`.

3. **Const-eval driver**: new post-pass that finds `const`-bound values, invokes `SyslInterpreter` against the typed AST with compile-time arguments, and replaces the binding's initializer with a `TArrayLit`/`TStructLit`/scalar literal.

4. **Codegen**: no changes — once the const-eval pass has lowered to a literal, every backend already handles array/struct literal initializers.

5. **Tests** under `sysl/tests/lang_features/const_eval/`. Coverage:
   - Scalar const fn (`const fn sq(x: i32) -> i32 = x * x`)
   - Array-returning const fn (the sine table example)
   - Recursive const fn (factorial, Fibonacci) with appropriate depth limit
   - `const fn` calling another `const fn`
   - `const fn` calling a non-const fn → reject
   - `const` value initialized from a non-`const fn` → reject
   - Float computation at compile time matches runtime computation

6. **`reference.md`** — new subsection. Cover the `const fn` keyword, the pure-only restriction, what's permitted in the body, the const-binding form, the float-determinism guarantee.

### Hazards

- **Infinite loops at compile time.** Hard cap on interpreter steps during const-eval; report "const evaluation exceeded N steps" with a clear pointer to the call.
- **`#pure` vs `const fn`.** `const fn` implies `#pure` (and probably should implicitly carry it). Decide whether the syntax forces the user to write `#pure const fn` or whether `const fn` quietly implies it.
- **Cross-module const values.** A `const X = ...` in module A used by module B must be serialized through `.smeta`. The literal form (after eval) is easy; the question is whether B can call A's `const fn` itself at compile time (it should — `const fn` bodies need to live in `.smeta`).
- **Const-eval allocator scope.** Sine table is a stack array. What about `[1024 * 1024]i32` — does that exceed interpreter stack? Set a per-`const` heap arena with a configurable size limit.
- **Determinism across the seven backends.** `f64 sin()` should give bit-identical results everywhere because IEEE 754 is deterministic — but Scala's `math.sin` on JVM may differ in the last ULP from libm's `sin` if the LLVM-host backend ever does its own compile-time eval. Pick one: the JVM interpreter is the source of truth for all compile-time evaluation; backends consume the lowered literals. No backend ever re-evaluates a `const fn`.

---

## 3. Const-int-generics + `Fixed[I, F]` + Q-format literals

### Motivation

Musicbox is wall-to-wall Q1.31 fixed-point math. Every multiplication is `(a as i64) * (b as i64) >> 31`. A `Fixed[I=1, F=31]` library type with operator overloading lets the user write `a * b` and the compiler handles the shift. Saturating variants (`+|`, `*|`) cover the mixer-overflow case (mixing 32 voices at peak can overflow `i32`).

The hidden dependency is the language feature: `Fixed[I, F]` needs **const-int-generics** — struct type parameters that are integer constants, not just types. Sysl has constrained ints (`T::Range`, `within`) and enum metaprogramming, but I'm not certain it has true const-int-generics today. If it doesn't, this section is mostly about adding them; `Fixed` itself is a library exercise on top.

Without const-int-generics, `Fixed` degenerates into one type per Q-format (`Q1_31`, `Q2_30`, `Q0_32`). Workable but a real ergonomic loss; the user feels it on every line.

### Design

**Const-int-generics surface:**

```sysl
struct Fixed[I: i32, F: i32] {
    raw: i32  // requires I + F == 32, signed
    @static_assert(I + F == 32, "Fixed[I, F] must fit in 32 bits")
}
```

A type parameter can be:
- A type (`T`)
- An integer constant (`I: i32`, `F: i32`)

Const-int parameters can appear in:
- Field types (`raw: [I + F]bit` if bit types existed; for now just in `@static_assert`)
- Array sizes (`buf: [N]u8`)
- Match arm guards
- Constant expressions inside the struct's method bodies

This is the same shape as Rust's `const generics` and C++ `template <int N>`. Sysl already has `T::Range` etc. on enums; extending to numeric-parameterized structs is the next step.

**Operator overloading for `Fixed`:**

```sysl
impl Mul for Fixed[I, F] where I + F == 32 {
    fn op_mul(self: Fixed[I, F], other: Fixed[I, F]) -> Fixed[I, F] {
        Fixed[I, F] { raw: ((self.raw as i64) * (other.raw as i64) >> F) as i32 }
    }
}
```

Sysl's existing operator-overloading mechanism (per `reference.md`'s Operator Overloading section) should handle this once const-int-generics exist.

**Q-format literal syntax:**

- `0.5q1.31` — Q1.31 representation of 0.5 → `0x40000000`
- `1.0q2.30` — Q2.30 representation of 1.0 → `0x40000000`  
- `-1.0q1.31` — `0x80000000`

The parser turns `<float>q<I>.<F>` into a `Fixed[I, F]` typed literal. Pure parser work; the type-checker has to know how to interpret the literal as `Fixed[I, F]`.

**Saturating operators:** if `+|`, `*|`, `-|` aren't already in sysl, this section adds them as part of the Fixed package. They're conceptually independent (any integer type benefits), but the synth's mixer is the motivating use case.

### Work breakdown

1. **Audit current state.** Confirm sysl does *not* already have const-int-generics. If it does, skip step 2. (`T::Value` enum metaprogramming is related but not the same — that's compile-time access to enum field values, not type parameters bound to integer constants.)

2. **Const-int-generics in the analyzer.** Extend the type-parameter mechanism (`typeParams: List[String]` on `StructDeclAST`, etc.) to also support `(String, IntType)` pairs. Substitution: where the analyzer currently substitutes a type-param `T` for a concrete type, also substitute an int-param `I` for a concrete integer literal at instantiation. This is the bulk of the work; touches generics (`SyslAnalyzerGenerics.scala`), substitution (`SyslAnalyzerTypes.scala`), and codegen mangling (every backend's name-mangling has to encode the int value, e.g. `Fixed$1$31`).

3. **`@static_assert` inside generic struct bodies.** Already exists per CLAUDE.md (`static_assert(sizeof(T) == N, "msg")`); confirm it works inside generic struct decls with const-int params.

4. **Q-format literal parser.** Add `<float>q<int>.<int>` to the literal grammar. At type-check time, fold to `Fixed[I, F]` with `raw = round(value * 2^F)`. Reject if value is out of range (`-2^I .. 2^I - 1`).

5. **Saturating operators.** Confirm `+|`, `*|`, `-|` on integer types (the CLAUDE.md notes saturating arithmetic carve-outs were dropped from reference.md, suggesting it's on all backends now — verify). If only the carve-outs were dropped but the feature was never exposed at language level, that's the small follow-up.

6. **`Fixed` library** at `std/math/fixed.lsysl`. The Q-format struct with `op_mul`, `op_add`, `op_sub`, `op_div`, `op_neg`, plus saturating variants. Conversion to/from `i32` (`Fixed[I, F]::from_raw(x)`, `.to_raw()`), to/from `f64` (`Fixed[I, F]::from_f64(x)`, `.to_f64()`). Document the range and precision per `[I, F]`.

7. **Tests** at `sysl/tests/lang_features/generics/const_int_generics_*.lsysl` (for the language feature) and `std/math/fixed.lsysl` test sections (for the library). 

8. **`reference.md`** — extend the Generics section to cover const-int-generics; add a Fixed-Point Math section under Numeric Types.

### Hazards

- **Mangled names with integer parameters.** Cross-backend name-mangling has to be consistent. Pick one form: `Fixed$1$31` or `Fixed_I1_F31`. Document it. Update every backend's mangler.
- **Where clauses with arithmetic constraints.** `where I + F == 32` is a non-trivial compile-time constraint solver. For v1, restrict the constraint language to simple equalities and inequalities checkable at instantiation (no full Presburger arithmetic).
- **Q-format literal precision.** `0.1q1.31` rounds to the nearest representable Q1.31 value. Document the rounding mode (round-to-nearest-even is standard). Reject literals with explicit `.5` mid-bit values that can't be represented exactly only if the user opts into strict mode (probably not v1).
- **Negative values in unsigned Q-formats.** `Fixed[U=0, F=32]` (or however unsigned Q is spelled) needs literal-range checks. Decide: do we want `UFixed[I, F]` as a separate type, or a `signed: Boolean` parameter? Probably a separate type for clarity.
- **Saturating arithmetic on Fixed.** `+|` on raw `i32` is one operator; `Fixed[I, F] +| Fixed[I, F]` calls the user-defined operator overload which internally does saturating `i32` math. Confirm the overload mechanism dispatches `+|` distinctly from `+`.

---

## Cross-cutting: the porting exercise

Once #1 lands, port enough of musicbox to sysl to:

1. Render a single sine wave through a hardcoded callback into a `[N]i16` buffer (no driver yet).
2. Add the envelope/sequencer state machine.
3. Use interfaces for `Instrument` (replacing the C `instrument_t` vtable).

This pass will surface real ergonomic gaps. Don't speculate about them up front — let the port find them.

After #2 lands, the sine table moves to a `const fn`.

After #3 lands, all the Q1.31 multiplications become operator overloads on `Fixed[1, 31]` and the literals get nicer.

The CoreAudio (macOS) or SLIX-audio-server binding is a separate work item — not on this roadmap. The render-callback core comes first.

## How this roadmap is used

- **Add order is the order**: #1 → #2 → #3. Don't start later items before earlier ones land; the work after #1 builds on the effect-system shape #1 establishes.
- **Each feature gets its own done memo** in auto-memory when it lands, with concrete commit SHA, test counts on all 7 backends, and any ergonomic surprises that informed the next item.
- **The musicbox port is the rolling validator**: every time a feature lands, port more of the synth and let the porting experience drive the next refinement.
