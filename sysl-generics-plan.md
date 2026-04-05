# Sysl Generics — Multi-Phase Implementation Plan

Status: **proposal**
Last updated: 2026-04-05

Sysl will gain Go-style parametric polymorphism via **monomorphization**. The design
follows Go's surface syntax and semantics (`[T]` type parameters, inference at call
sites, no runtime type information, one specialized copy per instantiation).

---

## Guiding principles

1. **Go-compatible syntax.** `[T]` type parameter lists, `[T any]`-style constraints
   eventually, inference at call sites.
2. **Monomorphization, not type erasure.** Each distinct type instantiation produces
   a separate compiled function or struct layout. Zero runtime dispatch, zero hidden
   indirection, trivially inspectable in TRISC assembly (fits teaching ISA).
3. **Pay-as-you-go complexity.** No generics in user code → zero compile-time or
   runtime cost. Generic code costs only what it instantiates.
4. **Instantiation-time type checking (C++ template model for now).** Operations on
   a type variable `T` are checked when `T` is pinned to a concrete type at a call
   site. This is simpler than Go's constraints and lets us defer constraint design.
5. **Backend-agnostic.** All generics are resolved by the analyzer. The interpreter,
   TRISC codegen, and LLVM codegen see only fully-specialized typed AST — no new
   backend work required.

---

## Phases

### Phase 1 — Generic functions (MVP)

**Scope**
- Type parameter lists on top-level functions: `swap[T](a: *T, b: *T)`
- Multiple type parameters: `pair[K, V](k: K, v: V) -> ...`
- Type variables usable in parameter types, return types, local var declarations
- **Inference-only at call sites** — caller writes `swap(&x, &y)`, compiler infers `T`
- **Instantiation-time checking** — e.g. `max[T](a: T, b: T) -> T` using `a > b`
  compiles fine for `T=int`, errors at the `T=bool` call site with a clear message
- **Monomorphization cache** — each unique `(name, type-arg-tuple)` compiles once

**Surface examples**
```sysl
func swap[T](a: *T, b: *T)
    val tmp = *a
    *a = *b
    *b = tmp

func max[T](a: T, b: T) -> T
    if a > b then a else b

var x = 1; var y = 2
swap(&x, &y)                  // T = int
val m = max(3.14, 2.71)       // T = f64
```

**Non-goals for Phase 1**
- Generic structs (Phase 2)
- Generic methods (Phase 2)
- Explicit type arguments at call sites (Phase 3)
- Constraints like `T any`, `T comparable` (Phase 4)
- Type parameters on extern / asm functions

**Rejected operations (instantiation error)**
```sysl
max(true, false)              // ERROR at call site: operator > not defined for bool
```

---

### Phase 2 — Generic structs and methods

**Scope**
- Type parameters on struct declarations: `struct Pair[T] { first: T; second: T }`
- Generic tagged unions: `enum Option[T] { Some(T), None }`
- Methods on generic structs: `(p: *Pair[T]).swap()`
- Generic struct types used in function signatures
- Monomorphized struct layouts — each instantiation is a distinct struct type with
  a distinct memory layout, size, and `sizeof`

**Surface examples**
```sysl
struct Stack[T]
    items: &[]T
    len: int

(s: *Stack[T]).push(x: T)
    s.items = append(s.items, x)
    s.len += 1

(s: *Stack[T]).pop() -> T
    s.len -= 1
    s.items[s.len]

enum Option[T]
    Some(value: T)
    None

safe_div(a: int, b: int) -> Option[int]
    if b == 0 then None
    else Some(a / b)
```

**Implementation notes**
- Struct monomorphization must happen before field offset resolution
- `sizeof(Stack[int])` and `sizeof(Stack[Point])` may differ
- `deinit` blocks on generic structs are themselves instantiated per type
- Refcount / ARC semantics apply per-instantiation

---

### Phase 3 — Explicit type arguments

**Scope**
- Call sites may supply type arguments directly: `swap[int](&x, &y)`
- Struct instantiation: `val s = new Stack[int]()`
- Useful when inference can't resolve (e.g. no args, return-type-only generics)

**Surface examples**
```sysl
func make_empty[T]() -> &[]T
    new [0]T

val xs = make_empty[int]()       // explicit — can't infer T from zero args
val ys: &[]int = make_empty()    // alternative: infer from context (optional)
```

**Implementation notes**
- Parser accepts `ident [typeArgs] (args)` at call sites
- Analyzer prefers explicit over inferred; consistency check if both provided
- Covers `new Stack[int]()`, `Pair[int, string](1, "x")`, etc.

---

### Phase 4 — Trait bounds on type parameters

**This phase is owned by the traits feature.** See `sysl/traits-design.md` for the
full design. Generic type parameters gain the ability to constrain themselves to
types that implement a nominal trait:

```sysl
func max[T: Ord](a: T, b: T) -> T =
    if Ord.gt(a, b) then a else b

func sort[T: Ord + Eq](s: []T) = ...
```

Trait bounds **replace** the earlier idea of built-in pseudo-constraints like
`any` / `comparable` / `ordered` / `numeric`. Real nominal traits (`Eq`, `Ord`,
`Hash`, `Display`, `Iterator`, etc.) are strictly better: they carry laws,
support default methods, allow `?`/`From[E]`-style conversion, and give much
better error messages.

**Interaction with monomorphization:** at each generic instantiation, the
analyzer checks that the concrete type satisfies every declared bound by looking
up the corresponding `impl` block. Missing impl → clear error at the call site
naming the trait. Trait method calls inside the generic body (`Ord.cmp(a, b)`)
resolve via the same impl lookup after substitution.

**Depends on:** generics Phase 1 (this document), traits Phases 1-2 (separate doc).

---

### Phase 5 — Generic type aliases and misc.

- Generic type aliases: `type Callback[T] = func(T) -> T`
- Variadic type parameters (probably not)
- Higher-kinded parameters (not planned)

---

## Architecture

### Data model changes

**SyslAST.scala**
- `FunDeclAST`: add `typeParams: List[String]` (empty for non-generic)
- `StructDeclAST`: add `typeParams: List[String]` (Phase 2)
- `DataEnumDeclAST`: add `typeParams: List[String]` (Phase 2)
- New type-syntax variant: `TypeVarAST(name: String) extends TypeAST`
- `NamedTypeAST`: add `typeArgs: List[TypeAST]` (Phase 2, default `Nil`)
- `CallAST`: add `typeArgs: List[TypeAST]` (Phase 3, default `Nil`)

**SyslTypedAST.scala**
- After analysis, all type parameters are resolved. Typed AST contains no type
  variables — every `TFunDecl` is fully monomorphized with concrete types.
- Multiple specializations of the same source function appear as separately-named
  `TFunDecl` nodes.

**SyslType.scala**
- Add `TypeVar(name: String) extends SyslType` for analyzer-internal use
- Substitution: `SyslType.substitute(env: Map[String, SyslType]): SyslType`
- After monomorphization, no `TypeVar` survives into codegen input

### Analyzer additions

Template storage:
```scala
// Generic function declarations are stored as templates, keyed by name
case class GenericFunTemplate(decl: FunDeclAST, typeParams: List[String])
val templates: Map[String, GenericFunTemplate]

// Instantiations cache: (baseName, typeArgs) -> specializedName
val instantiations: Map[(String, List[SyslType]), String]

// Specialized declarations accumulated during analysis
val specializedDecls: ListBuffer[TFunDecl]
```

Workflow at a generic call site:
1. Look up the template for the callee name.
2. Unify the argument types against the parameter types to infer `T1..Tn`.
3. Check the cache — if `(name, inferredArgs)` already instantiated, reuse the
   mangled name.
4. Otherwise, substitute `T → concrete` throughout a *cloned* `FunDeclAST` body,
   then analyze that cloned body as a regular function with a mangled name
   (`swap_int`, `max_f64_f64`, etc.).
5. Append the resulting `TFunDecl` to `specializedDecls`.
6. Rewrite the `CallAST` to call the mangled name.

**Type inference algorithm** (Phase 1, simple unification):
- Walk each (parameter-type, argument-type) pair
- Where parameter-type contains a type variable, record the mapping
- If the same type variable is seen with conflicting concrete types, error:
  "cannot infer T: seen both int and f64"
- All type parameters must be pinned after walking all arguments; otherwise error:
  "cannot infer type parameter T" (Phase 3 will allow explicit annotation here)

### Name mangling

Format: `<basename>_<type1>_<type2>_...`
- `swap[int]` → `swap_int`
- `pair[int, string]` → `pair_int_string`
- `Stack[Option[int]]` (Phase 2) → `Stack_Option_int`

Keep it simple, human-readable for assembly inspection, and collision-free by
construction. If a user defines `swap_int` themselves, the analyzer errors on
collision.

### Codegen impact

**None for Phase 1.** After analysis, the typed AST contains only concrete
`TFunDecl` nodes — same shape as before. TRISC codegen, LLVM codegen, and the
interpreter are unchanged.

Phase 2 (generic structs) requires struct-layout monomorphization *before*
field-offset resolution, but that happens in the analyzer too.

---

## Parser grammar changes

**Function declaration (Phase 1)**
```
funDecl := ("private")? "func"? ident typeParams? "(" params ")" ("->" typeRef)? funBody
typeParams := "[" ident ("," ident)* "]"
```

Note: the existing grammar already uses `[n]T` for arrays and `arr[i]` for indexing;
type parameter lists are unambiguous because they only appear *after* an ident and
*before* `(`. Careful lookahead may be needed to disambiguate.

**Struct declaration (Phase 2)**
```
structDecl := "struct" ident typeParams? structBody
```

**Type references (Phase 2)**
```
typeRef := ident typeArgs? | ...
typeArgs := "[" typeRef ("," typeRef)* "]"
```

**Call sites (Phase 3)**
```
call := ident typeArgs? "(" args ")"
```

---

## Testing strategy

**Phase 1 test matrix**
- `swap[T]` with int, f64, string, struct, pointer, ref
- `identity[T](x: T) -> T` for each type
- `max[T]`/`min[T]` with numeric types; expect error for bool
- Multiple type parameters: `pair[K, V]`
- Nested generic calls: `swap(swap-result)`
- Instantiation cache: calling `swap[int]` twice produces one TFunDecl
- Name collision: user `swap_int` collides with `swap[int]`
- Inference failure: explicit vs implicit disagree (defer to Phase 3)

**Backends**
- Interpreter tests in `SyslGenericsTests` (in `sysl/`)
- TRISC codegen tests in `SyslCodegenGenericsTests` (in `trisc-cli/`)
- Verify monomorphized output in assembly dumps (spot check)

---

## Open questions

1. **Should Phase 1 allow `T` in local variable type annotations?**
   Yes — useful inside function bodies. E.g. `var tmp: T = *a`.

2. **What happens with recursion in generic functions?**
   Supported. Recursive call is re-analyzed as a call to the same instantiation,
   which is already in the cache.

3. **How do generic functions interact with modules / imports?**
   A generic template crosses module boundaries as a template, not a specialization.
   The importing module triggers instantiation when it calls the function. This
   means the template source must be available cross-module. For now, this is fine —
   analyzer has access to all compilation units. Later, if we want separate
   compilation, we'll need to revisit (Rust/C++ approach: emit specializations on
   demand in each compilation unit with deduplication at link time).

4. **Should `T` appear in struct field types before Phase 2?**
   No — Phase 1 is function-only. Field types must be fully concrete.

5. **Name mangling with complex types (Phase 2)?**
   E.g. `*T`, `[]T`, `Pair[int]`. Proposal: flatten via recursive mangling:
   `swap[*int]` → `swap_ptr_int`, `foo[Pair[int]]` → `foo_Pair_int`. Resolve
   ambiguity (e.g. `Pair_int` vs `Pair__int` for nested) with a separator escape
   if needed.

---

## Rollout

- Phase 1: single PR. Analyzer + parser + AST + tests.
- Phase 2: build on Phase 1 infrastructure; separate PR.
- Phase 3, 4: smaller incremental PRs.

After each phase, `sysl-reference.md` is updated with the newly-available syntax.

---

## Summary

| Phase | Adds | Effort | Blockers |
|-------|------|--------|----------|
| 1 | Generic functions, inference, monomorphization infrastructure | done (cec4281) | — |
| 2 | Generic structs, enums, methods | ~2 days | Phase 1 |
| 3 | Explicit type arguments at call sites | ~0.5 day | Phase 1 |
| 4 | Trait bounds on type parameters | ~1 day | Phase 1, traits Phase 1-2 |
| 5 | Generic type aliases and misc. | ~0.5 day | Phase 1 |

See also: `sysl/traits-design.md` for the companion traits design.
