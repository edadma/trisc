# std.result — Blocked by Generic Inference Limitation

**Date:** 2026-04-05
**Status:** Blocked. Module written but unusable pending language fix.
**Severity:** High — blocks core stdlib utility APIs over generic container types.

---

## Summary

While building `std.result` (a `Result[T, E]` type with utility functions like
`is_ok`, `unwrap`, `unwrap_or`, `expect`), two Sysl language limitations surfaced
that prevent the module from compiling. One is a minor ergonomics issue with a
straightforward workaround; the other blocks the entire utility-function API and
needs a language fix to proceed.

The `Result[T, E]` enum itself works. The `?` operator and `match` patterns work.
What does not work are free functions that take a generic-enum value as a
parameter — i.e., the Rust-style `Result::is_ok(&self) -> bool` pattern expressed
as `is_ok[T, E](r: Result[T, E]) -> bool` in Sysl.

---

## Issue 1 — Type inference does not unify through generic type constructors

### Symptom

```sysl
enum Result[T, E]
    Ok(value: T)
    Err(error: E)

is_ok[T, E](r: Result[T, E]) -> bool
    r match
        Ok(_) -> true
        Err(_) -> false

main() -> int
    val r: Result[int, int] = Ok(42)
    assert(is_ok(r), "should be ok")    // ERROR
    0
```

**Error:**

```
cannot infer type parameter 'T' for generic function 'is_ok'
```

### Expected behavior

At the call `is_ok(r)`, the analyzer should:

1. See that `r` has type `Result[int, int]`.
2. See that `is_ok`'s first parameter has type `Result[T, E]`.
3. Unify `Result[int, int]` against `Result[T, E]` recursively.
4. Conclude `T = int`, `E = int`.
5. Instantiate `is_ok_int_int` and proceed.

This is a standard unification algorithm on parameterized types.

### Actual behavior

The analyzer reports "cannot infer type parameter T" because it does not appear
to unfold the outer type constructor. Inference seems to only work when type
parameters appear directly as a parameter type (`id[T](x: T)`), not when they
appear nested inside a user-defined generic type (`foo[T](x: Container[T])`).

### Root cause (hypothesis)

Looking at the generics plan (`sysl-generics-plan.md`):

> **Type inference algorithm** (Phase 1, simple unification):
> - Walk each (parameter-type, argument-type) pair
> - Where parameter-type contains a type variable, record the mapping

"Contains a type variable" is ambiguous. If the unification walk only handles
type vars at the **top level** of each parameter type and doesn't recurse into
type constructors, this symptom is explained.

### Impact

This is **pervasive**. Nearly every stdlib utility function that takes a
generic container and returns a primitive has this shape:

- `is_ok[T, E](r: Result[T, E]) -> bool`
- `is_some[T](o: Option[T]) -> bool`
- `unwrap[T, E](r: Result[T, E]) -> T`
- `unwrap_or[T, E](r: Result[T, E], default: T) -> T`
- `len[T](v: Vec[T]) -> int`
- `empty[K, V](m: Map[K, V]) -> bool`
- `first[T](s: []T) -> Option[T]`

All blocked by the same inference gap. Users can still pattern-match directly,
but stdlib convenience functions are impossible to write.

### Why this matters for the stdlib

The point of `Result[T, E]` utility functions is ergonomics: `unwrap_or(r, -1)`
is clearer than the equivalent `match` expression at every call site. Without
these helpers, users write:

```sysl
val sum = validate_sum(10, 20) match
    Ok(v) -> v
    Err(_) -> -1
```

Instead of:

```sysl
val sum = unwrap_or(validate_sum(10, 20), -1)
```

The former is fine once, tedious at every call site.

### Recommended fix

Extend the Phase 1 type-inference algorithm to **recursively unify** through
user-defined parameterized types. Concretely, when the parameter type is
`Result[T, E]` (a `NamedTypeAST` with `typeArgs`) and the argument type is
`Result[int, int]`:

1. Check the outer name matches (`Result` == `Result`).
2. For each type-arg pair, recursively unify.
3. Record the resulting substitutions.

Pseudo-code:

```scala
def unify(param: Type, arg: Type, subst: Map[String, Type]): Map[String, Type] =
  (param, arg) match
    case (TypeVar(name), concrete) =>
      subst + (name -> concrete)  // with conflict check
    case (Named(n1, args1), Named(n2, args2)) if n1 == n2 && args1.length == args2.length =>
      args1.zip(args2).foldLeft(subst)((s, pair) => unify(pair._1, pair._2, s))
    // ... slices, refs, pointers, arrays — all recursive
```

This is a standard algorithm (Hindley-Milner-style first-order unification)
and is ~30 lines of code once the AST shape is clear.

### Workaround (not recommended)

Explicit type arguments at call sites (generics Phase 3):

```sysl
assert(is_ok[int, int](r), "should be ok")
```

This works around the inference gap but makes every call verbose. It's the
wrong shape for a stdlib API — users should not have to write type arguments
the analyzer can see for itself.

---

## Issue 2 — Simple enums cannot be used as distinct types

### Symptom

```sysl
enum TestError
    NotFound
    TooLarge
    Empty

fn foo() -> Result[int, TestError] = Err(NotFound)
```

**Error:**

```
unknown type: 'TestError'
```

### Root cause

Sysl classifies enums into two categories:

1. **Simple enums** — all variants have no data. Members are plain `i32` values.
   Accessed as `Color.Red`.
2. **Data enums** (tagged unions) — at least one variant has data. Distinct
   type; members constructed with `Circle(5)` or `Empty`.

`TestError` has no variants with data, so it's classified as a simple enum.
Simple enums apparently do not register as usable **types** — only as
namespaces for integer constants. They cannot be referenced in type positions
like `Result[int, TestError]`.

### Impact

Common error-type patterns — a closed set of named error variants without
payload — fall into this trap. Users want:

```sysl
enum ParseError
    Empty
    BadDigit
    Overflow
```

To use as `Result[i64, ParseError]`. Currently impossible.

### Workaround (used in this session)

Use integer constants instead of an enum:

```sysl
val ERR_EMPTY: int = 1
val ERR_BAD_DIGIT: int = 2
val ERR_OVERFLOW: int = 3

fn parse(s: string) -> Result[i64, int]
```

This works but loses type distinction — any `int` is a valid "error code",
and the set of valid codes is not enforced by the type system.

### Recommended fix

Promote simple enums to distinct types. Two approaches:

**Option A (minimum change):** Treat a simple enum `TestError` as a newtype
wrapping `i32`. It can be used as a type (`Result[int, TestError]` works);
construction is via the qualified `TestError.NotFound` OR bare `NotFound`;
matching works the same as data enums.

**Option B (unify enum kinds):** Eliminate the simple/data-enum distinction
entirely. All enums are tagged unions; no-data variants take zero bytes of
payload. Simpler mental model, consistent syntax, no trap. Cost: simple
integer-enum access (`Color.Red` as an `i32` constant) becomes a variant
construction; may affect interop or size-sensitive code.

Option A is the incremental fix; Option B is the long-term cleanup.

---

## State of `std.result`

The module file `std/result/result.lsysl` has been written with:

- `enum Result[T, E]` declaration — **works**.
- Utility functions: `is_ok`, `is_err`, `unwrap`, `expect`, `unwrap_or`,
  `unwrap_err` — **blocked by Issue 1**.
- 16 tests covering construction, inspection, `?` propagation, chained `?`,
  and `unwrap_or` fallbacks — **blocked by Issue 1**.

The tests were originally written against a `TestError` enum (**Issue 2**) and
rewritten to use `int` error codes. Even after that workaround, the utility
functions cannot be called due to Issue 1.

### Options going forward

1. **Fix Issue 1 in the analyzer**, unblock the full `std.result` module. This
   is the right path for the stdlib. A ~30-line change enables every generic
   container API from here on (Result, Option, Vec, Map, etc.).

2. **Ship a minimal `std.result`** with just the `enum Result[T, E]` declaration.
   Users get the type and can use `?` / `match`, but no utility functions.
   This is trivial but leaves the module feeling incomplete.

3. **Do not ship `std.result` yet.** Hold until Issue 1 is fixed.

**Recommendation: 1.** The inference gap will bite every future generic stdlib
module. Fix it once, cleanly, and every subsequent module benefits.

---

## Related language findings from this session

While building `std.result`, we also confirmed that:

- `?` operator works correctly on `Result[T, E]` and `Option[T]` — good.
- `match` expressions work on generic enums — good.
- `()` (unit literal) is not a valid match-arm body — caused test rewrites.
- `_` as a discard binding works in single-value `val _ = expr` but **not** in
  tuple destructuring `val _, ok = fn()` — noted elsewhere.

---

## Appendix — verbatim failing test

```sysl
module std.result

enum Result[T, E]
    Ok(value: T)
    Err(error: E)

is_ok[T, E](r: Result[T, E]) -> bool
    r match
        Ok(_) -> true
        Err(_) -> false

#test
test_is_ok_on_ok() -> void
    val r: Result[int, int] = Ok(42)
    assert(is_ok(r), "Ok is ok")    // ← fails here
```

Error:

```
cannot infer type parameter 'T' for generic function 'is_ok'
```

Expected: succeeds, instantiates `is_ok_int_int`, test passes.
