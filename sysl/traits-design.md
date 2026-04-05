# Sysl Traits — Design Document

Status: draft
Scope: language design for the traits/interfaces feature

## 1. Goals

Provide a trait system that:

- Supports generic programming with clear constraints (`fn sort[T: Ord]`).
- Carries semantic intent and laws, not just method signatures (`Eq`, `Ord`, `Hash`).
- Integrates naturally with ARC, sum types, and the forthcoming generics.
- Keeps the language small and teachable, in the Go-influenced "boring by design" spirit.
- Stays implementable with monomorphization-style codegen on the TRISC backend.

Non-goals:

- Higher-kinded types.
- Higher-ranked trait bounds / lifetimes.
- Specialization.
- Trait objects with complex coherence rules across crates (Sysl has a central ecosystem).

## 2. Summary

Sysl adopts **nominal traits with default method implementations**, and explicitly declared `impl` blocks. There is **no orphan rule**: any `impl` may be written anywhere in the program, because Sysl's ecosystem is centralized (stdlib + compiler + OS in one tree).

A possible second, lighter feature — **structural (duck-typed) interfaces** — is deferred to a later phase. It is scoped to I/O-like one-method shapes (`Reader`, `Writer`, `Closer`) and is not required for the initial stdlib.

## 3. Why Nominal

Considered alternatives:

| Option | Chosen? |
|---|---|
| Structural / duck (Go-style) | No |
| Nominal (Rust/Swift/Haskell-style) | **Yes** |
| Hybrid (Scala 3) | Deferred |

Nominal wins for Sysl because:

- **Laws matter.** `Eq` is reflexive, `Ord` is total, `Hash` is consistent with `Eq`. Nominal traits let these contracts be documented and tested; structural typing reduces them to "has a method named X".
- **Marker traits.** `Copy`, `Send`, `Drop`-style hooks are zero-method traits that only make sense nominally.
- **Better error messages.** `Foo does not implement Ord` beats `method less_than not found on Foo`.
- **Associated types.** `Iterator::Item`, `Add::Output` are natural in a nominal system, awkward structurally.
- **Coherence by construction.** One canonical impl per (type, trait) pair avoids ambiguity in generic dispatch.
- **`?` operator needs `From[E]` error conversion** — inherently nominal.

The orphan rule is omitted because it solves a decentralized-ecosystem problem that Sysl doesn't have.

Structural interfaces are still useful (I/O shape polymorphism, callback-style APIs), and may be added later as a **scoped** second mechanism. Introducing them first would make nominal traits harder to add cleanly; nominal-first is the correct ordering.

## 4. Trait Declaration

```
trait Ord[T]:
  fn cmp(a: T, b: T) -> Ordering           // required

  fn lt(a: T, b: T) -> bool =               // default
    cmp(a, b) == Ordering.Less

  fn le(a: T, b: T) -> bool =
    cmp(a, b) != Ordering.Greater

  fn gt(a: T, b: T) -> bool =
    cmp(a, b) == Ordering.Greater

  fn ge(a: T, b: T) -> bool =
    cmp(a, b) != Ordering.Less
```

Rules:

- A trait contains **method signatures** and optional **default bodies**.
- A method without a body is **required**; implementers must provide it.
- A method with a body is **provided**; implementers may override it but are not required to.
- Traits are **stateless** — no fields. (Avoids diamond inheritance, initialization order, and memory layout issues.)

Default bodies may call other methods of the same trait, including required ones. This is what makes minimal implementations feasible: define `cmp`, get `lt`/`le`/`gt`/`ge` for free.

## 5. Implementation (`impl` blocks)

```
impl Ord[i64]:
  fn cmp(a: i64, b: i64) -> Ordering =
    if a < b then Ordering.Less
    else if a > b then Ordering.Greater
    else Ordering.Equal
```

Rules:

- An `impl` block binds one trait to one concrete type.
- All **required** methods must be implemented.
- Any **provided** method may be overridden; otherwise the default is used.
- `impl` blocks may appear in any module. There is no orphan rule.
- Duplicate `impl`s for the same (trait, type) pair are a compile-time error.

## 6. Type Parameters on Traits

Traits may themselves take type parameters, producing families of traits:

```
trait From[T]:
  fn from(x: T) -> Self

impl From[i32] for i64:
  fn from(x: i32) -> i64 = x as i64

impl From[str] for Error:
  fn from(s: str) -> Error = Error { message: s }
```

`From[i32]` and `From[str]` are distinct traits; `Error` may implement many `From[T]` at once. This is exactly the mechanism needed for error conversion in `?`:

```
// Result<T, E1> auto-converts to Result<T, E2> when E2: From[E1]
```

Other canonical examples: `Add[Rhs]`, `Mul[Rhs]`, `Into[T]`, `TryFrom[T]`, `PartialEq[Other]`.

## 7. Generic Methods on Traits

Trait methods may introduce their own type parameters:

```
trait Iterator:
  type Item
  fn next(self) -> Option[Item]

  fn map[U](self, f: fn(Item) -> U) -> Map[Self, U] = ...
  fn fold[B](self, init: B, f: fn(B, Item) -> B) -> B = ...
```

The caller picks these types at each call site. Implementers do not fix them.

## 8. Associated Types

Traits may declare associated types the impl must define:

```
trait Iterator:
  type Item
  fn next(self) -> Option[Item]

impl Iterator for Range:
  type Item = i64
  fn next(self) -> Option[i64] = ...

impl Iterator for Chars:
  type Item = rune
  fn next(self) -> Option[rune] = ...
```

**Guidance:** prefer associated types when each (Self, Trait) pair has exactly one natural output type (`Iterator::Item`, `Add::Output`). Use trait type parameters when one type needs multiple impls of the "same" trait (`From[i32]`, `From[str]`).

Associated types may carry bounds:

```
trait Iterator:
  type Item: Eq              // later: allows default methods that use ==
  fn next(self) -> Option[Item]
```

## 9. Trait Bounds in Generics

```
fn sort[T: Ord](s: []T) = ...

fn max[T: Ord](a: T, b: T) -> T =
  if Ord.gt(a, b) then a else b

fn print_all[T: Display](items: []T) = ...
```

Multiple bounds use `+`:

```
fn dedup_sort[T: Ord + Eq](s: []T) = ...
```

## 10. Method Dispatch

**Monomorphization by default.** When a generic function is called, the compiler generates a specialized copy for each concrete type. Trait method calls resolve statically.

**No dynamic dispatch (trait objects) in v1.** Can be added later as `dyn Trait` / `&dyn Trait` if needed for heterogeneous collections. Most uses can wait.

## 11. Syntax for Associated Type Access

Chosen form: `I::Item`. (Final syntax TBD; alternatives `I.Item`, `I#Item`.)

Example:

```
fn sum[I: Iterator](it: I) -> I::Item
  where I::Item: Add[I::Item, Output = I::Item] = ...
```

## 12. Callable / Function Traits

Closures and function pointers implement a built-in callable trait implicitly. Users do not write `impl` blocks for lambdas. This gives Go-style "pass a function" ergonomics within a nominal system.

```
trait Fn[Args, Output]:
  fn call(self, args: Args) -> Output
```

(Exact form TBD; may mirror Rust's `Fn`/`FnMut`/`FnOnce` split if ARC ownership makes it necessary.)

## 13. Core Traits (Starter Set)

Initial traits to define in `std`:

- `Eq` — reflexive equality. Required: `eq`. Provided: `ne`.
- `Ord` — total ordering. Required: `cmp`. Provided: `lt`, `le`, `gt`, `ge`.
- `Hash` — consistent hashing. Required: `hash(self, hasher: &Hasher)`.
- `Clone` — explicit deep copy. Required: `clone`.
- `Copy` — marker: value is trivially copyable (no ARC bump).
- `Display` — user-facing formatting. Required: `write_to(self, w: &Writer)`. Provided: `to_string`.
- `Debug` — developer-facing formatting. Same shape as `Display`.
- `From[T]` / `Into[T]` — type conversion. `Into` is auto-derived from `From`.
- `Iterator` — stream of values. Required: `next`. Provided: `map`, `filter`, `fold`, `count`, `collect`.
- `Add[Rhs]`, `Sub[Rhs]`, `Mul[Rhs]`, `Div[Rhs]` — arithmetic operators with associated `Output`.

## 14. Staging Plan

Implement in this order:

1. **Trait declarations with required + default methods** on concrete types (no generics yet).
2. **`impl` blocks**, duplicate detection, required-method checking.
3. **Trait bounds in generic functions** (`fn f[T: Trait]`) — piggybacks on generics work already underway.
4. **Trait type parameters** (`trait From[T]`).
5. **Associated types** (`type Item`).
6. **Bounds on associated types** (`type Item: Ord`).
7. **Callable traits** for closures/function pointers.
8. (Later) **Structural interfaces** for I/O shape polymorphism.
9. (Later, if needed) **Trait objects / `dyn Trait`** for dynamic dispatch.

## 15. Non-Features (Explicitly Out of Scope)

- Higher-kinded types.
- Higher-ranked trait bounds.
- Specialization.
- Trait inheritance with state.
- Orphan rule / coherence across crates.
- Associated constants (deferred; can be added if concrete need appears).
- Multiple trait implementations for the same (trait, type) pair under different "instances".

## 16. Open Questions

- Exact syntax for associated type access (`::` vs `.` vs `#`).
- Whether `Fn` needs a three-way split (`Fn`/`FnMut`/`FnOnce`) given ARC semantics.
- Syntax for `where` clauses on associated-type bounds.
- Whether `impl Trait for Type` and `impl Type` (inherent methods) share a block form or are separate.
- Auto-derive mechanism: attribute (`@derive(Eq, Ord)`) vs keyword vs manual only for v1.
