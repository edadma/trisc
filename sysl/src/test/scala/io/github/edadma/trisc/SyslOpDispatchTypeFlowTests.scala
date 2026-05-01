package io.github.edadma.trisc

class SyslOpDispatchTypeFlowTests extends SyslTestHelpers {

  // ===== Operator dispatch must do the same type-flow work as ordinary calls =====
  //
  // Two distinct shapes of failure, same root cause: the operator-dispatch path
  // skipped two pieces of plumbing that ordinary call sites already have.
  //
  // (A) Lattice-aware impl matching. The unifier's nominal-alias branch was
  //     keyed off a `genericAliasToTemplate` cache that maps mangled name →
  //     concrete type args. `typeToMangled` drops effect annotations on
  //     `FuncType`, so `Parser[(int, int) -> int]` and
  //     `Parser[(int, int) -> int #pure]` collide on the same mangled name and
  //     clobber each other in the cache. Whichever instantiation is cached
  //     last wins, and the unifier picks the wrong binding for `[A]`. Fix:
  //     after the cache lookup, also unify the alias's expanded `target`
  //     against `arg.underlying` so per-instance bindings — including effect
  //     annotations — are recovered directly from the type structure.
  //
  // (B) Expected-type forwarding into operator operands. Ordinary call sites
  //     analyze each arg with `currentExpected = formalParam.typ`, so a
  //     closure-literal placeholder (`(_ + _)`) can resolve. Operator
  //     dispatch analyzed both operands with no expected context, so a
  //     placeholder RHS failed *before* dispatch could pick an impl. Fix:
  //     before analyzing the RHS, do a lookahead — match the operator's trait
  //     impls against the LHS (and the outer expected return type when
  //     present), and if exactly one impl matches, push its 2nd-formal-param
  //     type as the RHS's `currentExpected`.

  // ===== Reproducer A — impl matching modulo the effect lattice =====

  "Or[Parser[A]] dispatches when A is a function type produced by ^^^" in {
    eval(
      """type Parser[A] = new (int) -> A
        |
        |trait Or[T]
        |    #operator("|")
        |    or_op(a: T, b: T) -> T
        |
        |impl[A] Or[Parser[A]]
        |    or_op(a: Parser[A], b: Parser[A]) -> Parser[A] = a
        |
        |trait MapTo[A, V, R]
        |    #operator("^^^")
        |    pmapto(a: A, v: V) -> R
        |
        |impl[A, B] MapTo[Parser[A], B, Parser[B]]
        |    pmapto(a: Parser[A], v: B) -> Parser[B] =
        |        Parser[B]((_x: int) -> v)
        |
        |p_int() -> Parser[int] = Parser[int]((_x: int) -> 0)
        |
        |add_p() -> Parser[(int, int) -> int] =
        |    p_int() ^^^ ((a: int, b: int) -> a + b)
        |
        |sub_p() -> Parser[(int, int) -> int] =
        |    p_int() ^^^ ((a: int, b: int) -> a - b)
        |
        |main() -> int
        |    val _ = add_p() | sub_p()
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ===== Reproducer B — placeholder closure as the second arg of `^^^` =====

  "operator-dispatch forwards expected type to closure-literal placeholder" in {
    eval(
      """type Parser[A] = new (int) -> A
        |
        |trait MapTo[A, V, R]
        |    #operator("^^^")
        |    pmapto(a: A, v: V) -> R
        |
        |impl[A, B] MapTo[Parser[A], B, Parser[B]]
        |    pmapto(a: Parser[A], v: B) -> Parser[B] =
        |        Parser[B]((_x: int) -> v)
        |
        |p_int() -> Parser[int] = Parser[int]((_x: int) -> 0)
        |
        |add_p() -> Parser[(int, int) -> int] =
        |    p_int() ^^^ (_ + _)
        |
        |main() -> int
        |    val f = add_p()
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ===== Single-arg `^^` placeholder also picks up the expected slot =====

  "operator-dispatch forwards expected type to single-arg `^^`" in {
    eval(
      """type Parser[A] = new (int) -> A
        |
        |trait Map[A, F, R]
        |    #operator("^^")
        |    pmap(a: A, f: F) -> R
        |
        |impl[A, B] Map[Parser[A], (A) -> B, Parser[B]]
        |    pmap(a: Parser[A], f: (A) -> B) -> Parser[B] =
        |        Parser[B]((_x: int) -> f(_x))
        |
        |p_int() -> Parser[int] = Parser[int]((_x: int) -> 41)
        |
        |inc_p() -> Parser[int] =
        |    p_int() ^^ (_ + 1)
        |
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  // ===== Targeted regressions for (A) — phantom + collision boundaries =====

  "(A) phantom type parameter still binds via cache even when underlying is non-generic" in {
    // `type Box[T] = new int` — `T` doesn't appear in the underlying. The
    // expand-and-unify path can't recover T from the underlying (there's no
    // T-position there); the cache lookup is the only source of binding info.
    // This regression locks in that the cache path still fires.
    eval(
      """type Box[T] = new int
        |
        |trait Show[T]
        |    showInt(x: T) -> int
        |
        |impl[A] Show[Box[A]]
        |    showInt(x: Box[A]) -> int = int(x) * 10
        |
        |main() -> int
        |    var b = Box[bool](7)
        |    Show.showInt(b)
        |""".stripMargin) shouldBe 70
  }

  "(A) Parser[fn] | Parser[fn] with explicit closure annotations still ok" in {
    // Control case: same shape as the parsyl repro but the function-typed
    // value is built with an outer constructor that doesn't go through the
    // closure-literal auto-#pure path. Confirms the lattice merge isn't
    // accidentally rejecting the all-Unknown case.
    eval(
      """type Parser[A] = new (int) -> A
        |
        |trait Or[T]
        |    #operator("|")
        |    or_op(a: T, b: T) -> T
        |
        |impl[A] Or[Parser[A]]
        |    or_op(a: Parser[A], b: Parser[A]) -> Parser[A] = a
        |
        |p_fn() -> Parser[(int, int) -> int] =
        |    Parser[(int, int) -> int]((_x: int) -> ((a: int, b: int) -> a + b))
        |
        |main() -> int
        |    val _ = p_fn() | p_fn()
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ===== Targeted regressions for (B) — lookahead must not over-fire =====

  "(B) lookahead returns None when no impl matches LHS — falls back to built-in arithmetic" in {
    // 1 + 2 has no user-type operand, so the lookahead returns None; built-in
    // arithmetic still works. Ordinary regression that the new path doesn't
    // intercept primitive-only operators.
    eval("""main() -> int = 1 + 2 * 3""".stripMargin) shouldBe 7
  }

  "(B) lookahead returns None when several impls match LHS (ambiguity preserved)" in {
    // Two impls both match `Parser[int]` at the LHS slot; the lookahead
    // refuses to commit to either, so the RHS analyzes with no special
    // expected context. With an explicit-typed closure RHS the eventual
    // dispatch picks one impl on its own — the lookahead is only an *aid*,
    // not a commitment.
    eval(
      """type Parser[A] = new (int) -> A
        |
        |trait MapTo[A, V, R]
        |    #operator("^^^")
        |    pmapto(a: A, v: V) -> R
        |
        |impl[A] MapTo[Parser[A], int, Parser[int]]
        |    pmapto(a: Parser[A], v: int) -> Parser[int] =
        |        Parser[int]((_x: int) -> v)
        |
        |impl[A] MapTo[Parser[A], bool, Parser[bool]]
        |    pmapto(a: Parser[A], v: bool) -> Parser[bool] =
        |        Parser[bool]((_x: int) -> v)
        |
        |p_int() -> Parser[int] = Parser[int]((_x: int) -> 0)
        |
        |main() -> int
        |    val q: Parser[int] = p_int() ^^^ 7
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ===== Acceptance: parsyl-flavored op-table compiles end-to-end =====

  "parsyl-style chainl1 op-table builds with placeholder closures" in {
    // The motivating end-to-end shape: `op_p() | op_p()` with each op_p
    // built as `lex(...) ^^^ (_ + _)`. Both fixes must hold simultaneously.
    eval(
      """type Parser[A] = new (int) -> A
        |
        |trait Or[T]
        |    #operator("|")
        |    or_op(a: T, b: T) -> T
        |
        |impl[A] Or[Parser[A]]
        |    or_op(a: Parser[A], b: Parser[A]) -> Parser[A] = a
        |
        |trait MapTo[A, V, R]
        |    #operator("^^^")
        |    pmapto(a: A, v: V) -> R
        |
        |impl[A, B] MapTo[Parser[A], B, Parser[B]]
        |    pmapto(a: Parser[A], v: B) -> Parser[B] =
        |        Parser[B]((_x: int) -> v)
        |
        |lit() -> Parser[int] = Parser[int]((_x: int) -> 0)
        |
        |add_op_p() -> Parser[(int, int) -> int] = lit() ^^^ (_ + _)
        |sub_op_p() -> Parser[(int, int) -> int] = lit() ^^^ (_ - _)
        |mul_op_p() -> Parser[(int, int) -> int] = lit() ^^^ (_ * _)
        |div_op_p() -> Parser[(int, int) -> int] = lit() ^^^ (_ / _)
        |
        |main() -> int
        |    val _ = add_op_p() | sub_op_p()
        |    val _2 = mul_op_p() | div_op_p()
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ===== Custom-operator dispatch must fire even when both operands are
  //       built-in types =====
  //
  // Earlier the dispatcher gated on `hasUserType` for *all* operators —
  // intended to keep `1 + 2` from going through trait machinery. But that
  // gate is wrong for *custom* operators (registered via `#operator(...)`):
  // those have no built-in fallback to preserve. With the gate in place,
  // `"+" ^^^ (_ + _)` failed at the dispatcher itself: even after the
  // closure-RHS resolved to `(int, int) -> int` via expected-type forwarding
  // from the impl's `R = Parser[B]` slot, the dispatcher silently returned
  // `None` because both operands were built-in (`string`, `fn`). Fix: gate
  // `hasUserType` only when the op IS a built-in (`+`, `-`, …); otherwise,
  // always go through dispatch.

  "MapTo with string LHS infers B from expected result type via R = Parser[B]" in {
    eval(
      """type Parser[A] = new (int) -> A
        |
        |trait MapTo[A, V, R]
        |    #operator("^^^")
        |    pmapto(a: A, v: V) -> R
        |
        |impl[B] MapTo[string, B, Parser[B]]
        |    pmapto(s: string, v: B) -> Parser[B] =
        |        Parser[B]((_x: int) -> v)
        |
        |make() -> Parser[(int, int) -> int] = "+" ^^^ (_ + _)
        |
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  "custom op on `(string, int)` dispatches when single impl matches" in {
    // Same fix exercised from a different angle: both operands built-in,
    // single matching impl, no closure-placeholder involvement.
    eval(
      """trait Tag[A, V, R]
        |    #operator("^^^")
        |    tag(a: A, v: V) -> R
        |
        |impl Tag[string, int, int]
        |    tag(a: string, v: int) -> int = v * 10
        |
        |main() -> int = "label" ^^^ 7
        |""".stripMargin) shouldBe 70
  }

  "no expected result type still rejects placeholder closure" in {
    // Negative — without a context that pins B, the `(_ + _)` placeholder
    // closure has nothing to resolve _ph0 against, and the analyzer must
    // still error. The fix permits dispatch on built-in-typed operands; it
    // must not paper over genuine "no expected type" failures upstream.
    val ex = intercept[Exception] {
      eval(
        """type Parser[A] = new (int) -> A
          |
          |trait MapTo[A, V, R]
          |    #operator("^^^")
          |    pmapto(a: A, v: V) -> R
          |
          |impl[B] MapTo[string, B, Parser[B]]
          |    pmapto(s: string, v: B) -> Parser[B] =
          |        Parser[B]((_x: int) -> v)
          |
          |main() -> int
          |    val _ = "+" ^^^ (_ + _)
          |    0
          |""".stripMargin)
    }
    val msg = ex.getMessage.toLowerCase
    assert(msg.contains("infer") || msg.contains("type"),
      s"placeholder closure with no expected type should still error, got: ${ex.getMessage}")
  }

  "regression: `1 + 2` still uses built-in arithmetic, not Add dispatch" in {
    // Built-in operators must keep the hasUserType gate — `+` on (i32, i32)
    // should never go through trait dispatch even if some `impl Add[i32]`
    // exists somewhere. Otherwise primitive arithmetic regresses.
    eval("""main() -> int = 1 + 2 * 3""".stripMargin) shouldBe 7
  }

  // ===== `expectedTypeForBinaryOpRhs` post-check parity with the dispatcher =====
  //
  // `unifyTypes` is silently no-op on shape mismatches (case `_ => ()` arms);
  // its callers post-validate via `latticeEqual`. Ordinary dispatch through
  // `tryUnifyAll` does the post-check; the closure-RHS lookahead
  // `expectedTypeForBinaryOpRhs` originally did not. With two `MapTo` impls
  // registered (`MapTo[Parser[A], …]` and `MapTo[string, …]`), `string` LHS
  // would non-deterministically "unify" against the Parser pattern (no throw),
  // then both candidates returned `Some(...)`, the lookahead aggregated to
  // `None` (ambiguous), and the placeholder RHS had no expected type.
  //
  // Fix: do the same `latticeEqual(resolved, actual)` post-check the
  // dispatcher does, before resolving the second-param pattern. With it, the
  // Parser-pattern candidate is correctly dropped on `string` LHS, the
  // string-pattern candidate is the unique winner, and `B` is bound from
  // either the closure's eventual unify *or* the expected-result `Parser[B]`
  // slot — the placeholder RHS resolves cleanly.

  "two impls (Parser-LHS + string-LHS) — placeholder RHS dispatches with string LHS" in {
    eval(
      """type Parser[A] = new (int) -> A
        |
        |trait MapTo[A, V, R]
        |    #operator("^^^")
        |    pmapto(a: A, v: V) -> R
        |
        |impl[A, B] MapTo[Parser[A], B, Parser[B]]
        |    pmapto(a: Parser[A], v: B) -> Parser[B] =
        |        Parser[B]((_x: int) -> v)
        |
        |impl[B] MapTo[string, B, Parser[B]]
        |    pmapto(s: string, v: B) -> Parser[B] =
        |        Parser[B]((_x: int) -> v)
        |
        |make() -> Parser[(int, int) -> int] = "+" ^^^ (_ + _)
        |
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  "two impls — Parser-LHS variant still works in the placeholder shape" in {
    // Mirror case: with the same two impls, a Parser-typed LHS must select
    // the Parser-pattern impl and the placeholder RHS must still resolve.
    // Confirms the post-check rejects the wrong impl in both directions.
    eval(
      """type Parser[A] = new (int) -> A
        |
        |literal(s: string) -> Parser[string] = Parser[string]((_x: int) -> s)
        |
        |trait MapTo[A, V, R]
        |    #operator("^^^")
        |    pmapto(a: A, v: V) -> R
        |
        |impl[A, B] MapTo[Parser[A], B, Parser[B]]
        |    pmapto(a: Parser[A], v: B) -> Parser[B] =
        |        Parser[B]((_x: int) -> v)
        |
        |impl[B] MapTo[string, B, Parser[B]]
        |    pmapto(s: string, v: B) -> Parser[B] =
        |        Parser[B]((_x: int) -> v)
        |
        |make() -> Parser[(int, int) -> int] = literal("+") ^^^ (_ + _)
        |
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }
}
