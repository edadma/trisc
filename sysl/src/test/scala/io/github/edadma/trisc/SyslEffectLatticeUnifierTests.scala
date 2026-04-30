package io.github.edadma.trisc

class SyslEffectLatticeUnifierTests extends SyslTestHelpers {

  // ===== Effect-lattice in the function-type unifier =====
  //
  // tryUnifyAll's post-validation used to demand strict structural equality
  // (`resolved == actual`) between the resolved impl pattern and the call-site
  // actual. That broke generic-impl dispatch on any combinator-shaped impl
  // whose pattern slot is `(A) -> B` (Unknown effects) but whose call-site
  // actual is a literal closure (always inferred `#pure` for side-effect-free
  // bodies). The fix is to compare with effect-lattice tolerance for FuncType:
  //
  //   #pure   ≤ anything
  //   reads/writes(R, W)   ≤ unannotated  OR  reads/writes(R', W') with R⊆R' W⊆W'
  //   unannotated   ≤ unannotated only
  //
  // The lattice is one-directional (actual ≤ slot) and matches the rules the
  // reference already states for indirect calls. The unifier was the only
  // remaining place using equality.

  // ===== Lattice — accept cases =====

  "pure closure into unannotated higher-order param (parsyl repro)" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |trait Map[A, F, R]
        |    #operator("^^")
        |    pmap(a: A, f: F) -> R
        |
        |success() -> Parser[int] = Parser[int]((x: int) -> x)
        |
        |impl[A, B] Map[Parser[A], (A) -> B, Parser[B]]
        |    pmap(a: Parser[A], f: (A) -> B) -> Parser[B] = a
        |
        |main() -> int
        |    val p = success() ^^ ((x: int) -> x * 2)
        |    p(21)
        |""".stripMargin) shouldBe 21
  }

  "pure closure into pure slot (direct match) — via Trait.method() form" in {
    // Trait.method() bypasses operator dispatch's user-type gate; tests just
    // the unifier's pattern match: `(A) -> B #pure` against `(int) -> int #pure`.
    eval(
      """trait Twice[T, F, R]
        |    twice(a: T, f: F) -> R
        |
        |impl[A, B] Twice[A, (A) -> B #pure, B]
        |    twice(a: A, f: (A) -> B #pure) -> B = f(a)
        |
        |main() -> int = Twice.twice(21, (x: int) -> x * 2)
        |""".stripMargin) shouldBe 42
  }

  // ===== Lattice — reject cases =====

  "unannotated function ref into pure slot is rejected" in {
    val ex = intercept[Exception] {
      eval(
        """trait Twice[T, F, R]
          |    #operator("^^")
          |    twice(a: T, f: F) -> R
          |
          |impl[A, B] Twice[A, (A) -> B #pure, B]
          |    twice(a: A, f: (A) -> B #pure) -> B = f(a)
          |
          |var counter = 0
          |
          |impure_double(x: int) -> int
          |    counter = counter + 1
          |    x * 2
          |
          |main() -> int
          |    // Building a function-typed value from an unannotated function;
          |    // the impl wants a `#pure` callable. Lattice rejects.
          |    val f: (int) -> int = impure_double
          |    val v = 21 ^^ f
          |    v
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.toLowerCase.contains("impl") || msg.toLowerCase.contains("operator") || msg.toLowerCase.contains("pure"),
      s"unannotated → pure dispatch should fail, got: $msg")
  }

  // ===== Generic dispatch with effect lattice plus other type args =====

  "two-tparam impl: pure closure inferred for tparam slot" in {
    eval(
      """trait Apply[X, F, R]
        |    apply(x: X, f: F) -> R
        |
        |impl[A, B] Apply[A, (A) -> B, B]
        |    apply(x: A, f: (A) -> B) -> B = f(x)
        |
        |main() -> int = Apply.apply(20, (n: int) -> n + 22)
        |""".stripMargin) shouldBe 42
  }

  // ===== Diagnostic / regression — non-fn-type changes still strict =====

  "regression: Vec2 + Vec2 dispatch unchanged (no fn-type involved)" in {
    eval(
      """struct Vec2
        |    x: int
        |    y: int
        |
        |trait Add[T]
        |    add(a: T, b: T) -> T
        |
        |impl Add[Vec2]
        |    add(a: Vec2, b: Vec2) -> Vec2 = Vec2(a.x + b.x, a.y + b.y)
        |
        |main() -> int
        |    val a = Vec2(1, 2)
        |    val b = Vec2(10, 20)
        |    val c = a + b
        |    c.x * 100 + c.y
        |""".stripMargin) shouldBe 1122
  }

  "regression: function-type with mismatched param-arity still rejected" in {
    val ex = intercept[Exception] {
      eval(
        """trait Apply2[X, F, R]
          |    apply2(x: X, f: F) -> R
          |
          |// Impl pattern requires a 2-param closure, call passes 1-param closure
          |impl[A, B] Apply2[A, (A, A) -> B, B]
          |    apply2(x: A, f: (A, A) -> B) -> B = f(x, x)
          |
          |main() -> int = Apply2.apply2(7, (n: int) -> n + 1)
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.toLowerCase.contains("apply2") || msg.toLowerCase.contains("impl") || msg.toLowerCase.contains("function"),
      s"arity mismatch should fail dispatch, got: $msg")
  }

  "regression: nominal-alias dispatch still works" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |trait Concat[A, B, R]
        |    #operator("~")
        |    concat(a: A, b: B) -> R
        |
        |impl[X, Y] Concat[Parser[X], Parser[Y], int]
        |    concat(a: Parser[X], b: Parser[Y]) -> int = 99
        |
        |success() -> Parser[int] = Parser[int]((inp: int) -> 1)
        |
        |main() -> int
        |    val p = success()
        |    val q = success()
        |    p ~ q
        |""".stripMargin) shouldBe 99
  }
}
