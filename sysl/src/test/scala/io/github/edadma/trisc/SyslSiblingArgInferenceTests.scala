package io.github.edadma.trisc

class SyslSiblingArgInferenceTests extends SyslTestHelpers {

  // ===== Type-args inferred from arg0 propagate to placeholder closures in arg1 =====
  //
  // Background: in a generic call `f[A](a, b, ...)`, when `A` is determined by
  // the first arg's analyzed type and a later arg is a closure with placeholder
  // parameters whose types depend on `A`, placeholder inference fired *before*
  // `A` had been bound — and failed with "cannot infer type for closure
  // parameter '_phN'". Adding the explicit type-arg `f[int](...)` worked,
  // proving the only missing piece was the order of operations.
  //
  // Fix: in the `CallAST(name, args)` analysis path for generic templates,
  // analyze args left-to-right; after each arg, run a partial unification of
  // the formal-param TypeAST against the actual arg type and grow the binding
  // env; for the next arg, substitute env into its formal TypeAST and use the
  // resolved type as `currentExpected`. The closure-placeholder analyzer then
  // sees a concrete type instead of a free type parameter.

  // ===== Spec reproducer ===============================================

  "type-args inferred from arg0 propagate to placeholder closures in arg1" in {
    eval(
      """f[A](x: A, g: (A, A) -> A) -> A = g(x, x)
        |main() -> int = f(7, _ + _)
        |""".stripMargin) shouldBe 14
  }

  "explicit type-arg path still works (regression check)" in {
    eval(
      """f[A](x: A, g: (A, A) -> A) -> A = g(x, x)
        |main() -> int = f[int](7, _ + _)
        |""".stripMargin) shouldBe 14
  }

  "later arg with no inferable type still errors" in {
    val ex = intercept[Exception] {
      eval(
        """f[A](g: (A, A) -> A) -> int = 0
          |main() -> int = f(_ + _)
          |""".stripMargin)
    }
    assert(ex.getMessage.toLowerCase.contains("infer"),
      s"placeholder with no inferable expected type should still error, got: ${ex.getMessage}")
  }

  // ===== Targeted regressions for the incremental-binding path =========

  "three-arg call: A pinned by arg0, two later closure args both get expected type" in {
    eval(
      """f[A](x: A, g: (A) -> A, h: (A) -> A) -> A = h(g(x))
        |main() -> int = f(40, _ + 1, _ + 1)
        |""".stripMargin) shouldBe 42
  }

  "two type-params: both inferred incrementally from leading args" in {
    // A pinned by x:A, B pinned by y:B, then closure g: (A) -> B picks up both.
    eval(
      """f[A, B](x: A, y: B, g: (A) -> B) -> B = g(x)
        |main() -> int = f(7, 0, _ * 2)
        |""".stripMargin) shouldBe 14
  }

  "non-generic call with closure-placeholder arg unaffected" in {
    // Non-generic path uses the existing static expected-type computation;
    // the new incremental branch must not interfere.
    eval(
      """g(x: int, f: (int, int) -> int) -> int = f(x, x)
        |main() -> int = g(5, _ + _)
        |""".stripMargin) shouldBe 10
  }

  // ===== End-to-end: parsyl-style chainl1 works without explicit `[int]` =====

  "parsyl-style chainl1: A inferred from operand: Parser[A] flows into op closure" in {
    eval(
      """type Parser[A] = new (int) -> A
        |
        |chainl1[A](operand: Parser[A], op: Parser[(A, A) -> A]) -> Parser[A] =
        |    Parser[A]((_x: int) -> operand(_x))
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
        |lex_plus()  -> Parser[int] = Parser[int]((_x: int) -> 0)
        |lex_minus() -> Parser[int] = Parser[int]((_x: int) -> 0)
        |term_p()    -> Parser[int] = Parser[int]((_x: int) -> 0)
        |
        |expr_p() -> Parser[int] =
        |    chainl1(term_p(),
        |        (lex_plus() ^^^ (_ + _)) | (lex_minus() ^^^ (_ - _)))
        |
        |main() -> int
        |    val _ = expr_p()
        |    0
        |""".stripMargin) shouldBe 0
  }
}
