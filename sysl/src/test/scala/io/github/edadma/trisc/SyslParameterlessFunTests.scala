package io.github.edadma.trisc

class SyslParameterlessFunTests extends SyslTestHelpers {

  // ===== Parameterless function declarations (`f -> T = body`) =====
  //
  // Mirrors Scala's `def foo` (parameterless) vs `def bar()` (zero-arg)
  // distinction. Useful for "computed value" patterns — parser
  // combinators in particular benefit, since `expr_p` reads as a thing,
  // not a call. Internally a parameterless function is identical to a
  // zero-arg one (storage type `() -> T`, mangled the same way); the
  // only difference is at the use site, where bare `name` auto-calls.
  //
  // Same auto-call mechanism `def` already uses — `autoCallsBare` on
  // FunInfo is `isDef || isParameterless`. Difference from `def`:
  // parameterless is NOT implicitly pure; the body can have side effects.

  "parameterless function called by bare name" in {
    eval(
      """seven -> int = 7
        |main() -> int = seven
        |""".stripMargin) shouldBe 7
  }

  "parameterless function references another parameterless function" in {
    eval(
      """six -> int = 6
        |seven -> int = six + 1
        |main() -> int = seven
        |""".stripMargin) shouldBe 7
  }

  "zero-arg function with `()` still works alongside" in {
    eval(
      """seven() -> int = 7
        |main() -> int = seven()
        |""".stripMargin) shouldBe 7
  }

  "parameterless and zero-arg with same name is rejected" in {
    val ex = intercept[Exception] {
      eval(
        """foo -> int = 1
          |foo() -> int = 2
          |main() -> int = 0
          |""".stripMargin)
    }
    val msg = ex.getMessage.toLowerCase
    assert(msg.contains("redefin") || msg.contains("conflict") || msg.contains("duplicate"),
      s"redefinition should error, got: ${ex.getMessage}")
  }

  "parameterless with block body" in {
    eval(
      """factorial_5 -> int
        |    var n = 1
        |    for i in 1..<6 do n = n * i
        |    n
        |main() -> int = factorial_5
        |""".stripMargin) shouldBe 120
  }

  "parameterless with side effects (not implicitly pure unlike def)" in {
    // Parameterless is just sugar for "zero-arg with bare-name call site".
    // Unlike `def`, the body can mutate global state — there's no implicit
    // purity. (`def` is already auto-called, so this distinction matters
    // only because parameterless declarations don't pay the def-purity tax.)
    eval(
      """var counter = 0
        |
        |bump_and_get -> int
        |    counter = counter + 1
        |    counter
        |
        |main() -> int
        |    val a = bump_and_get
        |    val b = bump_and_get
        |    counter * 100 + a + b
        |""".stripMargin) shouldBe 203 // counter=2, a=1, b=2
  }

  // ===== Negative — generic parameterless rejected =====
  //
  // A generic parameterless function has no call site to fix the type
  // args (`generic_thing[int]` would just be a type-arg, no call). So
  // we reject at decl time.

  "regression: generic parameterless function is rejected" in {
    val ex = intercept[Exception] {
      eval(
        """thing[A] -> int = 0
          |main() -> int = 0
          |""".stripMargin)
    }
    val msg = ex.getMessage.toLowerCase
    assert(msg.contains("generic") || msg.contains("parameterless") || msg.contains("expected"),
      s"generic parameterless should be rejected, got: ${ex.getMessage}")
  }

  // ===== Regression: zero-arg semantics unchanged =====

  "regression: zero-arg `f()` is NOT auto-called by bare name" in {
    // The function-as-value path: `f` (no parens) for a zero-arg `f()`
    // produces a TFuncRef, not an auto-call. Used for first-class
    // function values.
    eval(
      """seven() -> int = 7
        |
        |main() -> int
        |    val f = seven   // function value, no call
        |    f()             // explicit call
        |""".stripMargin) shouldBe 7
  }
}
