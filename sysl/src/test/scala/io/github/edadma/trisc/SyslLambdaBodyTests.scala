package io.github.edadma.trisc

class SyslLambdaBodyTests extends SyslTestHelpers {

  // ===== Single-line if-then-else as a lambda body =====
  //
  // This used to fail because closureBody only accepted `logicalOr`, which
  // didn't include the `if`-as-expression branch. With closureBody using
  // `expr`, it's just an inline ifExpr and works in any expression position.

  "single-line if-then-else lambda body in call arg" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply((x: int) -> if x > 0 then x else -x, -7)
        |""".stripMargin) shouldBe 7
  }

  "single-line match-expression lambda body in call arg" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |classify(n: int) -> int = n match
        |    0 -> 0
        |    _ -> 1
        |
        |main() -> int = apply((x: int) -> classify(x) + 1, 5)
        |""".stripMargin) shouldBe 2
  }

  // ===== Multi-line bodies inside parens =====
  //
  // Inside `(`/`[` the lexer suppresses Newline/Indent/Dedent (line joining).
  // What looks like a multi-line body to the user is one big expression to
  // the parser — the indentation is purely visual. The fix that makes these
  // work is: closureBody accepts full `expr`, so the body can be a multi-
  // clause if/match expression spelled across several physical lines.

  "multi-line if-else lambda body in call arg" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply((x: int) ->
        |    if x > 0 then x
        |    else -x, -7)
        |""".stripMargin) shouldBe 7
  }

  "parsyl-style: nominal-alias cast wrapping a closure with if-then-else" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |p_double() -> Parser[int] =
        |    Parser[int]((x: int) ->
        |        if x > 0 then x * 2
        |        else 0)
        |
        |main() -> int
        |    val p = p_double()
        |    p(21)
        |""".stripMargin) shouldBe 42
  }

  // ===== Mixed args =====

  "mixed: single-line lambda, multi-line lambda, plain arg" in {
    eval(
      """combine(f: (int) -> int, g: (int) -> int, x: int) -> int = f(x) + g(x) + x
        |
        |main() -> int = combine(
        |    (x: int) -> x * 2,
        |    (x: int) ->
        |        if x > 0 then x + 10
        |        else x - 10,
        |    5)
        |""".stripMargin) shouldBe 30  // (5*2) + (5+10) + 5
  }

  // ===== Top-level multi-line body still works =====

  "regression: top-level multi-line lambda body" in {
    eval(
      """main() -> int
        |    var transform: (int) -> int = x ->
        |        val doubled = x * 2
        |        doubled + 1
        |    transform(20)
        |""".stripMargin) shouldBe 41
  }

}
