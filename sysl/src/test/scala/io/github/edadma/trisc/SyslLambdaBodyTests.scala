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

  // ===== Match inside a lambda body inside parens =====
  //
  // The closure body's `match` would normally need Newline/Indent/Dedent for
  // its arm list, but the lexer suppresses those tokens inside parens. The
  // matchExpr parser carries an inline form that detects arms greedily by
  // pattern start (so it terminates cleanly at `)` / `,`).

  "match-with-arms in lambda body inside parens (user bug repro)" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |classify() -> Parser[int] =
        |    Parser[int]((n: int) ->
        |        n match
        |            0 -> 100
        |            1 -> 200
        |            _ -> 999)
        |
        |main() -> int
        |    val p = classify()
        |    p(1)
        |""".stripMargin) shouldBe 200
  }

  "match with destructure pattern arms in lambda body inside parens" in {
    eval(
      """enum Result
        |    Ok(v: int)
        |    Err(m: int)
        |
        |apply(f: (Result) -> int, r: Result) -> int = f(r)
        |
        |handle(r: Result, def_: int) -> int =
        |    apply((r2: Result) ->
        |        r2 match
        |            Ok(v) -> v
        |            Err(_) -> def_, r)
        |
        |main() -> int = handle(Ok(42), 0)
        |""".stripMargin) shouldBe 42
  }

  "match with else-clause inline in lambda body inside parens" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply((n: int) ->
        |    n match
        |        0 -> 100
        |        else -> n * 2,
        |    21)
        |""".stripMargin) shouldBe 42
  }

  // ===== Top-level multi-line body still works =====

  "regression: top-level indented match still works (no parens around lambda)" in {
    eval(
      """classify(n: int) -> int
        |    n match
        |        0 -> 100
        |        _ -> 42
        |
        |main() -> int = classify(7)
        |""".stripMargin) shouldBe 42
  }

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
