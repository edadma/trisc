package io.github.edadma.trisc

class SyslLambdaParamTests extends SyslTestHelpers {

  // ===== `_` as a (typed) lambda parameter =====
  //
  // Adds a fourth disjoint position for `_`, alongside discard binding (`val
  // _ = e`), pattern wildcard (`(_, b)`, match arms `_ -> ...`, struct-pattern
  // fields), and the expression-position placeholder (per 7e4f58045). In a
  // lambda parameter list, `_` introduces a fresh slot that is unreferenceable
  // from the body — multiple `_` parameters are independent.

  // ===== Bare and typed discard =====

  "typed discard parameter — single arg lambda" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply((_: int) -> 42, 7)
        |""".stripMargin) shouldBe 42
  }

  "bare discard parameter — type inferred from context" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply((_) -> 42, 7)
        |""".stripMargin) shouldBe 42
  }

  // ===== Multiple discards in one parameter list =====

  "two typed discard parameters — independent slots, no collision" in {
    eval(
      """apply2(f: (int, int) -> int, a: int, b: int) -> int = f(a, b)
        |
        |main() -> int = apply2((_: int, _: int) -> 99, 1, 2)
        |""".stripMargin) shouldBe 99
  }

  "three discards — one inferred, two typed" in {
    eval(
      """apply3(f: (int, string, bool) -> int, a: int, b: string, c: bool) -> int = f(a, b, c)
        |
        |main() -> int = apply3((_, _: string, _: bool) -> 7, 1, "x", true)
        |""".stripMargin) shouldBe 7
  }

  // ===== Mixed: named and discard =====

  "mixed named + discard — only named referenceable" in {
    eval(
      """apply2(f: (int, string) -> int, a: int, b: string) -> int = f(a, b)
        |
        |main() -> int = apply2((x: int, _: string) -> x + 100, 5, "ignored")
        |""".stripMargin) shouldBe 105
  }

  "mixed: discard first, named second" in {
    eval(
      """apply2(f: (int, int) -> int, a: int, b: int) -> int = f(a, b)
        |
        |main() -> int = apply2((_: int, y: int) -> y * 3, 99, 14)
        |""".stripMargin) shouldBe 42
  }

  // ===== Parsyl-style: `(_: A) -> v` inside a generic-alias cast =====
  //
  // The motivating case: `MapTo` for parser combinators. The closure replaces
  // the success value with a constant, ignoring its argument. Until this fix,
  // the typed discard parameter would not parse.

  "parsyl-style: typed discard in closure inside nominal-alias cast" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make(v: int) -> Parser[int] =
        |    Parser[int]((_: int) -> v)
        |
        |main() -> int
        |    val p = make(42)
        |    p(99999)
        |""".stripMargin) shouldBe 42
  }

  // ===== Single-param shorthand with `_` =====
  //
  // The bare `name -> body` lambda shorthand uses `ident`, not the parens
  // form. `_ -> body` would parse `_` as a placeholder in expression position
  // (yielding a `(_x) -> _x` lambda — see SyslPlaceholderTests). So
  // `_ -> body` is intentionally NOT a discard-param shorthand. Use `(_) ->
  // body` for that.

  "single-param `(_) -> body` works" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply((_) -> 7, 100)
        |""".stripMargin) shouldBe 7
  }

  // ===== Closure body can still use `_` as expression placeholder =====
  //
  // Discard params don't shadow the placeholder. `_` in expression position
  // inside the body still creates an inner `_x -> _x`-style lambda or behaves
  // as a partial-application slot per the existing rule.

  "discard param + nested placeholder lambda in body" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |applyNested(g: (int) -> int) -> int = g(11)
        |
        |main() -> int
        |    val outer = (_: int) -> applyNested(_ + 1)
        |    apply(outer, 999)
        |""".stripMargin) shouldBe 12
  }

  // ===== Capture / closure correctness =====
  //
  // The synthetic discard name shouldn't disturb capture analysis: a discard
  // param cannot be referenced and so cannot be captured. Other captures
  // (real outer locals) still flow through.

  "discard param doesn't disturb captures of outer locals" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    val outer = 30
        |    apply((_: int) -> outer + 12, 999)
        |""".stripMargin) shouldBe 42
  }

  // ===== Negative cases =====

  "untyped bare discard with no contextual type still errors" in {
    val ex = intercept[Exception] {
      eval(
        """main() -> int
          |    val f = (_) -> 42
          |    f(99)
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(
      msg.toLowerCase.contains("infer") || msg.toLowerCase.contains("type") || msg.toLowerCase.contains("annotation"),
      s"untyped discard with no context should fail to infer, got: $msg",
    )
  }

  // ===== Regressions: existing `_` positions still work =====

  "regression: discard binding `val _ = expr`" in {
    eval(
      """side(x: int) -> int = x + 1
        |
        |main() -> int
        |    val _ = side(10)
        |    7
        |""".stripMargin) shouldBe 7
  }

  "regression: pattern wildcard in match arm" in {
    eval(
      """classify(n: int) -> int = n match
        |    0 -> 100
        |    _ -> 99
        |
        |main() -> int = classify(7)
        |""".stripMargin) shouldBe 99
  }

  "regression: expression-position placeholder produces 1-arg lambda" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(_ + 1, 10)
        |""".stripMargin) shouldBe 11
  }
}
