package io.github.edadma.trisc

class SyslMultilineValRhsTests extends SyslTestHelpers {

  // ===== Multi-line `val`/`var` initializer =====
  //
  // The bug: `val x: T =` followed by a Newline + Indent then the value on the
  // next line failed to parse. The parser surfaced misleading "( expected"
  // diagnostics — closureExpr is the first alternative of `expr`, and after
  // an unexpected Newline it was looking for the multi-param-closure opening
  // paren. The fix introduces a `valRhs` helper that accepts either an inline
  // expression (`= expr`) or an indented expression on the next line
  // (`=` ⏎ Indent expr Dedent), the same way function bodies accept both
  // `= expr` and `= ⏎ Indent stmts Dedent`.
  //
  // The user-reported repro pinned this on "fn-type as generic type arg in
  // declared-type position", but every fn-type-as-arg test from
  // SyslFnTypeArgTests already passed — the symptom was actually the
  // multi-line val syntax. This change covers that.

  // ===== Multi-line val with type annotation =====

  "val with type annotation, value on next line" in {
    eval(
      """f(x: int) -> int = x + 1
        |
        |main() -> int
        |    val v: int =
        |        f(41)
        |    v
        |""".stripMargin) shouldBe 42
  }

  "val with fn-type-as-generic-arg ascription, value on next line" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[(int, int) -> int] = Parser[(int, int) -> int]((a: int) -> a)
        |
        |main() -> int
        |    val v: Parser[(int, int) -> int] =
        |        make()
        |    v(7)
        |""".stripMargin) shouldBe 7
  }

  "var with type annotation, value on next line" in {
    eval(
      """main() -> int
        |    var n: int =
        |        100 + 23
        |    n - 81
        |""".stripMargin) shouldBe 42
  }

  // ===== Multi-line val without type annotation =====

  "val without type annotation, value on next line" in {
    eval(
      """f(x: int) -> int = x * 2
        |
        |main() -> int
        |    val v =
        |        f(21)
        |    v
        |""".stripMargin) shouldBe 42
  }

  // ===== Multi-line works with bigger / more deeply-indented expressions =====

  "val with multi-line if-then-else as RHS" in {
    eval(
      """main() -> int
        |    val n =
        |        if 5 > 3 then 99 else 0
        |    n
        |""".stripMargin) shouldBe 99
  }

  "val with deeply-indented call expression on next line" in {
    eval(
      """add3(a: int, b: int, c: int) -> int = a + b + c
        |
        |main() -> int
        |    val v =
        |        add3(10, 20, 12)
        |    v
        |""".stripMargin) shouldBe 42
  }

  // ===== Assignment (not just initialization) supports multi-line RHS too =====

  "plain assignment, value on next line" in {
    eval(
      """f(x: int) -> int = x * 3
        |
        |main() -> int
        |    var v = 0
        |    v =
        |        f(14)
        |    v
        |""".stripMargin) shouldBe 42
  }

  // ===== Regressions: single-line forms still parse =====

  "regression: single-line val with type annotation" in {
    eval(
      """main() -> int
        |    val v: int = 42
        |    v
        |""".stripMargin) shouldBe 42
  }

  "regression: single-line val without type annotation" in {
    eval(
      """main() -> int
        |    val v = 42
        |    v
        |""".stripMargin) shouldBe 42
  }

  "regression: single-line var with type annotation" in {
    eval(
      """main() -> int
        |    var n: int = 42
        |    n
        |""".stripMargin) shouldBe 42
  }
}
