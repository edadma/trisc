package io.github.edadma.trisc

class SyslFnTypeArgTests extends SyslTestHelpers {

  // ===== Function type as generic type argument =====
  //
  // The expression-position type-argument converter (exprToTypeAST) handles
  // tuples (edba294ba) and slices/arrays (8eb7dc625). Function types are the
  // last common shape: `Parser[(int, int) -> int]`. The fix lifts a parser-
  // emitted FuncTypeAST in expression position via `TypeRefExprAST(FuncTypeAST(...))`,
  // tried before the paren-tuple / paren-group rules so the combinator
  // backtracks cleanly on a non-arrow paren expression.

  // ===== Basic shapes =====

  "fn type as type arg of nominal alias — Parser[(int, int) -> int]" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[(int, int) -> int] =
        |    Parser[(int, int) -> int]((n: int) -> n)
        |
        |main() -> int
        |    val p = make()
        |    p(99)
        |""".stripMargin) shouldBe 99
  }

  "fn type with single-arg form — Parser[(int) -> int]" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[(int) -> int] =
        |    Parser[(int) -> int]((n: int) -> n + 1)
        |
        |main() -> int
        |    val p = make()
        |    p(41)
        |""".stripMargin) shouldBe 42
  }

  "zero-arg fn type — Parser[() -> int]" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[() -> int] =
        |    Parser[() -> int]((n: int) -> n)
        |
        |main() -> int
        |    val p = make()
        |    p(7)
        |""".stripMargin) shouldBe 7
  }

  // ===== Higher-order shapes — fn returning fn =====

  "fn returning fn — Parser[(int) -> (int) -> int]" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[(int) -> (int) -> int] =
        |    Parser[(int) -> (int) -> int]((n: int) -> n * 2)
        |
        |main() -> int
        |    val p = make()
        |    p(21)
        |""".stripMargin) shouldBe 42
  }

  // ===== Composition with tuple / slice / array element types =====

  "fn type with tuple result — Parser[(int) -> (int, int)]" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[(int) -> (int, int)] =
        |    Parser[(int) -> (int, int)]((n: int) -> n)
        |
        |main() -> int
        |    val p = make()
        |    p(5)
        |""".stripMargin) shouldBe 5
  }

  "fn type with slice arg — Parser[([]int) -> int]" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[([]int) -> int] =
        |    Parser[([]int) -> int]((n: int) -> n)
        |
        |main() -> int
        |    val p = make()
        |    p(11)
        |""".stripMargin) shouldBe 11
  }

  "fn type with slice result — Parser[(int) -> []int]" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[(int) -> []int] =
        |    Parser[(int) -> []int]((n: int) -> n)
        |
        |main() -> int
        |    val p = make()
        |    p(13)
        |""".stripMargin) shouldBe 13
  }

  "fn type with array arg — Parser[([4]int) -> int]" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[([4]int) -> int] =
        |    Parser[([4]int) -> int]((n: int) -> n)
        |
        |main() -> int
        |    val p = make()
        |    p(17)
        |""".stripMargin) shouldBe 17
  }

  // ===== Nested generics with fn type at the leaf =====

  "nested generic alias with fn type leaf — Parser[Parser[(int) -> int]]" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[Parser[(int) -> int]] =
        |    Parser[Parser[(int) -> int]]((n: int) -> n)
        |
        |main() -> int
        |    val p = make()
        |    p(23)
        |""".stripMargin) shouldBe 23
  }

  "generic struct holding a fn-typed field — Box[(int) -> int]" in {
    eval(
      """struct Box[T]
        |    v: T
        |
        |inc(x: int) -> int = x + 1
        |
        |main() -> int
        |    val b = Box[(int) -> int](inc)
        |    b.v(41)
        |""".stripMargin) shouldBe 42
  }

  // ===== Parsyl-shape: chainl1 / chainr1 binary-operator combinator =====

  "parsyl-style: chainl1 takes Parser[(A, A) -> A] as op type" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |chainl1(operand: Parser[int], op: Parser[(int, int) -> int]) -> int
        |    operand(0) + op(0)
        |
        |make_op() -> Parser[(int, int) -> int] =
        |    Parser[(int, int) -> int]((n: int) -> n + 100)
        |
        |make_oper() -> Parser[int] =
        |    Parser[int]((n: int) -> n + 50)
        |
        |main() -> int
        |    val ope = make_oper()
        |    val op = make_op()
        |    chainl1(ope, op)
        |""".stripMargin) shouldBe 150
  }

  // ===== Negative — multi-param fn without parens is rejected =====

  "fn type without parens around params is rejected" in {
    val ex = intercept[Exception] {
      eval(
        """type Parser[A] = new (int) -> int
          |
          |main() -> int
          |    val p: Parser[int, int -> int] = Parser[int, int -> int]((n: int) -> n)
          |    p(1)
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.nonEmpty, s"missing parens around fn-type params should fail, got: $msg")
  }

  // ===== Regressions =====

  "regression: tuple as type arg still works — Parser[(int, int)]" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[(int, int)] =
        |    Parser[(int, int)]((n: int) -> n)
        |
        |main() -> int
        |    val p = make()
        |    p(31)
        |""".stripMargin) shouldBe 31
  }

  "regression: slice as type arg still works — Parser[[]int]" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[[]int] =
        |    Parser[[]int]((n: int) -> n)
        |
        |main() -> int
        |    val p = make()
        |    p(37)
        |""".stripMargin) shouldBe 37
  }

  "regression: array as type arg still works — Parser[[3]int]" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[[3]int] =
        |    Parser[[3]int]((n: int) -> n)
        |
        |main() -> int
        |    val p = make()
        |    p(41)
        |""".stripMargin) shouldBe 41
  }

  "regression: nested generic still works — Parser[Parser[int]]" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |make() -> Parser[Parser[int]] =
        |    Parser[Parser[int]]((n: int) -> n)
        |
        |main() -> int
        |    val p = make()
        |    p(43)
        |""".stripMargin) shouldBe 43
  }

  "regression: paren-grouped expression that isn't a fn type still parses" in {
    eval(
      """f(x: int) -> int = x + 1
        |
        |main() -> int = f((1 + 2))
        |""".stripMargin) shouldBe 4
  }

  "regression: tuple literal in expression position still parses" in {
    eval(
      """first(t: (int, int)) -> int = 0
        |
        |main() -> int
        |    val t = (1, 2)
        |    first(t)
        |""".stripMargin) shouldBe 0
  }
}
