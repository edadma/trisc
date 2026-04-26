package io.github.edadma.trisc

class SyslClosureTests extends SyslTestHelpers {

  // ===== Basic closures (no captures) =====

  "zero-capture closure" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(x -> x + 1, 41)
        |""".stripMargin) shouldBe 42
  }

  "multi-param closure" in {
    eval(
      """apply2(f: (int, int) -> int, a: int, b: int) -> int = f(a, b)
        |
        |main() -> int = apply2((x, y) -> x + y, 20, 22)
        |""".stripMargin) shouldBe 42
  }

  "zero-param closure" in {
    eval(
      """call(f: () -> int) -> int = f()
        |
        |main() -> int = call(() -> 42)
        |""".stripMargin) shouldBe 42
  }

  "closure assigned to variable" in {
    eval(
      """main() -> int
        |    val f: (int) -> int = x -> x * 2
        |    f(21)
        |""".stripMargin) shouldBe 42
  }

  // ===== Capture by value =====

  "capture local variable" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    val a = 10
        |    apply(x -> x + a, 32)
        |""".stripMargin) shouldBe 42
  }

  "capture is frozen (by value)" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    var a = 10
        |    val f: (int) -> int = x -> x + a
        |    a = 100
        |    f(32)
        |""".stripMargin) shouldBe 42
  }

  "capture multiple variables" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    val a = 10
        |    val b = 20
        |    apply(x -> x + a + b, 12)
        |""".stripMargin) shouldBe 42
  }

  // ===== Higher-order functions =====

  "closure passed to higher-order function" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(x -> x * 2, 21)
        |""".stripMargin) shouldBe 42
  }

  "closure as return value" in {
    eval(
      """make_adder(n: int) -> (int) -> int
        |    val captured = n
        |    x -> x + captured
        |
        |main() -> int
        |    val add10 = make_adder(10)
        |    add10(32)
        |""".stripMargin) shouldBe 42
  }

  // ===== Type-annotated parameters =====

  "closure with typed parameters" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply((x: int) -> x + 1, 41)
        |""".stripMargin) shouldBe 42
  }

  // ===== Expressions =====

  "closure in arithmetic expression" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(x -> x + 1, 20) + apply(x -> x + 1, 20)
        |""".stripMargin) shouldBe 42
  }

  // ===== Nested closure declarations =====

  "closure-typed local declared inside another closure body resolves correctly" in {
    // Regression: bare `name = (...) -> ...` inside a function body parses as
    // AssignStmtAST, which the analyzer used to lower to TAssignStmt regardless of
    // whether the name was a fresh binding. When it occurred inside another closure's
    // body, the capture-detection pass walked TAssignStmt as a write to an outer name
    // and added the freshly-created `inner` to the outer closure's capture list. At
    // runtime the outer closure tried to look up `inner` in its captured environment
    // and failed with "undefined variable: inner". Fix: emit TVarStmt for fresh-local
    // creation so capture detection sees the binding.
    eval(
      """main() -> int
        |    outer = (a: int) ->
        |        inner = (b: int) ->
        |            a + b
        |        inner(5)
        |    outer(2)
        |""".stripMargin) shouldBe 7
  }

  // ===== Inner def declarations (recursive named local closures) =====

  "inner def with self-recursion (factorial)" in {
    eval(
      """outer() -> int
        |    def fact(n: int) -> int
        |        if n == 0 then return 1
        |        n * fact(n - 1)
        |    fact(5)
        |
        |main() -> int = outer()
        |""".stripMargin) shouldBe 120
  }

  "inner def captures outer parameter" in {
    eval(
      """outer(base: int) -> int
        |    def add_base(n: int) -> int
        |        n + base
        |    add_base(7)
        |
        |main() -> int = outer(35)
        |""".stripMargin) shouldBe 42
  }

  "inner def with self-recursion uses captured outer local" in {
    // Combines self-recursion with an outer-scope capture: the recursive helper
    // sums n down to 0, adding `bonus` (captured from outer) each step.
    eval(
      """outer(bonus: int) -> int
        |    def sum_with_bonus(n: int) -> int
        |        if n == 0 then return 0
        |        n + bonus + sum_with_bonus(n - 1)
        |    sum_with_bonus(3)
        |
        |main() -> int = outer(10)
        |""".stripMargin) shouldBe 36 // (3+10) + (2+10) + (1+10) + 0 = 36
  }

  "inner def with zero parameters" in {
    eval(
      """outer() -> int
        |    def constant() -> int = 42
        |    constant()
        |
        |main() -> int = outer()
        |""".stripMargin) shouldBe 42
  }
}
