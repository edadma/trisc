package io.github.edadma.trisc

class SyslLLVMClosureTests extends SyslLLVMTestHelpers {

  "zero-capture closure" in {
    llvmExit(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(x -> x + 1, 41)
        |""".stripMargin) shouldBe 42
  }

  "multi-param closure" in {
    llvmExit(
      """apply2(f: (int, int) -> int, a: int, b: int) -> int = f(a, b)
        |
        |main() -> int = apply2((x, y) -> x + y, 20, 22)
        |""".stripMargin) shouldBe 42
  }

  "zero-param closure" in {
    llvmExit(
      """call(f: () -> int) -> int = f()
        |
        |main() -> int = call(() -> 42)
        |""".stripMargin) shouldBe 42
  }

  "closure assigned to variable" in {
    llvmExit(
      """main() -> int
        |    val f: (int) -> int = x -> x * 2
        |    f(21)
        |""".stripMargin) shouldBe 42
  }

  "capture local variable" in {
    llvmExit(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    val a = 10
        |    apply(x -> x + a, 32)
        |""".stripMargin) shouldBe 42
  }

  "capture is frozen (by value)" in {
    llvmExit(
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
    llvmExit(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    val a = 10
        |    val b = 20
        |    apply(x -> x + a + b, 12)
        |""".stripMargin) shouldBe 42
  }

  "function reference as closure" in {
    llvmExit(
      """dbl(x: int) -> int = x * 2
        |
        |apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(dbl, 21)
        |""".stripMargin) shouldBe 42
  }

  "closure as return value" in {
    llvmExit(
      """make_adder(n: int) -> (int) -> int
        |    val captured = n
        |    x -> x + captured
        |
        |main() -> int
        |    val add10 = make_adder(10)
        |    add10(32)
        |""".stripMargin) shouldBe 42
  }

  // ===== Inner def declarations (recursive named local closures) =====

  "inner def with self-recursion (factorial)" in {
    llvmExit(
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
    llvmExit(
      """outer(base: int) -> int
        |    def add_base(n: int) -> int
        |        n + base
        |    add_base(7)
        |
        |main() -> int = outer(35)
        |""".stripMargin) shouldBe 42
  }

  "inner def with self-recursion uses captured outer local" in {
    llvmExit(
      """outer(bonus: int) -> int
        |    def sum_with_bonus(n: int) -> int
        |        if n == 0 then return 0
        |        n + bonus + sum_with_bonus(n - 1)
        |    sum_with_bonus(3)
        |
        |main() -> int = outer(10)
        |""".stripMargin) shouldBe 36
  }

  "inner def with zero parameters" in {
    llvmExit(
      """outer() -> int
        |    def constant() -> int = 42
        |    constant()
        |
        |main() -> int = outer()
        |""".stripMargin) shouldBe 42
  }
}
