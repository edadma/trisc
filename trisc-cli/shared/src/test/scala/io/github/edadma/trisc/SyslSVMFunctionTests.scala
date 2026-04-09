package io.github.edadma.trisc

class SyslSVMFunctionTests extends SyslSVMCodegenHelpers {

  "function call" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |
        |main() -> int = dbl(21)
        |""".stripMargin) shouldBe 42
  }

  "function with two args" in {
    compileAndRun(
      """myAdd(a: int, b: int) -> int = a + b
        |main() -> int = myAdd(20, 22)
        |""".stripMargin) shouldBe 42
  }

  "function with three args" in {
    compileAndRun(
      """sum3(a: int, b: int, c: int) -> int = a + b + c
        |main() -> int = sum3(10, 20, 12)
        |""".stripMargin) shouldBe 42
  }

  "abs function" in {
    compileAndRun(
      """abs_val(x: int) -> int
        |    if x < 0 then -x else x
        |
        |main() -> int = abs_val(-42)
        |""".stripMargin) shouldBe 42
  }

  "simple recursion" in {
    compileAndRun(
      """countdown(n: int) -> int
        |    if n <= 0 then 0 else countdown(n - 1)
        |
        |main() -> int = countdown(3)
        |""".stripMargin, maxCycles = 500000) shouldBe 0
  }

  "recursive factorial small" ignore {
    compileAndRun(
      """fact(n: int) -> int
        |    if n <= 1 then 1 else n * fact(n - 1)
        |
        |main() -> int = fact(2)
        |""".stripMargin, maxCycles = 500000) shouldBe 2
  }

  "recursive factorial" ignore {
    compileAndRun(
      """fact(n: int) -> int
        |    if n <= 1 then 1 else n * fact(n - 1)
        |
        |main() -> int = fact(5)
        |""".stripMargin, maxCycles = 500000) shouldBe 120
  }

  "void function" in {
    compileAndRun(
      """var x: int = 0
        |
        |bump()
        |    x = 42
        |
        |main() -> int
        |    bump()
        |    x
        |""".stripMargin) shouldBe 42
  }
}
