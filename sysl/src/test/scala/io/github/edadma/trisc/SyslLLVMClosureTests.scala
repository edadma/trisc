package io.github.edadma.trisc

class SyslLLVMClosureTests extends SyslLLVMTestHelpers {

  "zero-capture closure" in {
    llvmExit(
      """apply(f: func(int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(x -> x + 1, 41)
        |""".stripMargin) shouldBe 42
  }

  "multi-param closure" in {
    llvmExit(
      """apply2(f: func(int, int) -> int, a: int, b: int) -> int = f(a, b)
        |
        |main() -> int = apply2((x, y) -> x + y, 20, 22)
        |""".stripMargin) shouldBe 42
  }

  "zero-param closure" in {
    llvmExit(
      """call(f: func() -> int) -> int = f()
        |
        |main() -> int = call(() -> 42)
        |""".stripMargin) shouldBe 42
  }

  "closure assigned to variable" in {
    llvmExit(
      """main() -> int
        |    val f: func(int) -> int = x -> x * 2
        |    f(21)
        |""".stripMargin) shouldBe 42
  }

  "capture local variable" in {
    llvmExit(
      """apply(f: func(int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    val a = 10
        |    apply(x -> x + a, 32)
        |""".stripMargin) shouldBe 42
  }

  "capture is frozen (by value)" in {
    llvmExit(
      """apply(f: func(int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    var a = 10
        |    val f: func(int) -> int = x -> x + a
        |    a = 100
        |    f(32)
        |""".stripMargin) shouldBe 42
  }

  "capture multiple variables" in {
    llvmExit(
      """apply(f: func(int) -> int, x: int) -> int = f(x)
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
        |apply(f: func(int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(dbl, 21)
        |""".stripMargin) shouldBe 42
  }

  "closure as return value" in {
    llvmExit(
      """make_adder(n: int) -> func(int) -> int
        |    val captured = n
        |    x -> x + captured
        |
        |main() -> int
        |    val add10 = make_adder(10)
        |    add10(32)
        |""".stripMargin) shouldBe 42
  }
}
