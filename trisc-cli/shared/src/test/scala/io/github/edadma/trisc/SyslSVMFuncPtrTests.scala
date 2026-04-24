package io.github.edadma.trisc

class SyslSVMFuncPtrTests extends SyslSVMCodegenHelpers {

  "call through function variable" in {
    compileAndRun(
      """add(a: i64, b: i64) -> i64 = a + b
        |
        |main() -> i64
        |    var f = add
        |    f(3, 4)
        |""".stripMargin) shouldBe 7
  }

  "higher-order apply" in {
    compileAndRun(
      """apply(f: (i64) -> i64, x: i64) -> i64 = f(x)
        |
        |dbl(n: i64) -> i64 = n * 2
        |
        |main() -> i64 = apply(dbl, 21)
        |""".stripMargin) shouldBe 42
  }
}
