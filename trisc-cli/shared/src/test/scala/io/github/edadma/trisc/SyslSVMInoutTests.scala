package io.github.edadma.trisc

class SyslSVMInoutTests extends SyslSVMCodegenHelpers {

  "inout param" in {
    compileAndRun(
      """bump(inout n: i64)
        |    n = n + 1
        |
        |main() -> i64
        |    var x: i64 = 10
        |    bump(x)
        |    x
        |""".stripMargin) shouldBe 11
  }
}
