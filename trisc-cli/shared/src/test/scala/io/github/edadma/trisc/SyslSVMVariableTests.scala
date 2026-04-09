package io.github.edadma.trisc

class SyslSVMVariableTests extends SyslSVMCodegenHelpers {

  "local variable" in {
    compileAndRun(
      """main() -> int
        |    var x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "local reassignment" in {
    compileAndRun(
      """main() -> int
        |    var x = 10
        |    x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "multiple locals" in {
    compileAndRun(
      """main() -> int
        |    var a = 10
        |    var b = 20
        |    var c = 12
        |    a + b + c
        |""".stripMargin) shouldBe 42
  }

  "compound assign +=" in {
    compileAndRun(
      """main() -> int
        |    var x = 40
        |    x += 2
        |    x
        |""".stripMargin) shouldBe 42
  }

  "compound assign *=" in {
    compileAndRun(
      """main() -> int
        |    var x = 6
        |    x *= 7
        |    x
        |""".stripMargin) shouldBe 42
  }

  "pre-increment" in {
    compileAndRun(
      """main() -> int
        |    var x = 41
        |    ++x
        |""".stripMargin) shouldBe 42
  }

  "post-increment" in {
    compileAndRun(
      """main() -> int
        |    var x = 42
        |    x++
        |""".stripMargin) shouldBe 42
  }

  "global variable" in {
    compileAndRun(
      """var g: int = 42
        |
        |main() -> int = g
        |""".stripMargin) shouldBe 42
  }

  "global variable assignment" in {
    compileAndRun(
      """var g: int = 0
        |
        |main() -> int
        |    g = 42
        |    g
        |""".stripMargin) shouldBe 42
  }
}
