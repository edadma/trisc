package io.github.edadma.trisc

class SyslCodegenConstTests extends SyslCodegenHelpers {

  "return constant 0" in {
    compileAndRun("main() -> int = 0\n") shouldBe 0
  }

  "return constant 42" in {
    compileAndRun("main() -> int = 42\n") shouldBe 42
  }

  "return constant 255" in {
    compileAndRun("main() -> int = 255\n") shouldBe 255
  }

  "return large constant" in {
    compileAndRun("main() -> int = 1000\n") shouldBe 1000
  }

  "unary minus" in {
    compileAndRun("main() -> int = -42\n") shouldBe -42
  }

  "global val with negative value" in {
    compileAndRun(
      """val NEG = -1
        |main() -> int = NEG
        |""".stripMargin) shouldBe -1
  }

  "global val with negative value used in comparison" in {
    compileAndRun(
      """val EMPTY = -1
        |var x = -1
        |
        |main() -> int
        |    if x == EMPTY
        |        42
        |    else
        |        0
        |""".stripMargin) shouldBe 42
  }

  "global val with negative value in array init" in {
    compileAndRun(
      """val NONE = -1
        |var arr: [4]int
        |
        |init()
        |    var i = 0
        |    while i < 4
        |        arr[i] = NONE
        |        i += 1
        |
        |main() -> int
        |    init()
        |    arr[0] + arr[1] + arr[2] + arr[3]
        |""".stripMargin) shouldBe -4
  }

  "global val with unary minus expression" in {
    compileAndRun(
      """val A = -42
        |val B = -100
        |main() -> int = A + B
        |""".stripMargin) shouldBe -142
  }
}
