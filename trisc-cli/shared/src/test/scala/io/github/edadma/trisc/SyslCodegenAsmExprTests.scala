package io.github.edadma.trisc

class SyslCodegenAsmExprTests extends SyslCodegenHelpers {

  "asm expr as expression body — integer" in {
    compileAndRun(
      """double_it(x: int) -> int = asm("add r1, r1, r1")
        |
        |main() -> int = double_it(21)
        |""".stripMargin) shouldBe 42
  }

  "asm expr as last statement in block body" in {
    compileAndRun(
      """add_ten(x: int) -> int
        |    asm("addi r1, r1, 10")
        |
        |main() -> int = add_ten(32)
        |""".stripMargin) shouldBe 42
  }

  "asm expr with preceding code in block" in {
    compileAndRun(
      """add_and_double(x: int, y: int) -> int
        |    var z = x + y
        |    asm("add r1, r1, r1")
        |
        |main() -> int = add_and_double(10, 11)
        |""".stripMargin) shouldBe 42
  }

  "asm expr with fsqrt" in {
    compileAndRun(
      """my_sqrt(x: double) -> double = asm("fsqrt r1, r1")
        |
        |main() -> int
        |    var x = 4.0
        |    int(my_sqrt(x))
        |""".stripMargin) shouldBe 2
  }

  "asm expr with fabs" in {
    compileAndRun(
      """my_fabs(x: double) -> double = asm("fabs r1, r1")
        |
        |main() -> int
        |    var x = -5.0
        |    int(my_fabs(x))
        |""".stripMargin) shouldBe 5
  }

  "asm expr with fneg" in {
    compileAndRun(
      """my_fneg(x: double) -> double = asm("fneg r1, r1")
        |
        |main() -> int
        |    var x = 7.0
        |    int(my_fneg(x))
        |""".stripMargin) shouldBe -7
  }
}
