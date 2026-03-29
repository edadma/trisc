package io.github.edadma.trisc

class SyslCodegenControlFlowTests extends SyslCodegenHelpers {

  "if true branch" in {
    compileAndRun(
      """main() -> int
        |    if true
        |        return 42
        |    0
        |""".stripMargin) shouldBe 42
  }

  "if false branch" in {
    compileAndRun(
      """main() -> int
        |    if false
        |        return 42
        |    0
        |""".stripMargin) shouldBe 0
  }

  "if-then-else expression" in {
    compileAndRun(
      """main() -> int
        |    x = -42
        |    if x < 0 then -x else x
        |""".stripMargin) shouldBe 42
  }

  "while loop" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    sum = 0
        |    while i < 5
        |        sum = sum + i
        |        i = i + 1
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "for loop sum" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 5; i++
        |        sum += i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "for loop factorial" in {
    compileAndRun(
      """main() -> int
        |    result = 1
        |    for i = 1; i <= 5; i++
        |        result *= i
        |    result
        |""".stripMargin) shouldBe 120
  }

  "do/while loop" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    do
        |        i += 1
        |    while i < 5
        |    i
        |""".stripMargin) shouldBe 5
  }

  "do/while executes at least once" in {
    compileAndRun(
      """main() -> int
        |    x = 0
        |    do
        |        x = 42
        |    while false
        |    x
        |""".stripMargin) shouldBe 42
  }

  "break in while" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    while true
        |        if i == 5 then break
        |        i += 1
        |    i
        |""".stripMargin) shouldBe 5
  }

  "break in for" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 100; i++
        |        if i == 5 then break
        |        sum += i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "continue in for" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 10; i++
        |        if i % 2 == 0 then continue
        |        sum += i
        |    sum
        |""".stripMargin) shouldBe 25
  }

  "bool cast from nonzero" in {
    compileAndRun("main() -> int = bool(42)\n") shouldBe 1
  }

  "bool cast from zero" in {
    compileAndRun("main() -> int = bool(0)\n") shouldBe 0
  }

  "byte cast truncates" in {
    compileAndRun("main() -> int = byte(256)\n") shouldBe 0
  }

  "byte cast preserves low bits" in {
    compileAndRun("main() -> int = byte(0xff)\n") shouldBe -1  // byte is signed i8: 0xFF → -1
  }

  "char cast truncates to 32 bits" in {
    compileAndRun("main() -> int = char(65)\n") shouldBe 65
  }
}
