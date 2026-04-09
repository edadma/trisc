package io.github.edadma.trisc

class SyslSVMControlFlowTests extends SyslSVMCodegenHelpers {

  "if-else expression" in {
    compileAndRun(
      """main() -> int
        |    if true then 42 else 0
        |""".stripMargin) shouldBe 42
  }

  "if-else false branch" in {
    compileAndRun(
      """main() -> int
        |    if false then 0 else 42
        |""".stripMargin) shouldBe 42
  }

  "while loop" in {
    compileAndRun(
      """main() -> int
        |    var sum = 0
        |    var i = 1
        |    while i <= 10
        |        sum += i
        |        i += 1
        |    sum
        |""".stripMargin) shouldBe 55
  }

  "for loop" in {
    compileAndRun(
      """main() -> int
        |    var sum = 0
        |    for var i = 1; i <= 10; i += 1
        |        sum += i
        |    sum
        |""".stripMargin) shouldBe 55
  }

  "while with break" in {
    compileAndRun(
      """main() -> int
        |    var x = 0
        |    while true
        |        x += 1
        |        if x == 42
        |            break
        |    x
        |""".stripMargin) shouldBe 42
  }

  "for with continue" in {
    compileAndRun(
      """main() -> int
        |    var sum = 0
        |    for var i = 1; i <= 10; i += 1
        |        if i % 2 == 0
        |            continue
        |        sum += i
        |    sum
        |""".stripMargin) shouldBe 25
  }

  "nested if" in {
    compileAndRun(
      """classify(x: int) -> int
        |    if x > 0
        |        1
        |    else if x < 0
        |        -1
        |    else
        |        0
        |
        |main() -> int = classify(5) + classify(-3) + classify(0)
        |""".stripMargin) shouldBe 0
  }
}
