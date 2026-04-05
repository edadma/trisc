package io.github.edadma.trisc

class SyslCodegenMatchTests extends SyslCodegenHelpers {

  "match single value" in {
    compileAndRun(
      """main() -> int
        |    x = 2
        |    x match
        |        1 -> 10
        |        2 -> 20
        |        3 -> 30
        |""".stripMargin) shouldBe 20
  }

  "match with else" in {
    compileAndRun(
      """main() -> int
        |    x = 99
        |    x match
        |        1 -> 10
        |        2 -> 20
        |        else -> 0
        |""".stripMargin) shouldBe 0
  }

  "match multiple values per arm" in {
    compileAndRun(
      """main() -> int
        |    x = 3
        |    x match
        |        1, 2 -> 10
        |        3, 4 -> 20
        |        else -> 0
        |""".stripMargin) shouldBe 20
  }

  "match as expression" in {
    compileAndRun(
      """main() -> int
        |    x = 2
        |    y = x match
        |        1 -> 100
        |        2 -> 200
        |        else -> 0
        |    y
        |""".stripMargin) shouldBe 200
  }

  "match as return value" in {
    compileAndRun(
      """classify(x: int) -> int
        |    x match
        |        0 -> 0
        |        1 -> 1
        |        else -> 2
        |
        |main() -> int
        |    classify(0) * 100 + classify(1) * 10 + classify(5)
        |""".stripMargin) shouldBe 12
  }

  "match with block body" in {
    compileAndRun(
      """main() -> int
        |    x = 2
        |    x match
        |        1 ->
        |            a = 10
        |            a + 1
        |        2 ->
        |            b = 20
        |            b + 2
        |        else -> 0
        |""".stripMargin) shouldBe 22
  }

  "match on expression" in {
    compileAndRun(
      """main() -> int
        |    a = 3
        |    b = 2
        |    a + b match
        |        4 -> 40
        |        5 -> 50
        |        6 -> 60
        |        else -> 0
        |""".stripMargin) shouldBe 50
  }

  "no match no else returns 0" in {
    compileAndRun(
      """main() -> int
        |    99 match
        |        1 -> 10
        |        2 -> 20
        |""".stripMargin) shouldBe 0
  }
}
