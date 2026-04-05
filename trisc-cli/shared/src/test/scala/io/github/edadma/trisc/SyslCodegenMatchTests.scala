package io.github.edadma.trisc

class SyslCodegenMatchTests extends SyslCodegenHelpers {

  // ===== Basic value matching =====

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

  "no match no else returns 0" in {
    compileAndRun(
      """main() -> int
        |    99 match
        |        1 -> 10
        |        2 -> 20
        |""".stripMargin) shouldBe 0
  }

  // ===== Wildcard =====

  "wildcard matches anything" in {
    compileAndRun(
      """main() -> int
        |    x = 42
        |    x match
        |        1 -> 10
        |        _ -> 99
        |""".stripMargin) shouldBe 99
  }

  // ===== Guards =====

  "match with guard" in {
    compileAndRun(
      """main() -> int
        |    x = 15
        |    x match
        |        _ if x > 10 -> 1
        |        _ if x > 0 -> 2
        |        else -> 3
        |""".stripMargin) shouldBe 1
  }

  "guard rejects first, matches second" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    x match
        |        _ if x > 10 -> 1
        |        _ if x > 0 -> 2
        |        else -> 3
        |""".stripMargin) shouldBe 2
  }

  // ===== Range matching =====

  "range match" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    x match
        |        1..3 -> 1
        |        4..6 -> 2
        |        7..9 -> 3
        |        else -> 0
        |""".stripMargin) shouldBe 2
  }

  "range match boundary" in {
    compileAndRun(
      """main() -> int
        |    x = 10
        |    x match
        |        1..10 -> 1
        |        else -> 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Struct destructuring =====

  "destructure struct in match" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p = Point(10, 20)
        |    p match
        |        Point(x, y) -> x + y
        |""".stripMargin) shouldBe 30
  }

  "destructure with wildcard fields" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p = Point(10, 20)
        |    p match
        |        Point(_, y) -> y
        |""".stripMargin) shouldBe 20
  }
}
