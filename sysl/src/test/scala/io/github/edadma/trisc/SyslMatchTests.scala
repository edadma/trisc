package io.github.edadma.trisc

class SyslMatchTests extends SyslTestHelpers {

  // ===== Basic value matching =====

  "match single value" in {
    eval(
      """main() -> int
        |    x = 2
        |    x match
        |        1 -> 10
        |        2 -> 20
        |        3 -> 30
        |""".stripMargin) shouldBe 20
  }

  "match first case" in {
    eval(
      """main() -> int
        |    x = 1
        |    x match
        |        1 -> 10
        |        2 -> 20
        |""".stripMargin) shouldBe 10
  }

  "match with else" in {
    eval(
      """main() -> int
        |    x = 99
        |    x match
        |        1 -> 10
        |        2 -> 20
        |        else -> 0
        |""".stripMargin) shouldBe 0
  }

  "match multiple values per arm" in {
    eval(
      """main() -> int
        |    x = 3
        |    x match
        |        1, 2 -> 10
        |        3, 4 -> 20
        |        else -> 0
        |""".stripMargin) shouldBe 20
  }

  // ===== Match as expression =====

  "match as expression in assignment" in {
    eval(
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
    eval(
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

  // ===== Match with block bodies =====

  "match with block body" in {
    eval(
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

  // ===== Match on enum values =====

  "match on enum" in {
    eval(
      """enum Color
        |    Red
        |    Green
        |    Blue
        |
        |main() -> int
        |    c = Color.Green
        |    c match
        |        Color.Red -> 1
        |        Color.Green -> 2
        |        Color.Blue -> 3
        |        else -> 0
        |""".stripMargin) shouldBe 2
  }

  // ===== Match with expressions as scrutinee =====

  "match on expression" in {
    eval(
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

  // ===== No match and no else returns 0 =====

  "no match no else returns 0" in {
    eval(
      """main() -> int
        |    99 match
        |        1 -> 10
        |        2 -> 20
        |""".stripMargin) shouldBe 0
  }
}
