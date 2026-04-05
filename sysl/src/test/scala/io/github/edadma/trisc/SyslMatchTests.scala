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

  "match as expression" in {
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

  "no match no else returns 0" in {
    eval(
      """main() -> int
        |    99 match
        |        1 -> 10
        |        2 -> 20
        |""".stripMargin) shouldBe 0
  }

  // ===== Wildcard =====

  "wildcard matches anything" in {
    eval(
      """main() -> int
        |    x = 42
        |    x match
        |        1 -> 10
        |        _ -> 99
        |""".stripMargin) shouldBe 99
  }

  // ===== Guards =====

  "match with guard" in {
    eval(
      """main() -> int
        |    x = 15
        |    x match
        |        _ if x > 10 -> 1
        |        _ if x > 0 -> 2
        |        else -> 3
        |""".stripMargin) shouldBe 1
  }

  "guard rejects first, matches second" in {
    eval(
      """main() -> int
        |    x = 5
        |    x match
        |        _ if x > 10 -> 1
        |        _ if x > 0 -> 2
        |        else -> 3
        |""".stripMargin) shouldBe 2
  }

  "guard with value pattern" in {
    eval(
      """main() -> int
        |    x = 0
        |    x match
        |        0 if false -> 99
        |        0 -> 42
        |        else -> 0
        |""".stripMargin) shouldBe 42
  }

  // ===== Range matching =====

  "range match" in {
    eval(
      """main() -> int
        |    x = 5
        |    x match
        |        1..3 -> 1
        |        4..6 -> 2
        |        7..9 -> 3
        |        else -> 0
        |""".stripMargin) shouldBe 2
  }

  "range match boundary low" in {
    eval(
      """main() -> int
        |    x = 1
        |    x match
        |        1..10 -> 1
        |        else -> 0
        |""".stripMargin) shouldBe 1
  }

  "range match boundary high" in {
    eval(
      """main() -> int
        |    x = 10
        |    x match
        |        1..10 -> 1
        |        else -> 0
        |""".stripMargin) shouldBe 1
  }

  "range match out of range" in {
    eval(
      """main() -> int
        |    x = 11
        |    x match
        |        1..10 -> 1
        |        else -> 0
        |""".stripMargin) shouldBe 0
  }

  // ===== Struct destructuring =====

  "destructure struct in match" in {
    eval(
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
    eval(
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

  "destructure with guard" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p = Point(0, 42)
        |    p match
        |        Point(x, y) if x == 0 -> y
        |        Point(x, y) -> x + y
        |""".stripMargin) shouldBe 42
  }
}
