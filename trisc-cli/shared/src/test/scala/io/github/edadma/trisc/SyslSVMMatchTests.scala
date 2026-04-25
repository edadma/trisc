package io.github.edadma.trisc

class SyslSVMMatchTests extends SyslSVMCodegenHelpers {

  "match single value" in {
    compileAndRun(
      """main() -> i64
        |    var x: i64 = 2
        |    x match
        |        1 -> 10
        |        2 -> 20
        |        3 -> 30
        |""".stripMargin) shouldBe 20
  }

  "match with else" in {
    compileAndRun(
      """main() -> i64
        |    var x: i64 = 99
        |    x match
        |        1 -> 10
        |        2 -> 20
        |        else -> 0
        |""".stripMargin) shouldBe 0
  }

  "match multiple values per arm" in {
    compileAndRun(
      """main() -> i64
        |    var x: i64 = 3
        |    x match
        |        1, 2 -> 10
        |        3, 4 -> 20
        |        else -> 0
        |""".stripMargin) shouldBe 20
  }

  "match as expression" in {
    compileAndRun(
      """main() -> i64
        |    var x: i64 = 2
        |    var y: i64 = x match
        |        1 -> 100
        |        2 -> 200
        |        else -> 0
        |    y
        |""".stripMargin) shouldBe 200
  }

  "match as return value" in {
    compileAndRun(
      """classify(x: i64) -> i64
        |    x match
        |        0 -> 0
        |        1 -> 1
        |        else -> 2
        |
        |main() -> i64
        |    classify(0) * 100 + classify(1) * 10 + classify(5)
        |""".stripMargin) shouldBe 12
  }

  "match with block body" in {
    compileAndRun(
      """main() -> i64
        |    var x: i64 = 2
        |    x match
        |        1 ->
        |            var a: i64 = 10
        |            a + 1
        |        2 ->
        |            var b: i64 = 20
        |            b + 2
        |        else -> 0
        |""".stripMargin) shouldBe 22
  }

  "no match no else returns 0" in {
    compileAndRun(
      """main() -> i64
        |    99 match
        |        1 -> 10
        |        2 -> 20
        |""".stripMargin) shouldBe 0
  }

  "wildcard matches anything" in {
    compileAndRun(
      """main() -> i64
        |    var x: i64 = 42
        |    x match
        |        1 -> 10
        |        _ -> 99
        |""".stripMargin) shouldBe 99
  }

  "match range inclusive" in {
    compileAndRun(
      """main() -> i64
        |    var x: i64 = 5
        |    x match
        |        1..3 -> 10
        |        4..6 -> 20
        |        else -> 0
        |""".stripMargin) shouldBe 20
  }

  "match with guard" in {
    compileAndRun(
      """main() -> i64
        |    var x: i64 = 5
        |    x match
        |        _ if x > 3 -> 100
        |        _ -> 0
        |""".stripMargin) shouldBe 100
  }

  "match statement no result" in {
    compileAndRun(
      """main() -> i64
        |    var x: i64 = 2
        |    var out: i64 = 0
        |    x match
        |        1 -> out = 10
        |        2 -> out = 20
        |        else -> out = 99
        |    out
        |""".stripMargin) shouldBe 20
  }
}
