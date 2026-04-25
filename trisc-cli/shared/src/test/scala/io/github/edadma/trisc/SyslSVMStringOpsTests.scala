package io.github.edadma.trisc

class SyslSVMStringOpsTests extends SyslSVMCodegenHelpers {

  "concat string literals" in {
    compileAndRun(
      """main() -> i64
        |    var s = "hello" + " world"
        |    len(s)
        |""".stripMargin) shouldBe 11
  }

  "concat preserves content via puts" in {
    compileAndRun(
      """main() -> i64
        |    var s = "foo" + "bar"
        |    puts(s)
        |    len(s)
        |""".stripMargin) shouldBe 6
  }

  "string eq true" in {
    compileAndRun(
      """main() -> i64
        |    var a = "hello"
        |    var b = "hello"
        |    if a == b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "string eq false" in {
    compileAndRun(
      """main() -> i64
        |    var a = "hello"
        |    var b = "world"
        |    if a == b then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "string ne" in {
    compileAndRun(
      """main() -> i64
        |    var a = "hello"
        |    var b = "world"
        |    if a != b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "string eq different lengths" in {
    compileAndRun(
      """main() -> i64
        |    var a = "hi"
        |    var b = "hello"
        |    if a == b then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "chained concat" in {
    compileAndRun(
      """main() -> i64
        |    var s = "a" + "b" + "c" + "d"
        |    len(s)
        |""".stripMargin) shouldBe 4
  }
}
