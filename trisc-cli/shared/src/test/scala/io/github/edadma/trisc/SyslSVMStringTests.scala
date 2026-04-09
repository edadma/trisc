package io.github.edadma.trisc

class SyslSVMStringTests extends SyslSVMCodegenHelpers {

  "string length" in {
    compileAndRun(
      """main() -> i64
        |    var s = "hello"
        |    len(s)
        |""".stripMargin) shouldBe 5
  }

  "empty string length" in {
    compileAndRun(
      """main() -> i64
        |    var s = ""
        |    len(s)
        |""".stripMargin) shouldBe 0
  }

  "void function then return value" in {
    compileAndRun(
      """nop_func()
        |    var x: i64 = 1
        |
        |main() -> i64
        |    nop_func()
        |    42
        |""".stripMargin) shouldBe 42
  }

  "string alloc then return" in {
    compileAndRun(
      """main() -> i64
        |    var s = "hello"
        |    42
        |""".stripMargin) shouldBe 42
  }

  "string literal puts" in {
    compileAndRun(
      """main() -> i64
        |    puts("hello")
        |    42
        |""".stripMargin) shouldBe 42
  }

  "string passed to function" in {
    compileAndRun(
      """get_len(s: string) -> i64 = len(s)
        |
        |main() -> i64
        |    get_len("world")
        |""".stripMargin) shouldBe 5
  }

  "string local variable" in {
    compileAndRun(
      """main() -> i64
        |    var a = "abc"
        |    var b = "defgh"
        |    len(a) + len(b)
        |""".stripMargin) shouldBe 8
  }

  "string field in struct" in {
    compileAndRun(
      """struct Named
        |    name: string
        |    id: i64
        |
        |main() -> i64
        |    var n = Named("hello", 7)
        |    len(n.name) + n.id
        |""".stripMargin) shouldBe 12
  }
}
