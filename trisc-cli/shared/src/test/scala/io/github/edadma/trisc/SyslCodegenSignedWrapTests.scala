package io.github.edadma.trisc

class SyslCodegenSignedWrapTests extends SyslCodegenHelpers {

  "i32 addition wraps to negative" in {
    compileAndRun(
      """main() -> int
        |    var a: int = 2147483647
        |    a += 1
        |    i64(a)
        |""".stripMargin) shouldBe -2147483648L
  }

  "i32 subtraction wraps to positive" in {
    // Construct INT_MIN via overflow instead of literal
    compileAndRun(
      """main() -> int
        |    var a: int = 2147483647
        |    a += 1
        |    // a is now INT_MIN
        |    a -= 1
        |    i64(a)
        |""".stripMargin) shouldBe 2147483647L
  }

  "i32 increment wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: int = 2147483647
        |    a++
        |    i64(a)
        |""".stripMargin) shouldBe -2147483648L
  }

  "i32 decrement wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: int = 2147483647
        |    a += 1
        |    // a is now INT_MIN
        |    a--
        |    i64(a)
        |""".stripMargin) shouldBe 2147483647L
  }

  "i32 shift wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: int = 1
        |    a = a << 31
        |    i64(a)
        |""".stripMargin) shouldBe -2147483648L
  }

  "i32 expression wraps" in {
    compileAndRun(
      """main() -> int
        |    val a: int = 2147483647
        |    val b: int = a + 1
        |    i64(b)
        |""".stripMargin) shouldBe -2147483648L
  }

  "i8 wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: i8 = i8(127)
        |    a += i8(1)
        |    i64(a)
        |""".stripMargin) shouldBe -128
  }

  "i16 wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: i16 = i16(32767)
        |    a += i16(1)
        |    i64(a)
        |""".stripMargin) shouldBe -32768
  }

  "i32 negation of INT_MIN wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: int = 2147483647
        |    a += 1
        |    // a is now INT_MIN
        |    val b: int = -a
        |    i64(b)
        |""".stripMargin) shouldBe -2147483648L
  }
}
