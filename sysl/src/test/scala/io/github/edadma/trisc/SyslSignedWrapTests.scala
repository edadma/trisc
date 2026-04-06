package io.github.edadma.trisc

class SyslSignedWrapTests extends SyslTestHelpers {

  // ===== i32 wrapping =====

  "i32 addition wraps to negative" in {
    eval(
      """main() -> int
        |    var a: int = 2147483647
        |    a += 1
        |    i64(a)
        |""".stripMargin) shouldBe -2147483648L
  }

  "i32 subtraction wraps to positive" in {
    eval(
      """main() -> int
        |    var a: int = -2147483648
        |    a -= 1
        |    i64(a)
        |""".stripMargin) shouldBe 2147483647L
  }

  "i32 multiply wraps" in {
    eval(
      """main() -> int
        |    var a: int = 100000
        |    a = a * 100000
        |    i64(a)
        |""".stripMargin) shouldBe 1410065408L
  }

  "i32 increment wraps" in {
    eval(
      """main() -> int
        |    var a: int = 2147483647
        |    a++
        |    i64(a)
        |""".stripMargin) shouldBe -2147483648L
  }

  "i32 decrement wraps" in {
    eval(
      """main() -> int
        |    var a: int = -2147483648
        |    a--
        |    i64(a)
        |""".stripMargin) shouldBe 2147483647L
  }

  "i32 shift wraps" in {
    eval(
      """main() -> int
        |    var a: int = 1
        |    a = a << 31
        |    i64(a)
        |""".stripMargin) shouldBe -2147483648L
  }

  "i32 expression wraps" in {
    eval(
      """main() -> int
        |    val a: int = 2147483647
        |    val b: int = a + 1
        |    i64(b)
        |""".stripMargin) shouldBe -2147483648L
  }

  "i32 compound wraps" in {
    eval(
      """main() -> int
        |    var a: int = 2147483640
        |    a += 100
        |    i64(a) < 0
        |""".stripMargin) shouldBe 1
  }

  // ===== i8 wrapping =====

  "i8 wraps" in {
    eval(
      """main() -> int
        |    var a: i8 = i8(127)
        |    a += i8(1)
        |    i64(a)
        |""".stripMargin) shouldBe -128
  }

  "i8 underflow wraps" in {
    eval(
      """main() -> int
        |    var a: i8 = i8(-128)
        |    a -= i8(1)
        |    i64(a)
        |""".stripMargin) shouldBe 127
  }

  // ===== i16 wrapping =====

  "i16 wraps" in {
    eval(
      """main() -> int
        |    var a: i16 = i16(32767)
        |    a += i16(1)
        |    i64(a)
        |""".stripMargin) shouldBe -32768
  }

  "i16 underflow wraps" in {
    eval(
      """main() -> int
        |    var a: i16 = i16(-32768)
        |    a -= i16(1)
        |    i64(a)
        |""".stripMargin) shouldBe 32767
  }

  // ===== Negation =====

  "i32 negation of INT_MIN wraps" in {
    eval(
      """main() -> int
        |    val a: int = -2147483648
        |    val b: int = -a
        |    i64(b)
        |""".stripMargin) shouldBe -2147483648L
  }
}
