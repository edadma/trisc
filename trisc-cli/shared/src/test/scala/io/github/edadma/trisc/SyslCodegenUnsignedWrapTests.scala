package io.github.edadma.trisc

class SyslCodegenUnsignedWrapTests extends SyslCodegenHelpers {

  "u32 addition wraps" in {
    // Use subtraction to get 0xFFFFFFFF without large literal
    compileAndRun(
      """main() -> int
        |    var a: u32 = u32(0)
        |    a -= u32(1)
        |    // a is now 0xFFFFFFFF
        |    a += u32(1)
        |    i64(a)
        |""".stripMargin) shouldBe 0
  }

  "u32 multiply wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = u32(0x10000)
        |    a = a * u32(0x10000)
        |    i64(a)
        |""".stripMargin) shouldBe 0
  }

  "u32 sub underflow wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = u32(0)
        |    a -= u32(1)
        |    // Should be 0xFFFFFFFF = 4294967295
        |    val hi: u32 = a >> 16
        |    i64(hi)
        |""".stripMargin) shouldBe 0xFFFF
  }

  "u32 shift wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = u32(1)
        |    a = a << 32
        |    i64(a)
        |""".stripMargin) shouldBe 0
  }

  "u32 increment wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = u32(0)
        |    a -= u32(1)
        |    // a = 0xFFFFFFFF
        |    a++
        |    i64(a)
        |""".stripMargin) shouldBe 0
  }

  "u32 decrement wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = u32(0)
        |    a--
        |    val hi: u32 = a >> 16
        |    i64(hi)
        |""".stripMargin) shouldBe 0xFFFF
  }

  "u32 expression wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = u32(0)
        |    a -= u32(1)
        |    val b: u32 = a + u32(1)
        |    i64(b)
        |""".stripMargin) shouldBe 0
  }

  "u8 wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: u8 = u8(255)
        |    a += u8(1)
        |    i64(a)
        |""".stripMargin) shouldBe 0
  }

  "u16 wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: u16 = u16(0xFFFF)
        |    a += u16(1)
        |    i64(a)
        |""".stripMargin) shouldBe 0
  }

  "u32 compound wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = u32(0)
        |    a -= u32(1)
        |    // a = 0xFFFFFFFF
        |    a += u32(2)
        |    i64(a)
        |""".stripMargin) shouldBe 1
  }

  "u32 negation wraps" in {
    compileAndRun(
      """main() -> int
        |    val a: u32 = u32(1)
        |    val b: u32 = -a
        |    val hi: u32 = b >> 16
        |    i64(hi)
        |""".stripMargin) shouldBe 0xFFFF
  }

  "u8 bitwise NOT wraps" in {
    compileAndRun(
      """main() -> int
        |    val a: u8 = u8(0)
        |    val b: u8 = ~a
        |    i64(b)
        |""".stripMargin) shouldBe 255
  }

  "u32 rotate left via shift and or" in {
    compileAndRun(
      """main() -> int
        |    val x: u32 = u32(3)
        |    val r: u32 = (x << 31) | (x >> 1)
        |    // 3 << 31 = 0x80000000 + 0x00000000 (wraps to 0x80000000), 3 >> 1 = 1
        |    // result = 0x80000001
        |    val hi: u32 = r >> 16
        |    i64(hi)
        |""".stripMargin) shouldBe 0x8000
  }
}
