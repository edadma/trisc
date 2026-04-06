package io.github.edadma.trisc

class SyslUnsignedWrapTests extends SyslTestHelpers {

  // ===== u32 wrapping =====

  "u32 addition wraps" in {
    eval(
      """main() -> int
        |    var a: u32 = u32(0xFFFFFFFF)
        |    a += u32(1)
        |    i64(a)
        |""".stripMargin) shouldBe 0
  }

  "u32 multiply wraps" in {
    eval(
      """main() -> int
        |    var a: u32 = u32(0x80000000)
        |    a = a * u32(2)
        |    i64(a)
        |""".stripMargin) shouldBe 0
  }

  "u32 add midrange wraps" in {
    eval(
      """main() -> int
        |    var a: u32 = u32(0xFFFFFFFE)
        |    a += u32(3)
        |    i64(a)
        |""".stripMargin) shouldBe 1
  }

  "u32 sub underflow wraps" in {
    eval(
      """main() -> int
        |    var a: u32 = u32(0)
        |    a -= u32(1)
        |    i64(a)
        |""".stripMargin) shouldBe 0xFFFFFFFFL
  }

  "u32 shift wraps" in {
    eval(
      """main() -> int
        |    var a: u32 = u32(1)
        |    a = a << 32
        |    i64(a)
        |""".stripMargin) shouldBe 0
  }

  "u32 increment wraps" in {
    eval(
      """main() -> int
        |    var a: u32 = u32(0xFFFFFFFF)
        |    a++
        |    i64(a)
        |""".stripMargin) shouldBe 0
  }

  "u32 mul 64K * 64K wraps" in {
    eval(
      """main() -> int
        |    var a: u32 = u32(0x10000)
        |    a = a * u32(0x10000)
        |    i64(a)
        |""".stripMargin) shouldBe 0
  }

  "u32 rotate left" in {
    eval(
      """main() -> int
        |    val x: u32 = u32(0x80000001)
        |    val r: u32 = (x << 1) | (x >> 31)
        |    i64(r)
        |""".stripMargin) shouldBe 3
  }

  "u32 compound wraps" in {
    eval(
      """main() -> int
        |    var a: u32 = u32(0xFFFFFF00)
        |    a += u32(0x200)
        |    i64(a)
        |""".stripMargin) shouldBe 0x100
  }

  "u32 expression wraps" in {
    eval(
      """main() -> int
        |    val a: u32 = u32(0xFFFFFFFF)
        |    val b: u32 = a + u32(1)
        |    i64(b)
        |""".stripMargin) shouldBe 0
  }

  // ===== u8 wrapping =====

  "u8 wraps" in {
    eval(
      """main() -> int
        |    var a: u8 = u8(255)
        |    a += u8(1)
        |    i64(a)
        |""".stripMargin) shouldBe 0
  }

  "u8 sub underflow wraps" in {
    eval(
      """main() -> int
        |    var a: u8 = u8(0)
        |    a -= u8(1)
        |    i64(a)
        |""".stripMargin) shouldBe 255
  }

  // ===== u16 wrapping =====

  "u16 wraps" in {
    eval(
      """main() -> int
        |    var a: u16 = u16(0xFFFF)
        |    a += u16(1)
        |    i64(a)
        |""".stripMargin) shouldBe 0
  }

  "u16 sub underflow wraps" in {
    eval(
      """main() -> int
        |    var a: u16 = u16(0)
        |    a -= u16(1)
        |    i64(a)
        |""".stripMargin) shouldBe 0xFFFF
  }

  // ===== Negation and bitwise NOT =====

  "u32 negation wraps" in {
    eval(
      """main() -> int
        |    val a: u32 = u32(1)
        |    val b: u32 = -a
        |    i64(b)
        |""".stripMargin) shouldBe 0xFFFFFFFFL
  }

  "u8 bitwise NOT wraps" in {
    eval(
      """main() -> int
        |    val a: u8 = u8(0)
        |    val b: u8 = ~a
        |    i64(b)
        |""".stripMargin) shouldBe 255
  }

  // ===== Decrement wrapping =====

  "u32 decrement wraps" in {
    eval(
      """main() -> int
        |    var a: u32 = u32(0)
        |    a--
        |    i64(a)
        |""".stripMargin) shouldBe 0xFFFFFFFFL
  }
}
