package io.github.edadma.trisc

class SyslTriscIntrinsicTests extends SyslCodegenHelpers {

  // ===== wrapping_* =====

  "wrapping_add: u8 overflow wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: u8 = 200
        |    var b: u8 = 100
        |    int(wrapping_add(a, b))
        |""".stripMargin) shouldBe 44
  }

  "wrapping_sub: u8 underflow wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: u8 = 0
        |    var b: u8 = 1
        |    int(wrapping_sub(a, b))
        |""".stripMargin) shouldBe 255
  }

  "wrapping_mul: i16 overflow wraps" in {
    compileAndRun(
      """main() -> int
        |    var a: i16 = 1000
        |    var b: i16 = 100
        |    int(wrapping_mul(a, b))
        |""".stripMargin) shouldBe ((100000 << 16) >> 16)  // -31072
  }

  // ===== saturating_* (narrow widths) =====

  "saturating_add: u8 clamps to 255" in {
    compileAndRun(
      """main() -> int
        |    var a: u8 = 200
        |    var b: u8 = 100
        |    int(saturating_add(a, b))
        |""".stripMargin) shouldBe 255
  }

  "saturating_add: i8 clamps to MAX" in {
    compileAndRun(
      """main() -> int
        |    var a: i8 = 100
        |    var b: i8 = 50
        |    int(saturating_add(a, b))
        |""".stripMargin) shouldBe 127
  }

  "saturating_add: i8 clamps to MIN" in {
    compileAndRun(
      """main() -> int
        |    var a: i8 = i8(-100)
        |    var b: i8 = i8(-50)
        |    int(saturating_add(a, b))
        |""".stripMargin) shouldBe -128
  }

  "saturating_sub: u8 clamps to 0" in {
    compileAndRun(
      """main() -> int
        |    var a: u8 = 5
        |    var b: u8 = 10
        |    int(saturating_sub(a, b))
        |""".stripMargin) shouldBe 0
  }

  "saturating_mul: u16 clamps to 65535" in {
    compileAndRun(
      """main() -> int
        |    var a: u16 = 1000
        |    var b: u16 = 1000
        |    int(saturating_mul(a, b))
        |""".stripMargin) shouldBe 65535
  }

  "saturating_mul: i16 negative clamps to MIN" in {
    compileAndRun(
      """main() -> int
        |    var a: i16 = i16(-1000)
        |    var b: i16 = 100
        |    int(saturating_mul(a, b))
        |""".stripMargin) shouldBe -32768
  }

  "saturating_add: normal sum unchanged" in {
    compileAndRun(
      """main() -> int
        |    var a: i32 = 100
        |    var b: i32 = 50
        |    saturating_add(a, b)
        |""".stripMargin) shouldBe 150
  }
}
