package io.github.edadma.trisc

class SyslIntrinsicTests extends SyslTestHelpers {

  // ===== wrapping_add =====

  "wrapping_add: i32 normal sum" in {
    eval(
      """main() -> int
        |    wrapping_add(40, 2)
        |""".stripMargin) shouldBe 42
  }

  "wrapping_add: i32 overflow wraps to negative" in {
    eval(
      """main() -> int
        |    var x: i32 = 2147483647
        |    wrapping_add(x, 1)
        |""".stripMargin) shouldBe -2147483648
  }

  "wrapping_add: u8 overflow wraps to 0" in {
    eval(
      """main() -> int
        |    var a: u8 = 255
        |    var b: u8 = 1
        |    int(wrapping_add(a, b))
        |""".stripMargin) shouldBe 0
  }

  "wrapping_add: u8 wraps past max" in {
    eval(
      """main() -> int
        |    var a: u8 = 200
        |    var b: u8 = 100
        |    int(wrapping_add(a, b))
        |""".stripMargin) shouldBe 44
  }

  // ===== wrapping_sub =====

  "wrapping_sub: u8 underflow wraps to 255" in {
    eval(
      """main() -> int
        |    var a: u8 = 0
        |    var b: u8 = 1
        |    int(wrapping_sub(a, b))
        |""".stripMargin) shouldBe 255
  }

  // ===== wrapping_mul =====

  "wrapping_mul: i16 overflow wraps" in {
    eval(
      """main() -> int
        |    var a: i16 = 1000
        |    var b: i16 = 100
        |    int(wrapping_mul(a, b))
        |""".stripMargin) shouldBe ((100000 << 16) >> 16)  // = -31072 (sign-extended)
  }

  // ===== saturating_add =====

  "saturating_add: u8 max clamped" in {
    eval(
      """main() -> int
        |    var a: u8 = 200
        |    var b: u8 = 100
        |    int(saturating_add(a, b))
        |""".stripMargin) shouldBe 255
  }

  "saturating_add: i8 positive overflow clamped to max" in {
    eval(
      """main() -> int
        |    var a: i8 = 100
        |    var b: i8 = 50
        |    int(saturating_add(a, b))
        |""".stripMargin) shouldBe 127
  }

  "saturating_add: i8 negative overflow clamped to min" in {
    eval(
      """main() -> int
        |    var a: i8 = i8(-100)
        |    var b: i8 = i8(-50)
        |    int(saturating_add(a, b))
        |""".stripMargin) shouldBe -128
  }

  "saturating_add: normal sum unchanged" in {
    eval(
      """main() -> int
        |    var a: u8 = 50
        |    var b: u8 = 75
        |    int(saturating_add(a, b))
        |""".stripMargin) shouldBe 125
  }

  // ===== saturating_sub =====

  "saturating_sub: u8 underflow clamped to 0" in {
    eval(
      """main() -> int
        |    var a: u8 = 5
        |    var b: u8 = 10
        |    int(saturating_sub(a, b))
        |""".stripMargin) shouldBe 0
  }

  "saturating_sub: i8 underflow clamped to min" in {
    eval(
      """main() -> int
        |    var a: i8 = i8(-100)
        |    var b: i8 = 100
        |    int(saturating_sub(a, b))
        |""".stripMargin) shouldBe -128
  }

  // ===== saturating_mul =====

  "saturating_mul: u8 overflow clamped to 255" in {
    eval(
      """main() -> int
        |    var a: u8 = 100
        |    var b: u8 = 100
        |    int(saturating_mul(a, b))
        |""".stripMargin) shouldBe 255
  }

  "saturating_mul: i16 overflow clamped to max" in {
    eval(
      """main() -> int
        |    var a: i16 = 1000
        |    var b: i16 = 100
        |    int(saturating_mul(a, b))
        |""".stripMargin) shouldBe 32767
  }

  "saturating_mul: i16 negative overflow clamped to min" in {
    eval(
      """main() -> int
        |    var a: i16 = i16(-1000)
        |    var b: i16 = 100
        |    int(saturating_mul(a, b))
        |""".stripMargin) shouldBe -32768
  }

  // ===== Type checking =====

  "intrinsic rejects mixed types" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var a: i32 = 1
        |    var b: i64 = 2
        |    wrapping_add(a, b)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "intrinsic rejects float arguments" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var a: f32 = 1.0
        |    var b: f32 = 2.0
        |    int(wrapping_add(a, b))
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "intrinsic rejects wrong arity" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    wrapping_add(1)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }
}
