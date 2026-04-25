package io.github.edadma.trisc

class SyslSVMIntrinsicTests extends SyslSVMCodegenHelpers {

  // wrapping_add/sub/mul: arithmetic that wraps on overflow rather than
  // trapping. SVM's int ops already wrap naturally for i64.

  "wrapping_add basic" in {
    compileAndRun(
      """main() -> i64 = wrapping_add(3i64, 4i64)
        |""".stripMargin) shouldBe 7
  }

  "wrapping_add overflow" in {
    // i64 max + 1 should wrap to i64 min
    compileAndRun(
      """main() -> i64 = wrapping_add(9223372036854775807i64, 1i64)
        |""".stripMargin) shouldBe Long.MinValue
  }

  "wrapping_sub" in {
    compileAndRun(
      """main() -> i64 = wrapping_sub(10i64, 4i64)
        |""".stripMargin) shouldBe 6
  }

  "wrapping_sub underflow" in {
    // i64 MIN as a hex literal (parser can't directly use -9223372036854775808)
    compileAndRun(
      """main() -> i64 = wrapping_sub(0x8000000000000000i64, 1i64)
        |""".stripMargin) shouldBe Long.MaxValue
  }

  "wrapping_mul" in {
    compileAndRun(
      """main() -> i64 = wrapping_mul(7i64, 6i64)
        |""".stripMargin) shouldBe 42
  }

  "wrapping_mul overflow" in {
    // 2^32 * 2^32 = 2^64, wraps to 0
    compileAndRun(
      """main() -> i64 = wrapping_mul(4294967296i64, 4294967296i64)
        |""".stripMargin) shouldBe 0
  }

  "wrapping_add narrow i32" in {
    compileAndRun(
      """main() -> i64
        |    val r = wrapping_add(2147483647, 1)
        |    i64(r)
        |""".stripMargin) shouldBe Int.MinValue.toLong
  }

  // saturating_*: clamp to type bounds on overflow.

  "saturating_add no overflow" in {
    compileAndRun(
      """main() -> i64 = saturating_add(3i64, 4i64)
        |""".stripMargin) shouldBe 7
  }

  "saturating_add positive overflow signed" in {
    compileAndRun(
      """main() -> i64 = saturating_add(9223372036854775806i64, 100i64)
        |""".stripMargin) shouldBe Long.MaxValue
  }

  "saturating_add negative overflow signed" in {
    compileAndRun(
      """main() -> i64 = saturating_add(0x8000000000000001i64, -100i64)
        |""".stripMargin) shouldBe Long.MinValue
  }

  "saturating_sub no underflow" in {
    compileAndRun(
      """main() -> i64 = saturating_sub(10i64, 4i64)
        |""".stripMargin) shouldBe 6
  }

  "saturating_sub underflow signed" in {
    compileAndRun(
      """main() -> i64 = saturating_sub(0x8000000000000001i64, 100i64)
        |""".stripMargin) shouldBe Long.MinValue
  }

  "saturating_add unsigned overflow narrow" in {
    compileAndRun(
      """main() -> i64
        |    val a: u8 = 250u8
        |    val b: u8 = 100u8
        |    i64(saturating_add(a, b))
        |""".stripMargin) shouldBe 255
  }

  "saturating_sub unsigned underflow" in {
    compileAndRun(
      """main() -> i64
        |    val a: u8 = 5u8
        |    val b: u8 = 100u8
        |    i64(saturating_sub(a, b))
        |""".stripMargin) shouldBe 0
  }
}
