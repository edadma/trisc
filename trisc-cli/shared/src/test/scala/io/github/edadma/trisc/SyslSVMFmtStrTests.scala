package io.github.edadma.trisc

class SyslSVMFmtStrTests extends SyslSVMCodegenHelpers {

  // f"..." format-string interpolation. Verbs: %d %x %X %o %b %s, with width
  // and the +/-/0 flags. We exercise these by computing the rendered string's
  // length (or specific bytes) so the assertions stay numeric.
  // Note: f-string interpolation needs `${expr}` for literals; bare `$ident`
  // works only for letter-starting names.

  "f-string %d basic" in {
    compileAndRun(
      """main() -> i64
        |    val n = 42
        |    val s = f"$n%d"
        |    len(s)
        |""".stripMargin) shouldBe 2
  }

  "f-string %x lowercase" in {
    compileAndRun(
      """main() -> i64
        |    val n = 255
        |    val s = f"$n%x"
        |    len(s)
        |""".stripMargin) shouldBe 2
  }

  "f-string %X uppercase first byte" in {
    // 255 = 0xff → "FF", first byte 'F'=70
    compileAndRun(
      """main() -> i64
        |    val n = 255
        |    val s = f"$n%X"
        |    i64(s[0])
        |""".stripMargin) shouldBe 70
  }

  "f-string %o" in {
    // 8 → "10" in octal, length 2
    compileAndRun(
      """main() -> i64
        |    val n = 8
        |    val s = f"$n%o"
        |    len(s)
        |""".stripMargin) shouldBe 2
  }

  "f-string %b" in {
    // 5 → "101" in binary, length 3
    compileAndRun(
      """main() -> i64
        |    val n = 5
        |    val s = f"$n%b"
        |    len(s)
        |""".stripMargin) shouldBe 3
  }

  "f-string width with zero pad" in {
    // 42 → "00042" with %05d, length 5
    compileAndRun(
      """main() -> i64
        |    val n = 42
        |    val s = f"$n%05d"
        |    len(s)
        |""".stripMargin) shouldBe 5
  }

  "f-string zero pad — leading char is '0'" in {
    compileAndRun(
      """main() -> i64
        |    val n = 42
        |    val s = f"$n%05d"
        |    i64(s[0])
        |""".stripMargin) shouldBe 48
  }

  "f-string width with space pad (right-aligned)" in {
    // %5d on 42 → "   42", first byte ' '=32
    compileAndRun(
      """main() -> i64
        |    val n = 42
        |    val s = f"$n%5d"
        |    i64(s[0])
        |""".stripMargin) shouldBe 32
  }

  "f-string left-align with space pad" in {
    // %-5d on 42 → "42   ", last byte ' '=32
    compileAndRun(
      """main() -> i64
        |    val n = 42
        |    val s = f"$n%-5d"
        |    i64(s[4])
        |""".stripMargin) shouldBe 32
  }

  "f-string showSign positive" in {
    // %+d on 42 → "+42", first byte '+'=43
    compileAndRun(
      """main() -> i64
        |    val n = 42
        |    val s = f"$n%+d"
        |    i64(s[0])
        |""".stripMargin) shouldBe 43
  }

  "f-string showSign negative still has minus" in {
    compileAndRun(
      """main() -> i64
        |    val n = -7
        |    val s = f"$n%+d"
        |    i64(s[0])
        |""".stripMargin) shouldBe 45  // '-'
  }

  // Direct runtime probe: width=5, zero-pad on "42" should be "00042" (length 5).
  "direct fmt runtime width=5 zero-pad len" in {
    compileAndRun(
      """extern __svm_str_fmt_i64(n: i64, base: int, width: int, flags: int) -> string
        |
        |main() -> i64
        |    val s = __svm_str_fmt_i64(42i64, 10, 5, 1)
        |    len(s)
        |""".stripMargin) shouldBe 5
  }
}
