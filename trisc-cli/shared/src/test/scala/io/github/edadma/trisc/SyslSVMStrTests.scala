package io.github.edadma.trisc

class SyslSVMStrTests extends SyslSVMCodegenHelpers {

  // The test pattern: build a string via str(), then return its length so we
  // get an integer assertion rather than depending on string-equality runtime
  // detail. We pair this with checks that read individual bytes via the
  // string's data pointer.

  "str(int) length" in {
    compileAndRun(
      """main() -> i64
        |    val s = str(42)
        |    len(s)
        |""".stripMargin) shouldBe 2
  }

  "str(int) negative length" in {
    compileAndRun(
      """main() -> i64
        |    val s = str(-7)
        |    len(s)
        |""".stripMargin) shouldBe 2
  }

  "str(int) zero" in {
    compileAndRun(
      """main() -> i64
        |    val s = str(0)
        |    len(s)
        |""".stripMargin) shouldBe 1
  }

  "str(int) large positive" in {
    compileAndRun(
      """main() -> i64
        |    val s = str(123456789i64)
        |    len(s)
        |""".stripMargin) shouldBe 9
  }

  "str(bool) true" in {
    compileAndRun(
      """main() -> i64
        |    val s = str(true)
        |    len(s)
        |""".stripMargin) shouldBe 4
  }

  "str(bool) false" in {
    compileAndRun(
      """main() -> i64
        |    val s = str(false)
        |    len(s)
        |""".stripMargin) shouldBe 5
  }

  "str(int) digits are correct" in {
    // Read the bytes back: "42" → '4'=52, '2'=50
    compileAndRun(
      """main() -> i64
        |    val s = str(42)
        |    val first = i64(s[0])
        |    val second = i64(s[1])
        |    first * 1000i64 + second
        |""".stripMargin) shouldBe (52 * 1000 + 50)
  }

  "str(int) negative sign" in {
    // "-7" → '-'=45, '7'=55
    compileAndRun(
      """main() -> i64
        |    val s = str(-7)
        |    val first = i64(s[0])
        |    val second = i64(s[1])
        |    first * 1000i64 + second
        |""".stripMargin) shouldBe (45 * 1000 + 55)
  }

  "str(bool) true content" in {
    // "true" → 't'=116
    compileAndRun(
      """main() -> i64
        |    val s = str(true)
        |    i64(s[0])
        |""".stripMargin) shouldBe 116
  }

  "str + string concatenation" in {
    compileAndRun(
      """main() -> i64
        |    val s = "ans=" + str(42)
        |    len(s)
        |""".stripMargin) shouldBe 6
  }
}
