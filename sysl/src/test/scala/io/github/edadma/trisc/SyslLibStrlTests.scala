package io.github.edadma.trisc

class SyslLibStrlTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/string/string" -> readSysl("posix/string/string.sysl"),
    "posix/stdlib/stdlib" -> readSysl("posix/stdlib/stdlib.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.string.*
       |import posix.stdlib.*
       |$main
       |""".stripMargin)

  // ===== strlcpy =====

  "strlcpy normal copy" in {
    evalWith(
      """main() -> int
        |    var src: [4]byte
        |    src[0] = 'a'
        |    src[1] = 'b'
        |    src[2] = 'c'
        |    src[3] = 0
        |    var dst: [10]byte
        |    val ret = strlcpy(dst, src, 10)
        |    ret * 1000 + int(dst[0]) * 100 + int(dst[1]) * 10 + int(dst[2])
        |""".stripMargin) shouldBe 3000 + 97 * 100 + 98 * 10 + 99  // 3 + "abc"
  }

  "strlcpy truncates" in {
    evalWith(
      """main() -> int
        |    var src: [6]byte
        |    src[0] = 'h'
        |    src[1] = 'e'
        |    src[2] = 'l'
        |    src[3] = 'l'
        |    src[4] = 'o'
        |    src[5] = 0
        |    var dst: [4]byte
        |    val ret = strlcpy(dst, src, 4)
        |    ret * 100 + dst[3]
        |""".stripMargin) shouldBe 500  // returns 5 (strlen src), dst[3] = 0 (null terminated)
  }

  "strlcpy returns src length even when truncated" in {
    evalWith(
      """main() -> int
        |    var src: [6]byte
        |    src[0] = 'h'
        |    src[1] = 'e'
        |    src[2] = 'l'
        |    src[3] = 'l'
        |    src[4] = 'o'
        |    src[5] = 0
        |    var dst: [3]byte
        |    strlcpy(dst, src, 3)
        |""".stripMargin) shouldBe 5
  }

  "strlcpy zero dstsize" in {
    evalWith(
      """main() -> int
        |    var src: [4]byte
        |    src[0] = 'a'
        |    src[1] = 'b'
        |    src[2] = 'c'
        |    src[3] = 0
        |    var dst: [1]byte
        |    dst[0] = 'x'
        |    val ret = strlcpy(dst, src, 0)
        |    ret * 10 + int(dst[0])
        |""".stripMargin) shouldBe 30 + 120  // returns 3, dst unchanged ('x'=120)
  }

  "strlcpy copies exactly fitting string" in {
    evalWith(
      """main() -> int
        |    var src: [3]byte
        |    src[0] = 'a'
        |    src[1] = 'b'
        |    src[2] = 0
        |    var dst: [3]byte
        |    val ret = strlcpy(dst, src, 3)
        |    ret * 100 + int(dst[0]) * 10 + int(dst[2])
        |""".stripMargin) shouldBe 200 + 970 + 0  // returns 2, dst = "ab\0"
  }

  // ===== strlcat =====

  "strlcat appends" in {
    evalWith(
      """main() -> int
        |    var dst: [10]byte
        |    dst[0] = 'a'
        |    dst[1] = 'b'
        |    dst[2] = 0
        |    var src: [3]byte
        |    src[0] = 'c'
        |    src[1] = 'd'
        |    src[2] = 0
        |    val ret = strlcat(dst, src, 10)
        |    ret * 10000 + int(dst[0]) * 1000 + int(dst[1]) * 100 + int(dst[2]) * 10 + int(dst[3])
        |""".stripMargin) shouldBe 40000 + 97000 + 98 * 100 + 99 * 10 + 100  // 4, "abcd"
  }

  "strlcat truncates" in {
    evalWith(
      """main() -> int
        |    var dst: [5]byte
        |    dst[0] = 'a'
        |    dst[1] = 'b'
        |    dst[2] = 0
        |    var src: [4]byte
        |    src[0] = 'c'
        |    src[1] = 'd'
        |    src[2] = 'e'
        |    src[3] = 0
        |    val ret = strlcat(dst, src, 5)
        |    ret * 10 + dst[4]
        |""".stripMargin) shouldBe 50 + 0  // returns 5 (2+3), dst[4] = 0 (null terminated)
  }

  "strlcat with full buffer returns dstsize + srclen" in {
    evalWith(
      """main() -> int
        |    var dst: [3]byte
        |    dst[0] = 'a'
        |    dst[1] = 'b'
        |    dst[2] = 'c'
        |    var src: [3]byte
        |    src[0] = 'x'
        |    src[1] = 'y'
        |    src[2] = 0
        |    strlcat(dst, src, 3)
        |""".stripMargin) shouldBe 5  // dstsize(3) + strlen(src)(2) = 5
  }

  "strlcat to empty dst" in {
    evalWith(
      """main() -> int
        |    var dst: [10]byte
        |    dst[0] = 0
        |    var src: [3]byte
        |    src[0] = 'h'
        |    src[1] = 'i'
        |    src[2] = 0
        |    val ret = strlcat(dst, src, 10)
        |    ret * 100 + int(dst[0]) * 10 + int(dst[1])
        |""".stripMargin) shouldBe 200 + 104 * 10 + 105  // returns 2, dst = "hi"
  }
}
