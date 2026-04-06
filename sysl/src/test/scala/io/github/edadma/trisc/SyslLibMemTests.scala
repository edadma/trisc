package io.github.edadma.trisc

class SyslLibMemTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/string/string" -> readSysl("posix/string/string.sysl"),
    "posix/ctype/ctype" -> readSysl("posix/ctype/ctype.sysl"),
    "posix/stdlib/stdlib" -> readSysl("posix/stdlib/stdlib.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.string.*
       |import posix.stdlib.*
       |$main
       |""".stripMargin)

  // ===== memset =====

  "memset fills bytes" in {
    evalWith(
      """main() -> int
        |    buf: [4]byte
        |    memset(buf, 42, 4)
        |    buf[0] + buf[1] + buf[2] + buf[3]
        |""".stripMargin) shouldBe 168
  }

  "memset partial" in {
    evalWith(
      """main() -> int
        |    buf: [4]byte
        |    memset(buf, 0, 4)
        |    memset(buf, 1, 2)
        |    buf[0] + buf[1] + buf[2] + buf[3]
        |""".stripMargin) shouldBe 2
  }

  // ===== memcpy =====

  "memcpy copies bytes" in {
    evalWith(
      """main() -> int
        |    src: [4]byte
        |    dst: [4]byte
        |    src[0] = 10
        |    src[1] = 20
        |    src[2] = 30
        |    src[3] = 40
        |    memcpy(dst, src, 4)
        |    dst[0] + dst[1] + dst[2] + dst[3]
        |""".stripMargin) shouldBe 100
  }

  "memcpy partial" in {
    evalWith(
      """main() -> int
        |    src: [4]byte
        |    dst: [4]byte
        |    src[0] = 10
        |    src[1] = 20
        |    src[2] = 30
        |    src[3] = 40
        |    memcpy(dst, src, 2)
        |    dst[0] + dst[1] + dst[2] + dst[3]
        |""".stripMargin) shouldBe 30
  }

  // ===== memcmp =====

  "memcmp equal" in {
    evalWith(
      """main() -> int
        |    a: [3]byte
        |    b: [3]byte
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    b[0] = 1
        |    b[1] = 2
        |    b[2] = 3
        |    memcmp(a, b, 3)
        |""".stripMargin) shouldBe 0
  }

  "memcmp a less than b" in {
    evalWith(
      """main() -> int
        |    a: [3]byte
        |    b: [3]byte
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    b[0] = 1
        |    b[1] = 5
        |    b[2] = 3
        |    sign(memcmp(a, b, 3))
        |""".stripMargin) shouldBe -1
  }

  "memcmp a greater than b" in {
    evalWith(
      """main() -> int
        |    a: [3]byte
        |    b: [3]byte
        |    a[0] = 9
        |    a[1] = 2
        |    a[2] = 3
        |    b[0] = 1
        |    b[1] = 2
        |    b[2] = 3
        |    sign(memcmp(a, b, 3))
        |""".stripMargin) shouldBe 1
  }

  // ===== strlen =====

  "strlen basic" in {
    evalWith(
      """main() -> int
        |    s: [6]byte
        |    s[0] = 'H'
        |    s[1] = 'e'
        |    s[2] = 'l'
        |    s[3] = 'l'
        |    s[4] = 'o'
        |    s[5] = 0
        |    strlen(s)
        |""".stripMargin) shouldBe 5
  }

  "strlen empty" in {
    evalWith(
      """main() -> int
        |    s: [1]byte
        |    s[0] = 0
        |    strlen(s)
        |""".stripMargin) shouldBe 0
  }
}
