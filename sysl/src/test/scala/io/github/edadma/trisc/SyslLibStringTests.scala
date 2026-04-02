package io.github.edadma.trisc

class SyslLibStringTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map("string" -> readSysl("posix/lib/string.sysl"))

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import string.*
       |$main
       |""".stripMargin)

  // ===== strcmp =====

  "strcmp equal" in {
    evalWith(
      """main() -> int
        |    a: [4]byte
        |    b: [4]byte
        |    a[0] = 'a'
        |    a[1] = 'b'
        |    a[2] = 'c'
        |    a[3] = 0
        |    b[0] = 'a'
        |    b[1] = 'b'
        |    b[2] = 'c'
        |    b[3] = 0
        |    strcmp(a, b)
        |""".stripMargin) shouldBe 0
  }

  "strcmp a less" in {
    evalWith(
      """sign(x: int) -> int
        |    if x < 0
        |        return -1
        |    if x > 0
        |        return 1
        |    0
        |
        |main() -> int
        |    a: [4]byte
        |    b: [4]byte
        |    a[0] = 'a'
        |    a[1] = 'b'
        |    a[2] = 'c'
        |    a[3] = 0
        |    b[0] = 'a'
        |    b[1] = 'b'
        |    b[2] = 'z'
        |    b[3] = 0
        |    sign(strcmp(a, b))
        |""".stripMargin) shouldBe -1
  }

  "strcmp a greater" in {
    evalWith(
      """sign(x: int) -> int
        |    if x < 0
        |        return -1
        |    if x > 0
        |        return 1
        |    0
        |
        |main() -> int
        |    a: [4]byte
        |    b: [4]byte
        |    a[0] = 'z'
        |    a[1] = 0
        |    a[2] = 0
        |    a[3] = 0
        |    b[0] = 'a'
        |    b[1] = 0
        |    b[2] = 0
        |    b[3] = 0
        |    sign(strcmp(a, b))
        |""".stripMargin) shouldBe 1
  }

  "strcmp different lengths" in {
    evalWith(
      """sign(x: int) -> int
        |    if x < 0
        |        return -1
        |    if x > 0
        |        return 1
        |    0
        |
        |main() -> int
        |    a: [3]byte
        |    b: [4]byte
        |    a[0] = 'a'
        |    a[1] = 'b'
        |    a[2] = 0
        |    b[0] = 'a'
        |    b[1] = 'b'
        |    b[2] = 'c'
        |    b[3] = 0
        |    sign(strcmp(a, b))
        |""".stripMargin) shouldBe -1
  }

  // ===== strncmp =====

  "strncmp equal within n" in {
    evalWith(
      """main() -> int
        |    a: [4]byte
        |    b: [4]byte
        |    a[0] = 'a'
        |    a[1] = 'b'
        |    a[2] = 'x'
        |    a[3] = 0
        |    b[0] = 'a'
        |    b[1] = 'b'
        |    b[2] = 'y'
        |    b[3] = 0
        |    strncmp(a, b, 2)
        |""".stripMargin) shouldBe 0
  }

  "strncmp different within n" in {
    evalWith(
      """sign(x: int) -> int
        |    if x < 0
        |        return -1
        |    if x > 0
        |        return 1
        |    0
        |
        |main() -> int
        |    a: [4]byte
        |    b: [4]byte
        |    a[0] = 'a'
        |    a[1] = 'b'
        |    a[2] = 'x'
        |    a[3] = 0
        |    b[0] = 'a'
        |    b[1] = 'b'
        |    b[2] = 'y'
        |    b[3] = 0
        |    sign(strncmp(a, b, 3))
        |""".stripMargin) shouldBe -1
  }

  // ===== strchr =====

  "strchr found" in {
    evalWith(
      """main() -> int
        |    s: [6]byte
        |    s[0] = 'H'
        |    s[1] = 'e'
        |    s[2] = 'l'
        |    s[3] = 'l'
        |    s[4] = 'o'
        |    s[5] = 0
        |    strchr(s, 'l')
        |""".stripMargin) shouldBe 2
  }

  "strchr not found" in {
    evalWith(
      """main() -> int
        |    s: [4]byte
        |    s[0] = 'a'
        |    s[1] = 'b'
        |    s[2] = 'c'
        |    s[3] = 0
        |    strchr(s, 'z')
        |""".stripMargin) shouldBe -1
  }

  // ===== strrchr =====

  "strrchr finds last" in {
    evalWith(
      """main() -> int
        |    s: [6]byte
        |    s[0] = 'H'
        |    s[1] = 'e'
        |    s[2] = 'l'
        |    s[3] = 'l'
        |    s[4] = 'o'
        |    s[5] = 0
        |    strrchr(s, 'l')
        |""".stripMargin) shouldBe 3
  }

  "strrchr not found" in {
    evalWith(
      """main() -> int
        |    s: [4]byte
        |    s[0] = 'a'
        |    s[1] = 'b'
        |    s[2] = 'c'
        |    s[3] = 0
        |    strrchr(s, 'z')
        |""".stripMargin) shouldBe -1
  }

  // ===== strcpy =====

  "strcpy copies string" in {
    evalWith(
      """main() -> int
        |    src: [4]byte
        |    dst: [4]byte
        |    src[0] = 'H'
        |    src[1] = 'i'
        |    src[2] = '!'
        |    src[3] = 0
        |    strcpy(dst, src)
        |    dst[0] + dst[1] + dst[2]
        |""".stripMargin) shouldBe 'H' + 'i' + '!'
  }

  "strcpy null terminates" in {
    evalWith(
      """main() -> int
        |    src: [3]byte
        |    dst: [4]byte
        |    dst[2] = 99
        |    src[0] = 'A'
        |    src[1] = 'B'
        |    src[2] = 0
        |    strcpy(dst, src)
        |    dst[2]
        |""".stripMargin) shouldBe 0
  }

  // ===== strcat =====

  "strcat appends" in {
    evalWith(
      """main() -> int
        |    dst: [8]byte
        |    src: [3]byte
        |    dst[0] = 'H'
        |    dst[1] = 'i'
        |    dst[2] = 0
        |    src[0] = '!'
        |    src[1] = '!'
        |    src[2] = 0
        |    strcat(dst, src)
        |    dst[2] + dst[3]
        |""".stripMargin) shouldBe '!' + '!'
  }

  // ===== strncpy =====

  "strncpy truncates" in {
    evalWith(
      """main() -> int
        |    src: [6]byte
        |    dst: [4]byte
        |    src[0] = 'H'
        |    src[1] = 'e'
        |    src[2] = 'l'
        |    src[3] = 'l'
        |    src[4] = 'o'
        |    src[5] = 0
        |    strncpy(dst, src, 3)
        |    dst[0] + dst[1] + dst[2]
        |""".stripMargin) shouldBe 'H' + 'e' + 'l'
  }

  "strncpy pads with zeros" in {
    evalWith(
      """main() -> int
        |    src: [3]byte
        |    dst: [6]byte
        |    dst[0] = 99
        |    dst[1] = 99
        |    dst[2] = 99
        |    dst[3] = 99
        |    dst[4] = 99
        |    dst[5] = 99
        |    src[0] = 'A'
        |    src[1] = 'B'
        |    src[2] = 0
        |    strncpy(dst, src, 5)
        |    dst[2] + dst[3] + dst[4]
        |""".stripMargin) shouldBe 0
  }
}
