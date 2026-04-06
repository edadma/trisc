package io.github.edadma.trisc

class SyslLibCaseStrTests extends SyslTestHelpers {

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

  // ===== strcasecmp =====

  "strcasecmp equal" in {
    evalWith(
      """main() -> int
        |    var a: [4]byte
        |    a[0] = 'A'
        |    a[1] = 'B'
        |    a[2] = 'C'
        |    a[3] = 0
        |    var b: [4]byte
        |    b[0] = 'a'
        |    b[1] = 'b'
        |    b[2] = 'c'
        |    b[3] = 0
        |    strcasecmp(a, b)
        |""".stripMargin) shouldBe 0
  }

  "strcasecmp less" in {
    evalWith(
      """main() -> int
        |    var a: [2]byte
        |    a[0] = 'A'
        |    a[1] = 0
        |    var b: [2]byte
        |    b[0] = 'b'
        |    b[1] = 0
        |    sign(strcasecmp(a, b))
        |""".stripMargin) shouldBe -1
  }

  "strcasecmp greater" in {
    evalWith(
      """main() -> int
        |    var a: [2]byte
        |    a[0] = 'z'
        |    a[1] = 0
        |    var b: [2]byte
        |    b[0] = 'A'
        |    b[1] = 0
        |    sign(strcasecmp(a, b))
        |""".stripMargin) shouldBe 1
  }

  "strcasecmp different lengths" in {
    evalWith(
      """main() -> int
        |    var a: [3]byte
        |    a[0] = 'a'
        |    a[1] = 'b'
        |    a[2] = 0
        |    var b: [4]byte
        |    b[0] = 'a'
        |    b[1] = 'b'
        |    b[2] = 'c'
        |    b[3] = 0
        |    sign(strcasecmp(a, b))
        |""".stripMargin) shouldBe -1
  }

  // ===== strncasecmp =====

  "strncasecmp equal within n" in {
    evalWith(
      """main() -> int
        |    var a: [4]byte
        |    a[0] = 'A'
        |    a[1] = 'B'
        |    a[2] = 'x'
        |    a[3] = 0
        |    var b: [4]byte
        |    b[0] = 'a'
        |    b[1] = 'b'
        |    b[2] = 'y'
        |    b[3] = 0
        |    strncasecmp(a, b, 2)
        |""".stripMargin) shouldBe 0
  }

  "strncasecmp different within n" in {
    evalWith(
      """main() -> int
        |    var a: [3]byte
        |    a[0] = 'A'
        |    a[1] = 'X'
        |    a[2] = 0
        |    var b: [3]byte
        |    b[0] = 'a'
        |    b[1] = 'y'
        |    b[2] = 0
        |    sign(strncasecmp(a, b, 2))
        |""".stripMargin) shouldBe -1
  }

  "strncasecmp n=0" in {
    evalWith(
      """main() -> int
        |    var a: [2]byte
        |    a[0] = 'x'
        |    a[1] = 0
        |    var b: [2]byte
        |    b[0] = 'y'
        |    b[1] = 0
        |    strncasecmp(a, b, 0)
        |""".stripMargin) shouldBe 0
  }

  // ===== strcasestr =====

  "strcasestr found" in {
    evalWith(
      """main() -> int
        |    var h: [12]byte
        |    h[0] = 'H'
        |    h[1] = 'e'
        |    h[2] = 'L'
        |    h[3] = 'l'
        |    h[4] = 'O'
        |    h[5] = 0
        |    var n: [3]byte
        |    n[0] = 'l'
        |    n[1] = 'l'
        |    n[2] = 0
        |    val result = strcasestr(h, n)
        |    if result != *byte(0) then int(i64(result) - i64(&h[0]))
        |    else -1
        |""".stripMargin) shouldBe 2
  }

  "strcasestr not found" in {
    evalWith(
      """main() -> int
        |    var h: [4]byte
        |    h[0] = 'a'
        |    h[1] = 'b'
        |    h[2] = 'c'
        |    h[3] = 0
        |    var n: [2]byte
        |    n[0] = 'z'
        |    n[1] = 0
        |    val result = strcasestr(h, n)
        |    if result == *byte(0) then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "strcasestr empty needle" in {
    evalWith(
      """main() -> int
        |    var h: [4]byte
        |    h[0] = 'a'
        |    h[1] = 'b'
        |    h[2] = 'c'
        |    h[3] = 0
        |    var n: [1]byte
        |    n[0] = 0
        |    val result = strcasestr(h, n)
        |    if result != *byte(0) then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "strcasestr at end" in {
    evalWith(
      """main() -> int
        |    var h: [4]byte
        |    h[0] = 'a'
        |    h[1] = 'b'
        |    h[2] = 'C'
        |    h[3] = 0
        |    var n: [2]byte
        |    n[0] = 'c'
        |    n[1] = 0
        |    val result = strcasestr(h, n)
        |    if result != *byte(0) then int(i64(result) - i64(&h[0]))
        |    else -1
        |""".stripMargin) shouldBe 2
  }
}
