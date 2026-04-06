package io.github.edadma.trisc

class SyslLibStrrevSepTests extends SyslTestHelpers {

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

  // ===== strrev =====

  "strrev reverses abc" in {
    evalWith(
      """main() -> int
        |    var s: [4]byte
        |    s[0] = 'a'
        |    s[1] = 'b'
        |    s[2] = 'c'
        |    s[3] = 0
        |    strrev(s)
        |    if s[0] == 'c' && s[1] == 'b' && s[2] == 'a' then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "strrev single char" in {
    evalWith(
      """main() -> int
        |    var s: [2]byte
        |    s[0] = 'x'
        |    s[1] = 0
        |    strrev(s)
        |    if s[0] == 'x' then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "strrev empty string" in {
    evalWith(
      """main() -> int
        |    var s: [1]byte
        |    s[0] = 0
        |    strrev(s)
        |    if s[0] == 0 then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "strrev even length" in {
    evalWith(
      """main() -> int
        |    var s: [5]byte
        |    s[0] = 'a'
        |    s[1] = 'b'
        |    s[2] = 'c'
        |    s[3] = 'd'
        |    s[4] = 0
        |    strrev(s)
        |    if s[0] == 'd' && s[1] == 'c' && s[2] == 'b' && s[3] == 'a' then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "strrev returns pointer" in {
    evalWith(
      """main() -> int
        |    var s: [4]byte
        |    s[0] = 'a'
        |    s[1] = 'b'
        |    s[2] = 'c'
        |    s[3] = 0
        |    val ret = strrev(s)
        |    strlen(ret)
        |""".stripMargin) shouldBe 3
  }

  "strrev roundtrip" in {
    evalWith(
      """main() -> int
        |    var s: [4]byte
        |    s[0] = 'a'
        |    s[1] = 'b'
        |    s[2] = 'c'
        |    s[3] = 0
        |    strrev(s)
        |    strrev(s)
        |    if s[0] == 'a' && s[1] == 'b' && s[2] == 'c' then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== strsep =====

  "strsep splits on comma" in {
    evalWith(
      """main() -> int
        |    var s: [8]byte
        |    s[0] = 'a'
        |    s[1] = ','
        |    s[2] = 'b'
        |    s[3] = ','
        |    s[4] = 'c'
        |    s[5] = 0
        |    var delim: [2]byte
        |    delim[0] = ','
        |    delim[1] = 0
        |    var p: *byte = s
        |    val t1 = strsep(&p, delim)
        |    val t2 = strsep(&p, delim)
        |    val t3 = strsep(&p, delim)
        |    if *t1 == 'a' && *t2 == 'b' && *t3 == 'c' then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "strsep returns null after exhausted" in {
    evalWith(
      """main() -> int
        |    var s: [4]byte
        |    s[0] = 'a'
        |    s[1] = ','
        |    s[2] = 'b'
        |    s[3] = 0
        |    var delim: [2]byte
        |    delim[0] = ','
        |    delim[1] = 0
        |    var p: *byte = s
        |    strsep(&p, delim)
        |    strsep(&p, delim)
        |    val t3 = strsep(&p, delim)
        |    if t3 == *byte(0) then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "strsep no delimiter" in {
    evalWith(
      """main() -> int
        |    var s: [4]byte
        |    s[0] = 'a'
        |    s[1] = 'b'
        |    s[2] = 'c'
        |    s[3] = 0
        |    var delim: [2]byte
        |    delim[0] = ','
        |    delim[1] = 0
        |    var p: *byte = s
        |    val t1 = strsep(&p, delim)
        |    if *t1 == 'a' && strlen(t1) == 3 && p == *byte(0) then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "strsep null input" in {
    evalWith(
      """main() -> int
        |    var delim: [2]byte
        |    delim[0] = ','
        |    delim[1] = 0
        |    var p: *byte = *byte(0)
        |    val t = strsep(&p, delim)
        |    if t == *byte(0) then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "strsep multiple delimiters" in {
    evalWith(
      """main() -> int
        |    var s: [6]byte
        |    s[0] = 'a'
        |    s[1] = ','
        |    s[2] = 'b'
        |    s[3] = ':'
        |    s[4] = 'c'
        |    s[5] = 0
        |    var delim: [3]byte
        |    delim[0] = ','
        |    delim[1] = ':'
        |    delim[2] = 0
        |    var p: *byte = s
        |    val t1 = strsep(&p, delim)
        |    val t2 = strsep(&p, delim)
        |    val t3 = strsep(&p, delim)
        |    if *t1 == 'a' && *t2 == 'b' && *t3 == 'c' then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }
}
