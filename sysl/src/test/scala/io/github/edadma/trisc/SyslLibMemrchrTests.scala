package io.github.edadma.trisc

class SyslLibMemrchrTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/string/string" -> readSysl("posix/string/string.sysl"),
    "posix/stdlib/stdlib" -> readSysl("posix/stdlib/stdlib.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.string.*
       |import posix.stdlib.*
       |$main
       |""".stripMargin)

  "memrchr finds last occurrence" in {
    evalWith(
      """main() -> int
        |    var s: [6]byte
        |    s[0] = 'a'
        |    s[1] = 'b'
        |    s[2] = 'c'
        |    s[3] = 'b'
        |    s[4] = 'a'
        |    s[5] = 0
        |    val result = memrchr(s, 'b', 5)
        |    if result == *byte(0) then -1
        |    else int(i64(result) - i64(&s[0]))
        |""".stripMargin) shouldBe 3
  }

  "memrchr finds first when only one" in {
    evalWith(
      """main() -> int
        |    var s: [4]byte
        |    s[0] = 'a'
        |    s[1] = 'b'
        |    s[2] = 'c'
        |    s[3] = 0
        |    val result = memrchr(s, 'b', 3)
        |    if result == *byte(0) then -1
        |    else int(i64(result) - i64(&s[0]))
        |""".stripMargin) shouldBe 1
  }

  "memrchr not found" in {
    evalWith(
      """main() -> int
        |    var s: [4]byte
        |    s[0] = 'a'
        |    s[1] = 'b'
        |    s[2] = 'c'
        |    s[3] = 0
        |    val result = memrchr(s, 'z', 3)
        |    if result == *byte(0) then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "memrchr finds at position 0" in {
    evalWith(
      """main() -> int
        |    var s: [3]byte
        |    s[0] = 'x'
        |    s[1] = 'y'
        |    s[2] = 'z'
        |    val result = memrchr(s, 'x', 3)
        |    if result == *byte(0) then -1
        |    else int(i64(result) - i64(&s[0]))
        |""".stripMargin) shouldBe 0
  }

  "memrchr finds at last position" in {
    evalWith(
      """main() -> int
        |    var s: [3]byte
        |    s[0] = 'x'
        |    s[1] = 'y'
        |    s[2] = 'z'
        |    val result = memrchr(s, 'z', 3)
        |    if result == *byte(0) then -1
        |    else int(i64(result) - i64(&s[0]))
        |""".stripMargin) shouldBe 2
  }

  "memrchr with n=0 returns null" in {
    evalWith(
      """main() -> int
        |    var s: [3]byte
        |    s[0] = 'a'
        |    s[1] = 'b'
        |    s[2] = 'c'
        |    val result = memrchr(s, 'a', 0)
        |    if result == *byte(0) then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }
}
