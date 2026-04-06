package io.github.edadma.trisc

class SyslLibItoaTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/stdlib/stdlib" -> readSysl("posix/stdlib/stdlib.sysl"),
    "posix/string/string" -> readSysl("posix/string/string.sysl"),
    "posix/ctype/ctype" -> readSysl("posix/ctype/ctype.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.stdlib.*
       |import posix.string.*
       |$main
       |""".stripMargin)

  // ===== itoa =====

  "itoa positive decimal" in {
    evalWith(
      """main() -> int
        |    var buf: [12]byte
        |    itoa(123, buf, 10)
        |    if buf[0] == '1' && buf[1] == '2' && buf[2] == '3' && buf[3] == 0 then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "itoa negative decimal" in {
    evalWith(
      """main() -> int
        |    var buf: [12]byte
        |    itoa(-42, buf, 10)
        |    if buf[0] == '-' && buf[1] == '4' && buf[2] == '2' && buf[3] == 0 then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "itoa zero" in {
    evalWith(
      """main() -> int
        |    var buf: [12]byte
        |    itoa(0, buf, 10)
        |    if buf[0] == '0' && buf[1] == 0 then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "itoa hex" in {
    evalWith(
      """main() -> int
        |    var buf: [12]byte
        |    itoa(255, buf, 16)
        |    if buf[0] == 'f' && buf[1] == 'f' && buf[2] == 0 then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "itoa binary" in {
    evalWith(
      """main() -> int
        |    var buf: [12]byte
        |    itoa(10, buf, 2)
        |    if buf[0] == '1' && buf[1] == '0' && buf[2] == '1' && buf[3] == '0' && buf[4] == 0 then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "itoa octal" in {
    evalWith(
      """main() -> int
        |    var buf: [12]byte
        |    itoa(8, buf, 8)
        |    if buf[0] == '1' && buf[1] == '0' && buf[2] == 0 then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "itoa returns buffer pointer" in {
    evalWith(
      """main() -> int
        |    var buf: [12]byte
        |    val ret = itoa(42, buf, 10)
        |    strlen(ret)
        |""".stripMargin) shouldBe 2
  }

  "itoa roundtrip with atoi" in {
    evalWith(
      """main() -> int
        |    var buf: [12]byte
        |    itoa(9876, buf, 10)
        |    atoi(buf)
        |""".stripMargin) shouldBe 9876
  }
}
