package io.github.edadma.trisc

class SyslLibSwapTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/stdlib/stdlib" -> readSysl("posix/stdlib/stdlib.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.stdlib.*
       |$main
       |""".stripMargin)

  "swap two single bytes" in {
    evalWith(
      """main() -> int
        |    var a: [2]byte
        |    a[0] = 'x'
        |    a[1] = 'y'
        |    swap(&a[0], &a[1], 1)
        |    int(a[0]) * 10 + int(a[1])
        |""".stripMargin) shouldBe 121 * 10 + 120  // 'y'*10 + 'x'
  }

  "swap byte ranges in array" in {
    evalWith(
      """main() -> int
        |    var a: [6]byte
        |    a[0] = 'a'
        |    a[1] = 'b'
        |    a[2] = 'c'
        |    a[3] = 'x'
        |    a[4] = 'y'
        |    a[5] = 'z'
        |    swap(&a[0], &a[3], 3)
        |    if a[0] == 'x' && a[1] == 'y' && a[2] == 'z' && a[3] == 'a' && a[4] == 'b' && a[5] == 'c' then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "swap zero length is no-op" in {
    evalWith(
      """main() -> int
        |    var a: [2]byte
        |    a[0] = 'a'
        |    a[1] = 'b'
        |    swap(&a[0], &a[1], 0)
        |    if a[0] == 'a' && a[1] == 'b' then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }
}
