package io.github.edadma.trisc

class SyslLibAssertTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/assert/assert" -> readSysl("posix/assert/assert.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.assert.*
       |$main
       |""".stripMargin)

  "assert true passes" in {
    evalWith(
      """main() -> int
        |    assert(true)
        |    42
        |""".stripMargin) shouldBe 42
  }

  "assert expression passes" in {
    evalWith(
      """main() -> int
        |    val x = 10
        |    assert(x > 0)
        |    assert(x == 10)
        |    1
        |""".stripMargin) shouldBe 1
  }

  "assert false aborts" in {
    an[Exception] should be thrownBy evalWith(
      """main() -> int
        |    assert(false)
        |    0
        |""".stripMargin)
  }

  "assert failing condition aborts" in {
    an[Exception] should be thrownBy evalWith(
      """main() -> int
        |    val x = -1
        |    assert(x >= 0)
        |    0
        |""".stripMargin)
  }
}
