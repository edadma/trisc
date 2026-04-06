package io.github.edadma.trisc

class SyslLibAtexitTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/stdlib/stdlib" -> readSysl("posix/stdlib/stdlib.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.stdlib.*
       |$main
       |""".stripMargin)

  private def runWith(main: String): (Long, String) = runWithLibs(libs,
    s"""import posix.stdlib.*
       |$main
       |""".stripMargin)

  "atexit registers successfully" in {
    evalWith(
      """handler()
        |    return
        |
        |main() -> int = atexit(handler)
        |""".stripMargin) shouldBe 0
  }

  "atexit handlers called in reverse order" in {
    val (_, out) = runWith(
      """var order = 0
        |
        |first()
        |    order = order * 10 + 1
        |
        |second()
        |    order = order * 10 + 2
        |
        |third()
        |    order = order * 10 + 3
        |
        |main() -> int
        |    atexit(first)
        |    atexit(second)
        |    atexit(third)
        |    _run_atexit()
        |    print(order)
        |    0
        |""".stripMargin)
    out shouldBe "321"
  }

  "atexit with no handlers is no-op" in {
    evalWith(
      """main() -> int
        |    _run_atexit()
        |    0
        |""".stripMargin) shouldBe 0
  }

  "ATEXIT_MAX is 32" in {
    evalWith("main() -> int = ATEXIT_MAX") shouldBe 32
  }
}
