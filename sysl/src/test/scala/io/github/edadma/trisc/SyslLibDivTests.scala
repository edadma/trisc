package io.github.edadma.trisc

class SyslLibDivTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/stdlib/stdlib" -> readSysl("posix/stdlib/stdlib.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.stdlib.*
       |$main
       |""".stripMargin)

  // ===== div with struct destructuring =====

  "div basic" in {
    evalWith(
      """main() -> int
        |    val result = div(17, 5)
        |    result.quot * 100 + result.rem
        |""".stripMargin) shouldBe 302
  }

  "div with struct destructuring" in {
    evalWith(
      """main() -> int
        |    q, r = div(17, 5)
        |    q * 100 + r
        |""".stripMargin) shouldBe 302
  }

  "div negative" in {
    evalWith(
      """main() -> int
        |    q, r = div(-17, 5)
        |    q * 100 + abs(r)
        |""".stripMargin) shouldBe -300 + 2
  }

  "div exact" in {
    evalWith(
      """main() -> int
        |    q, r = div(20, 5)
        |    q * 10 + r
        |""".stripMargin) shouldBe 40
  }

  // ===== sign with match =====

  "sign negative" in { evalWith("main() -> int = sign(-42)") shouldBe -1 }
  "sign positive" in { evalWith("main() -> int = sign(42)") shouldBe 1 }
  "sign zero" in { evalWith("main() -> int = sign(0)") shouldBe 0 }
}
