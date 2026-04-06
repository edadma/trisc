package io.github.edadma.trisc

class SyslLibLabsTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/stdlib/stdlib" -> readSysl("posix/stdlib/stdlib.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.stdlib.*
       |$main
       |""".stripMargin)

  "labs positive" in { evalWith("main() -> i64 = labs(42i64)") shouldBe 42 }
  "labs negative" in { evalWith("main() -> i64 = labs(-42i64)") shouldBe 42 }
  "labs zero" in { evalWith("main() -> i64 = labs(0i64)") shouldBe 0 }
  "labs large negative" in { evalWith("main() -> i64 = labs(-100000i64)") shouldBe 100000 }
}
