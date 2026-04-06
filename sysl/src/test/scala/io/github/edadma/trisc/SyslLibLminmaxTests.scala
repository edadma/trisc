package io.github.edadma.trisc

class SyslLibLminmaxTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/stdlib/stdlib" -> readSysl("posix/stdlib/stdlib.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.stdlib.*
       |$main
       |""".stripMargin)

  "lmin first smaller" in { evalWith("main() -> i64 = lmin(10i64, 20i64)") shouldBe 10 }
  "lmin second smaller" in { evalWith("main() -> i64 = lmin(20i64, 10i64)") shouldBe 10 }
  "lmin equal" in { evalWith("main() -> i64 = lmin(5i64, 5i64)") shouldBe 5 }
  "lmin negative" in { evalWith("main() -> i64 = lmin(-10i64, 10i64)") shouldBe -10 }
  "lmax first larger" in { evalWith("main() -> i64 = lmax(20i64, 10i64)") shouldBe 20 }
  "lmax second larger" in { evalWith("main() -> i64 = lmax(10i64, 20i64)") shouldBe 20 }
  "lmax equal" in { evalWith("main() -> i64 = lmax(5i64, 5i64)") shouldBe 5 }
  "lmax negative" in { evalWith("main() -> i64 = lmax(-10i64, 10i64)") shouldBe 10 }
}
