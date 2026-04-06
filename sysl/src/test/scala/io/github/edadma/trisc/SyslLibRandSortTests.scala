package io.github.edadma.trisc

class SyslLibRandSortTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/stdlib/stdlib" -> readSysl("posix/stdlib/stdlib.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.stdlib.*
       |$main
       |""".stripMargin)

  // ===== rand / srand =====

  "rand returns value in [0, RAND_MAX]" in {
    evalWith(
      """main() -> int
        |    val r = rand()
        |    if 0 <= r <= RAND_MAX then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "srand produces repeatable sequence" in {
    evalWith(
      """main() -> int
        |    srand(42)
        |    val a = rand()
        |    val b = rand()
        |    srand(42)
        |    val c = rand()
        |    val d = rand()
        |    if a == c && b == d then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "different seeds produce different values" in {
    evalWith(
      """main() -> int
        |    srand(1)
        |    val a = rand()
        |    srand(2)
        |    val b = rand()
        |    if a != b then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "rand produces varying values" in {
    evalWith(
      """main() -> int
        |    srand(123)
        |    val a = rand()
        |    val b = rand()
        |    val c = rand()
        |    if a != b || b != c then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "RAND_MAX is 32767" in {
    evalWith(
      """main() -> int = RAND_MAX
        |""".stripMargin) shouldBe 32767
  }
}
