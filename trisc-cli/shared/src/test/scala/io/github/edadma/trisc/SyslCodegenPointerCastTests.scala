package io.github.edadma.trisc

class SyslCodegenPointerCastTests extends SyslCodegenHelpers {

  "same-type pointer assignment" in {
    compileAndRun("main() -> int\n    x = 42\n    var p: *int = &x\n    *p\n") shouldBe 42
  }

  "pointer to int read through copy" in {
    compileAndRun("main() -> int\n    x = 42\n    var p1: *int = &x\n    var p2: *int = p1\n    *p2\n") shouldBe 42
  }

  "read struct through *int pointer" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    var p: Point
        |    p.x = 42
        |    var raw: *int = &p
        |    *raw
        |""".stripMargin) shouldBe 42
  }

  "cast *i8 to *byte" in {
    compileAndRun(
      """main() -> int
        |    var x: i8 = 42
        |    var p: *i8 = &x
        |    var q: *byte = *byte(p)
        |    int(*q)
        |""".stripMargin) shouldBe 42
  }

  "non-null pointer to bool" in {
    compileAndRun(
      """main() -> int
        |    var x = 42
        |    var p = &x
        |    if bool(p) then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "null pointer to bool" in {
    compileAndRun(
      """main() -> int
        |    var p = *int(0)
        |    if bool(p) then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "func to bool" in {
    compileAndRun(
      """helper() -> int = 42
        |
        |main() -> int
        |    val f: func() -> int = helper
        |    if bool(f) then 1 else 0
        |""".stripMargin) shouldBe 1
  }
}
