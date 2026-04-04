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
}
