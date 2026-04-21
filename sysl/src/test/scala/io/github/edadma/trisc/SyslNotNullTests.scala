package io.github.edadma.trisc

class SyslNotNullTests extends SyslTestHelpers {

  "not null accepts valid pointer" in {
    eval("""
      |main() -> int =
      |    var n = 42
      |    var p: *int not null = &n
      |    *p
      |""".stripMargin) shouldBe 42
  }

  "not null traps on zero/null assignment at runtime" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |main() -> int =
        |    var src: *int = *int(0)
        |    var p: *int not null = src
        |    *p
        |""".stripMargin)
    }
    thrown.getMessage should include("type predicate")
  }

  "not null parameter accepts valid pointer" in {
    eval("""
      |use(p: *int not null) -> int = *p + 1
      |main() -> int =
      |    var n = 41
      |    use(&n)
      |""".stripMargin) shouldBe 42
  }

  "not null parameter traps on null arg" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |use(p: *int not null) -> int = *p
        |main() -> int =
        |    var src: *int = *int(0)
        |    use(src)
        |""".stripMargin)
    }
    thrown.getMessage should include("type predicate")
  }

  "not null value is pointer-compatible (read-through)" in {
    // A `*int not null` can be used wherever `*int` is expected.
    eval("""
      |takePtr(p: *int) -> int = *p
      |main() -> int =
      |    var n = 7
      |    var nn: *int not null = &n
      |    takePtr(nn)
      |""".stripMargin) shouldBe 7
  }
}
