package io.github.edadma.trisc

/** Additional type attribute tests: ::Value (string → enum) and for-in reverse iteration. */
class SyslTypeAttrExtraTests extends SyslTestHelpers {

  // ===== ::Value — string to enum =====

  "::Value parses variant name" in {
    eval("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |
      |main() -> int = Color::Value("Green")
      |""".stripMargin) shouldBe 1
  }

  "::Value respects explicit values" in {
    eval("""
      |enum Status
      |    Ok = 200
      |    NotFound = 404
      |    Error = 500
      |
      |main() -> int = Status::Value("NotFound")
      |""".stripMargin) shouldBe 404
  }

  "::Value round-trips with ::Image" in {
    eval("""
      |enum Dir
      |    Up
      |    Down
      |    Left
      |    Right
      |
      |main() -> int
      |    var d = Dir.Left
      |    Dir::Value(Dir::Image(d))
      |""".stripMargin) shouldBe 2
  }

  "::Value traps on unknown string" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |enum Color
        |    Red
        |    Green
        |    Blue
        |
        |main() -> int = Color::Value("Purple")
        |""".stripMargin)
    }
    thrown.getMessage should include("no variant matches string")
  }

  "::Value on non-enum fails" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Age = int within 0..150
        |main() -> int = Age::Value("42")
        |""".stripMargin)
    }
    thrown.getMessage should include("simple enum")
  }

  "::Value rejects non-string argument" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |enum Color
        |    Red
        |
        |main() -> int = Color::Value(0)
        |""".stripMargin)
    }
    thrown.getMessage should include("argument must be string")
  }

  // ===== for i in reverse T::Range =====

  "reverse iterates from Last down to First" in {
    eval("""
      |type Small = int within 1..5
      |main() -> int
      |    var first_seen = 0
      |    for i in reverse Small::Range
      |        if first_seen == 0 then first_seen = i
      |    first_seen
      |""".stripMargin) shouldBe 5
  }

  "reverse collects all values in descending order" in {
    output("""
      |type Small = int within 1..3
      |main() -> int
      |    for i in reverse Small::Range
      |        puts(str(i))
      |    0
      |""".stripMargin) shouldBe "321"
  }

  "reverse covers an exclusive-upper range correctly" in {
    eval("""
      |type Idx = int within 0..<4
      |main() -> int
      |    var last = -1
      |    for i in reverse Idx::Range
      |        if last < 0 then last = i
      |    last
      |""".stripMargin) shouldBe 3
  }

  "reverse over an enum yields last variant first" in {
    eval("""
      |enum Dir
      |    Up
      |    Down
      |    Left
      |    Right
      |
      |main() -> int
      |    var first = -1
      |    for i in reverse Dir::Range
      |        if first < 0 then first = i
      |    first
      |""".stripMargin) shouldBe 3
  }

  "reverse over a plain array iterates backward" in {
    output("""
      |main() -> int
      |    var a: [5]int
      |    a[0] = 1; a[1] = 2; a[2] = 3; a[3] = 4; a[4] = 5
      |    for v in reverse a
      |        puts(str(v))
      |    0
      |""".stripMargin) shouldBe "54321"
  }
}
