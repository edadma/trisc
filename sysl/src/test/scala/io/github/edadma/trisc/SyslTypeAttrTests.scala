package io.github.edadma.trisc

class SyslTypeAttrTests extends SyslTestHelpers {

  // ===== ::First and ::Last on within-constrained subtypes =====

  "::First on subtype within inclusive range" in {
    eval("""
      |type Age = int within 0..150
      |main() -> int = Age::First
      |""".stripMargin) shouldBe 0
  }

  "::Last on subtype within inclusive range" in {
    eval("""
      |type Age = int within 0..150
      |main() -> int = Age::Last
      |""".stripMargin) shouldBe 150
  }

  "::Last on subtype within exclusive-upper range" in {
    eval("""
      |type Idx = int within 0..<10
      |main() -> int = Idx::Last
      |""".stripMargin) shouldBe 9
  }

  "::First on subtype with negative lower bound" in {
    eval("""
      |type Temperature = int within -40..100
      |main() -> int = Temperature::First
      |""".stripMargin) shouldBe -40
  }

  "::First and ::Last arithmetic" in {
    eval("""
      |type Score = int within 0..100
      |main() -> int = Score::Last - Score::First
      |""".stripMargin) shouldBe 100
  }

  // ===== ::First and ::Last on derived (nominal) types =====

  "::First on derived type returns derived-typed value" in {
    eval("""
      |type Meters = new int within 0..1000
      |main() -> int =
      |    var m: Meters = Meters::First
      |    int(m)
      |""".stripMargin) shouldBe 0
  }

  "::Last on derived type returns derived-typed value" in {
    eval("""
      |type Meters = new int within 0..1000
      |main() -> int =
      |    var m: Meters = Meters::Last
      |    int(m)
      |""".stripMargin) shouldBe 1000
  }

  // ===== ::First and ::Last on simple enums =====

  "::First on simple enum (auto values)" in {
    eval("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |
      |main() -> int = Color::First
      |""".stripMargin) shouldBe 0
  }

  "::Last on simple enum (auto values)" in {
    eval("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |
      |main() -> int = Color::Last
      |""".stripMargin) shouldBe 2
  }

  "::First and ::Last on simple enum with explicit values" in {
    eval("""
      |enum Status
      |    Ok = 200
      |    NotFound = 404
      |    Error = 500
      |
      |main() -> int = Status::First + Status::Last
      |""".stripMargin) shouldBe 700
  }

  // ===== ::Range in for-in loop =====

  "::Range iterates within-constrained subtype inclusively" in {
    eval("""
      |type Small = int within 1..5
      |main() -> int
      |    var sum = 0
      |    for i in Small::Range
      |        sum = sum + i
      |    sum
      |""".stripMargin) shouldBe 15
  }

  "::Range iterates with exclusive-upper subtype" in {
    eval("""
      |type Idx = int within 0..<5
      |main() -> int
      |    var sum = 0
      |    for i in Idx::Range
      |        sum = sum + i
      |    sum
      |""".stripMargin) shouldBe 10
  }

  "::Range iterates simple enum values" in {
    eval("""
      |enum Dir
      |    Up
      |    Down
      |    Left
      |    Right
      |
      |main() -> int
      |    var count = 0
      |    for i in Dir::Range
      |        count = count + 1
      |    count
      |""".stripMargin) shouldBe 4
  }

  // ===== ::Image — string representation =====

  "::Image on simple enum returns variant name" in {
    output("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |
      |main() -> int
      |    var c = Color.Green
      |    puts(Color::Image(c))
      |    0
      |""".stripMargin) shouldBe "Green"
  }

  "::Image on simple enum works for each variant" in {
    output("""
      |enum Dir
      |    Up
      |    Down
      |    Left
      |    Right
      |
      |main() -> int
      |    puts(Dir::Image(Dir.Up))
      |    puts(Dir::Image(Dir.Down))
      |    puts(Dir::Image(Dir.Left))
      |    puts(Dir::Image(Dir.Right))
      |    0
      |""".stripMargin) shouldBe "UpDownLeftRight"
  }

  "::Image on simple enum with explicit values" in {
    output("""
      |enum Status
      |    Ok = 200
      |    NotFound = 404
      |
      |main() -> int
      |    puts(Status::Image(Status.NotFound))
      |    0
      |""".stripMargin) shouldBe "NotFound"
  }

  "::Image on within-constrained int delegates to str()" in {
    output("""
      |type Age = int within 0..150
      |main() -> int
      |    var a: Age = 42
      |    puts(Age::Image(a))
      |    0
      |""".stripMargin) shouldBe "42"
  }

  // ===== ::Pos — declaration position =====

  "::Pos on first variant is 0" in {
    eval("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |
      |main() -> int = Color::Pos(Color.Red)
      |""".stripMargin) shouldBe 0
  }

  "::Pos on last variant is n-1" in {
    eval("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |
      |main() -> int = Color::Pos(Color.Blue)
      |""".stripMargin) shouldBe 2
  }

  "::Pos respects declaration order when values have gaps" in {
    eval("""
      |enum Status
      |    Ok = 200
      |    NotFound = 404
      |    Error = 500
      |
      |main() -> int = Status::Pos(Status.NotFound)
      |""".stripMargin) shouldBe 1
  }

  // ===== ::Val — value at position =====

  "::Val at position 0 is first variant's value" in {
    eval("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |
      |main() -> int = Color::Val(0)
      |""".stripMargin) shouldBe 0
  }

  "::Val at last position is last variant's value" in {
    eval("""
      |enum Status
      |    Ok = 200
      |    NotFound = 404
      |    Error = 500
      |
      |main() -> int = Status::Val(2)
      |""".stripMargin) shouldBe 500
  }

  "::Val traps on out-of-range position" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |enum Color
        |    Red
        |    Green
        |    Blue
        |
        |main() -> int = Color::Val(5)
        |""".stripMargin)
    }
    thrown.getMessage should include("out-of-range position")
    thrown.getMessage should include("Color")
  }

  "::Pos traps on unknown enum value" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |enum Color
        |    Red
        |    Green
        |    Blue
        |
        |main() -> int
        |    var x = 99
        |    Color::Pos(x)
        |""".stripMargin)
    }
    thrown.getMessage should include("invalid enum value")
    thrown.getMessage should include("Color")
  }

  // ===== Pos/Val round-trip =====

  "::Val and ::Pos are inverses" in {
    eval("""
      |enum Status
      |    Ok = 200
      |    NotFound = 404
      |    Error = 500
      |
      |main() -> int
      |    var v = Status.NotFound
      |    Status::Val(Status::Pos(v))
      |""".stripMargin) shouldBe 404
  }

  // ===== Error cases =====

  "::First on plain alias (no range) fails" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type MyInt = int
        |main() -> int = MyInt::First
        |""".stripMargin)
    }
    thrown.getMessage should include("range-constrained")
  }

  "::First with an argument fails" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Age = int within 0..150
        |main() -> int = Age::First(42)
        |""".stripMargin)
    }
    thrown.getMessage should include("takes no arguments")
  }

  "::Image without an argument fails" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |enum Color
        |    Red
        |
        |main() -> int
        |    var s = Color::Image
        |    0
        |""".stripMargin)
    }
    thrown.getMessage should include("requires one argument")
  }

  "unknown attribute fails" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Age = int within 0..150
        |main() -> int = Age::Bogus
        |""".stripMargin)
    }
    thrown.getMessage should include("unknown type attribute")
  }

  "::Range used outside for loop fails" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Age = int within 0..150
        |main() -> int
        |    var x = Age::Range
        |    0
        |""".stripMargin)
    }
    thrown.getMessage should include("::Range")
  }

  "attribute on non-type fails" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |main() -> int = Unknown::First
        |""".stripMargin)
    }
    thrown.getMessage should include("not a type with attributes")
  }

  "::Pos on a within-constrained type fails" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Age = int within 0..150
        |main() -> int
        |    var a: Age = 42
        |    Age::Pos(a)
        |""".stripMargin)
    }
    thrown.getMessage should include("simple enum")
  }
}
