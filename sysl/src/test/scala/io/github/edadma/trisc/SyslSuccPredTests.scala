package io.github.edadma.trisc

class SyslSuccPredTests extends SyslTestHelpers {

  // ===== ::Succ / ::Pred on simple enums =====

  "::Succ on enum returns next variant's value" in {
    eval("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |
      |main() -> int = Color::Succ(Color.Red)
      |""".stripMargin) shouldBe 1
  }

  "::Succ from middle variant" in {
    eval("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |
      |main() -> int = Color::Succ(Color.Green)
      |""".stripMargin) shouldBe 2
  }

  "::Succ respects declaration order even with explicit values and gaps" in {
    eval("""
      |enum Status
      |    Ok = 200
      |    NotFound = 404
      |    Error = 500
      |
      |main() -> int = Status::Succ(Status.Ok)
      |""".stripMargin) shouldBe 404
  }

  "::Pred returns previous variant's value" in {
    eval("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |
      |main() -> int = Color::Pred(Color.Blue)
      |""".stripMargin) shouldBe 1
  }

  "::Pred with gaps" in {
    eval("""
      |enum Status
      |    Ok = 200
      |    NotFound = 404
      |    Error = 500
      |
      |main() -> int = Status::Pred(Status.Error)
      |""".stripMargin) shouldBe 404
  }

  "::Succ past last variant traps" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |enum Color
        |    Red
        |    Green
        |    Blue
        |
        |main() -> int = Color::Succ(Color.Blue)
        |""".stripMargin)
    }
    thrown.getMessage should include("no successor")
  }

  "::Pred before first variant traps" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |enum Color
        |    Red
        |    Green
        |    Blue
        |
        |main() -> int = Color::Pred(Color.Red)
        |""".stripMargin)
    }
    thrown.getMessage should include("no predecessor")
  }

  "::Succ and ::Pred are inverses within range" in {
    eval("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |
      |main() -> int = Color::Succ(Color::Pred(Color.Blue))
      |""".stripMargin) shouldBe 2
  }

  // ===== ::Succ / ::Pred on within-constrained ints =====

  "::Succ on within subtype" in {
    eval("""
      |type Age = int within 0..150
      |main() -> int
      |    var a: Age = 42
      |    Age::Succ(a)
      |""".stripMargin) shouldBe 43
  }

  "::Pred on within subtype" in {
    eval("""
      |type Age = int within 0..150
      |main() -> int
      |    var a: Age = 42
      |    Age::Pred(a)
      |""".stripMargin) shouldBe 41
  }

  "::Succ on within subtype at upper bound traps" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Age = int within 0..150
        |main() -> int
        |    var a: Age = 150
        |    Age::Succ(a)
        |""".stripMargin)
    }
    thrown.getMessage should include("upper bound")
  }

  "::Pred on within subtype at lower bound traps" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Age = int within 0..150
        |main() -> int
        |    var a: Age = 0
        |    Age::Pred(a)
        |""".stripMargin)
    }
    thrown.getMessage should include("lower bound")
  }

  "::Succ with exclusive-upper range respects exclusive limit" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type Idx = int within 0..<10
        |main() -> int
        |    var a: Idx = 9
        |    Idx::Succ(a)
        |""".stripMargin)
    }
    thrown.getMessage should include("upper bound")
  }

  // ===== ::Succ / ::Pred rejection on unsuitable types =====

  "::Succ on plain alias fails" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |type MyInt = int
        |main() -> int
        |    var x: MyInt = 5
        |    MyInt::Succ(x)
        |""".stripMargin)
    }
    thrown.getMessage should include("simple enum or range-constrained")
  }
}
