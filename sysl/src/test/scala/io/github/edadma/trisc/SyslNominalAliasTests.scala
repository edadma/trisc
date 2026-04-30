package io.github.edadma.trisc

class SyslNominalAliasTests extends SyslTestHelpers {

  // ===== Parse + register =====

  "nominal generic alias parses and analyzes" in {
    eval(
      """type Wrap[T] = new T
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  "nominal generic alias over a function type" in {
    eval(
      """type Parser[A] = new (int) -> A
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  // ===== Wrap + unwrap =====

  "wrap and unwrap a nominal generic alias instantiation" in {
    eval(
      """type IntBox[T] = new int
        |
        |main() -> int
        |    var b: IntBox[bool] = IntBox[bool](42)
        |    int(b)
        |""".stripMargin) shouldBe 42
  }

  "wrap a function-typed nominal generic alias" in {
    eval(
      """type Parser[A] = new (int) -> A
        |
        |id_i32(x: int) -> int = x
        |
        |main() -> int
        |    var p: Parser[int] = Parser[int](id_i32)
        |    7
        |""".stripMargin) shouldBe 7
  }

  // ===== Nominal distinctness =====

  "different instantiations are distinct types" in {
    val ex = intercept[Exception] {
      eval(
        """type Box[T] = new int
          |
          |take_int_box(b: Box[int]) -> int = int(b)
          |
          |main() -> int
          |    var b: Box[bool] = Box[bool](5)
          |    take_int_box(b)
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.toLowerCase.contains("box") || msg.toLowerCase.contains("type"),
      s"error should mention type mismatch, got: $msg")
  }

  "no implicit conversion from underlying to nominal alias" in {
    val ex = intercept[Exception] {
      eval(
        """type Box[T] = new int
          |
          |main() -> int
          |    var raw: int = 5
          |    var b: Box[bool] = raw
          |    int(b)
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.nonEmpty, s"should reject implicit conversion, got: $msg")
  }

  // ===== Generic-impl integration =====

  "generic impl over a nominal generic alias dispatches" in {
    eval(
      """type Box[T] = new int
        |
        |trait Show[T]
        |    showInt(x: T) -> int
        |
        |impl[A] Show[Box[A]]
        |    showInt(x: Box[A]) -> int = int(x) * 10
        |
        |main() -> int
        |    var b = Box[bool](7)
        |    Show.showInt(b)
        |""".stripMargin) shouldBe 70
  }

  "different alias instantiations get distinct impl specializations" in {
    eval(
      """type Box[T] = new int
        |
        |trait Show[T]
        |    showInt(x: T) -> int
        |
        |impl[A] Show[Box[A]]
        |    showInt(x: Box[A]) -> int = int(x) + 1
        |
        |main() -> int
        |    var a = Box[int](10)
        |    var b = Box[bool](20)
        |    Show.showInt(a) + Show.showInt(b)
        |""".stripMargin) shouldBe 32
  }

  // ===== Negative cases — within / where still rejected =====

  "generic alias with within is rejected" in {
    val ex = intercept[Exception] {
      eval(
        """type Foo[T] = T within 0..10
          |main() -> int = 0
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.contains("within"), s"error should reject within, got: $msg")
  }

  "generic alias with where is rejected" in {
    val ex = intercept[Exception] {
      eval(
        """isPos(x: int) -> bool = x > 0
          |type Foo[T] = T where isPos(value)
          |main() -> int = 0
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.contains("where") || msg.contains("within"),
      s"error should reject predicate, got: $msg")
  }

  "generic alias with new + within is rejected (within is the issue, not new)" in {
    val ex = intercept[Exception] {
      eval(
        """type Foo[T] = new int within 0..10
          |main() -> int = 0
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.contains("within"), s"error should reject within, got: $msg")
  }
}
