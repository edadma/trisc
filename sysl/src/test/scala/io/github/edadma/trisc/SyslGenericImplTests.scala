package io.github.edadma.trisc

class SyslGenericImplTests extends SyslTestHelpers {

  // ===== Stage F.4 — concrete impl of a multi-param trait =====

  "concrete multi-param trait impl with three trait targets" in {
    eval(
      """trait Concat[A, B, R]
        |    concat(a: A, b: B) -> R
        |
        |impl Concat[int, int, int]
        |    concat(a: int, b: int) -> int = a * 10 + b
        |
        |main() -> int = Concat.concat(3, 7)
        |""".stripMargin) shouldBe 37
  }

  // ===== Generic impl over a single trait type parameter =====

  "generic impl with all tvars in target patterns dispatches" in {
    eval(
      """struct Box[T]
        |    v: T
        |
        |trait Show[T]
        |    showInt(x: T) -> int
        |
        |impl[X] Show[Box[X]]
        |    showInt(x: Box[X]) -> int = 7
        |
        |main() -> int
        |    var b = Box[int](5)
        |    Show.showInt(b)
        |""".stripMargin) shouldBe 7
  }

  "impl with unused type parameter is rejected" in {
    val ex = intercept[Exception] {
      eval(
        """struct Box
          |    v: int
          |
          |trait Show[T]
          |    showInt(x: T) -> int
          |
          |impl[X] Show[Box]
          |    showInt(x: Box) -> int = x.v
          |
          |main() -> int = 0
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.contains("unused type parameter"), s"error should reject unused tvar, got: $msg")
  }

  // ===== Generic impl over a generic struct =====

  "generic impl over Box[T] specializes per-instance" in {
    eval(
      """struct Box[T]
        |    v: T
        |
        |trait Show[T]
        |    showInt(x: T) -> int
        |
        |impl[A] Show[Box[A]]
        |    showInt(x: Box[A]) -> int = 1
        |
        |main() -> int
        |    var b = Box[int](5)
        |    Show.showInt(b)
        |""".stripMargin) shouldBe 1
  }

  // ===== Generic multi-param impl (single impl tvar — multi-arg generic ctor not yet
  // supported in expressions, so we use a single-param Wrap[T]) =====

  "multi-param trait + generic impl over Wrap[T] dispatches" in {
    eval(
      """struct Wrap[T]
        |    v: T
        |
        |trait Combine[X, Y, R]
        |    combine(x: X, y: Y) -> R
        |
        |impl[U] Combine[Wrap[U], int, int]
        |    combine(x: Wrap[U], y: int) -> int = y * 2
        |
        |main() -> int
        |    var w = Wrap[bool](true)
        |    Combine.combine(w, 21)
        |""".stripMargin) shouldBe 42
  }

  // ===== Coherence — overlapping templates rejected =====

  "two overlapping generic impls are an error" in {
    val ex = intercept[Exception] {
      eval(
        """struct Box[T]
          |    v: T
          |
          |trait Show[T]
          |    showInt(x: T) -> int
          |
          |impl[A] Show[Box[A]]
          |    showInt(x: Box[A]) -> int = 1
          |
          |impl[B] Show[Box[B]]
          |    showInt(x: Box[B]) -> int = 2
          |
          |main() -> int = 0
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.contains("overlap"), s"error should mention overlap, got: $msg")
  }

  "concrete impl overlapping with prior generic is an error" in {
    val ex = intercept[Exception] {
      eval(
        """struct Box[T]
          |    v: T
          |
          |trait Show[T]
          |    showInt(x: T) -> int
          |
          |impl[A] Show[Box[A]]
          |    showInt(x: Box[A]) -> int = 1
          |
          |impl Show[Box[int]]
          |    showInt(x: Box[int]) -> int = 2
          |
          |main() -> int = 0
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.contains("overlap"), s"error should mention overlap, got: $msg")
  }

  // ===== Orphan rule (cross-module impl) =====

  "orphan impl across modules is an error" in {
    val ex = intercept[Exception] {
      evalWithLibs(
        Map(
          "mylib/cmp/cmp" ->
            """module mylib.cmp
              |
              |trait Eq[T]
              |    eq(a: T, b: T) -> bool
              |""".stripMargin,
          "mylib/types/types" ->
            """module mylib.types
              |
              |struct Point
              |    x: int
              |""".stripMargin,
          "mylib/orphan/orphan" ->
            """module mylib.orphan
              |
              |import mylib.cmp.*
              |import mylib.types.*
              |
              |impl Eq[Point]
              |    eq(a: Point, b: Point) -> bool = a.x == b.x
              |""".stripMargin,
        ),
        """import mylib.orphan.*
          |main() -> int = 0
          |""".stripMargin
      )
    }
    val msg = ex.getMessage
    assert(msg.contains("orphan"), s"error should mention orphan, got: $msg")
  }

  // ===== Specialization caching =====

  "repeated dispatch on same type reuses one mangled function" in {
    // If specialization were not cached, the analyzer would re-mangle on every
    // call and either collide on a duplicate function or emit two TFunDecls
    // for the same instance. Both manifest as test failure or wrong result.
    eval(
      """struct Box[T]
        |    v: T
        |
        |trait Twice[T]
        |    twice(x: T) -> int
        |
        |impl[A] Twice[Box[A]]
        |    twice(x: Box[A]) -> int = 2
        |
        |main() -> int
        |    var a = Box[int](1)
        |    var b = Box[int](2)
        |    Twice.twice(a) + Twice.twice(b)
        |""".stripMargin) shouldBe 4
  }

  // ===== Operator overloading on a generic impl =====

  "user operator routes through a generic impl" in {
    eval(
      """struct Box[T]
        |    v: T
        |
        |trait Pipe[T]
        |    #operator("|>")
        |    pipe(a: T, b: T) -> T
        |
        |impl[X] Pipe[Box[X]]
        |    pipe(a: Box[X], b: Box[X]) -> Box[X] = a
        |
        |main() -> int
        |    var x = Box[int](3)
        |    var y = Box[int](4)
        |    var r = x |> y
        |    r.v
        |""".stripMargin) shouldBe 3
  }
}
