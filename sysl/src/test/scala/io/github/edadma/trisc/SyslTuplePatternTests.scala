package io.github.edadma.trisc

class SyslTuplePatternTests extends SyslTestHelpers {

  // ===== Top-level tuple pattern on a tuple-typed scrutinee =====

  "tuple pattern on bare tuple value binds elements" in {
    eval(
      """main() -> int
        |    var t: (int, int) = (10, 32)
        |    t match
        |        (a, b) -> a + b
        |""".stripMargin) shouldBe 42
  }

  "tuple pattern with wildcard element" in {
    eval(
      """main() -> int
        |    var t: (int, int) = (7, 35)
        |    t match
        |        (a, _) -> a
        |""".stripMargin) shouldBe 7
  }

  // ===== Nested tuple pattern inside a variant pattern =====

  "nested tuple pattern in variant — user's seq case" in {
    eval(
      """enum Result
        |    Ok(p: (int, int))
        |    Err(m: int)
        |
        |main() -> int
        |    val r = Ok((10, 32))
        |    r match
        |        Ok((a, b)) -> a + b
        |        Err(_) -> 0
        |""".stripMargin) shouldBe 42
  }

  "nested tuple pattern in variant with secondary plain field" in {
    eval(
      """enum Pair
        |    P(t: (int, int), n: int)
        |
        |main() -> int
        |    val p = P((1, 2), 39)
        |    p match
        |        P((a, b), n) -> a + b + n
        |""".stripMargin) shouldBe 42
  }

  // ===== Nested tuple pattern inside a struct destructure =====

  "nested tuple pattern in struct destructure" in {
    eval(
      """struct Holder
        |    pair: (int, int)
        |    n: int
        |
        |main() -> int
        |    val h = Holder((10, 20), 12)
        |    h match
        |        Holder((a, b), n) -> a + b + n
        |""".stripMargin) shouldBe 42
  }

  // ===== Wildcards inside nested tuple pattern =====

  "wildcard inside nested tuple pattern" in {
    eval(
      """enum Wrap
        |    W(t: (int, int))
        |
        |main() -> int
        |    val w = W((42, 99))
        |    w match
        |        W((a, _)) -> a
        |""".stripMargin) shouldBe 42
  }

  // ===== Generic enum + tuple field type-arg (parsyl seq style) =====

  "tuple-typed variant payload from generic instantiation" in {
    eval(
      """enum Box[T]
        |    Some(v: T)
        |    Empty
        |
        |main() -> int
        |    val b: Box[(int, int)] = Some((20, 22))
        |    b match
        |        Some((a, b)) -> a + b
        |        Empty -> 0
        |""".stripMargin) shouldBe 42
  }
}
