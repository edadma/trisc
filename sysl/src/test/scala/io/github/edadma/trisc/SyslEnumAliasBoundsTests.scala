package io.github.edadma.trisc

/** Phase C follow-up: trait bounds on enum and type-alias type-parameters,
 *  plus explicit multi-type-arg call expressions `f[T1, T2, ...](args)`.
 *  Mirrors `SyslTraitBoundsTests` (which covers struct + fn type-param bounds)
 *  for the missing carriers. */
class SyslEnumAliasBoundsTests extends SyslTestHelpers {

  private val ordTrait =
    """trait Ord[T]
      |    cmp(a: T, b: T) -> int
      |""".stripMargin

  private val ordImplDollar =
    """struct Dollar
      |    cents: int
      |
      |impl Ord[Dollar]
      |    cmp(a: Dollar, b: Dollar) -> int = a.cents - b.cents
      |""".stripMargin

  // ===== GAP A — bounds on enum type-parameters =====

  "enum with bound parses" in {
    eval(
      ordTrait +
      """enum Box[T: Ord]
        |    Some(v: T)
        |    None
        |
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  "enum with bound, satisfied by user type" in {
    eval(
      ordTrait + ordImplDollar +
      """enum Box[T: Ord]
        |    Some(v: T)
        |    None
        |
        |main() -> int
        |    val b = Some(Dollar(42))
        |    b match
        |        Some(d) -> d.cents
        |        None -> 0
        |""".stripMargin) shouldBe 42
  }

  "enum with bound, instantiation with unsatisfied type fails" in {
    an[Exception] should be thrownBy eval(
      ordTrait +
      """struct Other
        |    v: int
        |
        |enum Box[T: Ord]
        |    Some(v: T)
        |    None
        |
        |main() -> int
        |    val b: Box[Other] = Some(Other(1))
        |    0
        |""".stripMargin)
  }

  "enum with multiple type params, mixed bounds" in {
    eval(
      ordTrait + ordImplDollar +
      """enum Pair[A: Ord, B]
        |    Both(a: A, b: B)
        |    Neither
        |
        |main() -> int
        |    val p = Both(Dollar(7), 3)
        |    p match
        |        Both(d, n) -> d.cents + n
        |        Neither -> 0
        |""".stripMargin) shouldBe 10
  }

  // ===== GAP B — bounds on type-alias type-parameters =====

  "transparent alias with bound parses" in {
    eval(
      ordTrait +
      """type Cmp[T: Ord] = (T, T) -> int
        |
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  "nominal alias with bound parses" in {
    eval(
      ordTrait +
      """type Cmp[T: Ord] = new (T, T) -> int
        |
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  "alias with bound, instantiation with unsatisfied type fails" in {
    an[Exception] should be thrownBy eval(
      ordTrait +
      """struct Other
        |    v: int
        |
        |type Cmp[T: Ord] = (T, T) -> int
        |
        |main() -> int
        |    val c: Cmp[Other] = (a, b) -> 0
        |    0
        |""".stripMargin)
  }

  "alias with multiple type params, mixed bounds" in {
    eval(
      ordTrait + ordImplDollar +
      """type Pick[A: Ord, B] = (A, A, B) -> A
        |
        |go(a: Dollar, b: Dollar, tag: int) -> Dollar
        |    if a.cents > b.cents then a else b
        |
        |main() -> int
        |    val f: Pick[Dollar, int] = go
        |    val r = f(Dollar(5), Dollar(11), 0)
        |    r.cents
        |""".stripMargin) shouldBe 11
  }

  // ===== GAP C — explicit multi-type-arg call expressions =====

  "explicit multi-type-arg fn call, two type args" in {
    eval(
      """f[A, B](a: A, b: B) -> int = 0
        |
        |main() -> int = f[int, int](1, 2)
        |""".stripMargin) shouldBe 0
  }

  "explicit multi-type-arg fn call returns first" in {
    eval(
      """first[A, B](a: A, b: B) -> A = a
        |
        |main() -> int = first[int, int](7, 99)
        |""".stripMargin) shouldBe 7
  }

  "explicit multi-type-arg fn call, three type args" in {
    eval(
      """tripick[A, B, C](a: A, b: B, c: C) -> A = a
        |
        |main() -> int = tripick[int, int, int](42, 0, 0)
        |""".stripMargin) shouldBe 42
  }

  "explicit multi-type-arg struct constructor" in {
    eval(
      """struct Pair[A, B]
        |    first: A
        |    second: B
        |
        |main() -> int
        |    val p = Pair[int, int](10, 20)
        |    p.first + p.second
        |""".stripMargin) shouldBe 30
  }

  "single-type-arg fn call still works (regression)" in {
    eval(
      """id[T](x: T) -> T = x
        |
        |main() -> int = id[int](7)
        |""".stripMargin) shouldBe 7
  }

  // ===== Cross-module — bounds and explicit multi-type-arg calls survive sibling-meta path =====

  "bounded enum imported across modules" in {
    // Mirrors SyslAssocTypesTests' cross-module assoc-type pattern. Trait + bound-
    // satisfying impl + the bounded enum all live in `boxlib`; main imports and
    // constructs/destructures.
    val libs = Map(
      "boxlib/box" ->
        """module boxlib
          |
          |trait Ord[T]
          |    cmp(a: T, b: T) -> int
          |
          |struct Coin
          |    cents: int
          |
          |impl Ord[Coin]
          |    cmp(a: Coin, b: Coin) -> int = a.cents - b.cents
          |
          |enum Maybe[T: Ord]
          |    Just(v: T)
          |    Nothing
          |""".stripMargin,
    )
    evalWithLibs(libs,
      """import boxlib.*
        |
        |main() -> int
        |    val m = Just(Coin(13))
        |    m match
        |        Just(c) -> c.cents
        |        Nothing -> 0
        |""".stripMargin) shouldBe 13
  }

  "bounded type-alias imported across modules" in {
    val libs = Map(
      "cmplib/cmp" ->
        """module cmplib
          |
          |trait Ord[T]
          |    cmp(a: T, b: T) -> int
          |
          |struct Coin
          |    cents: int
          |
          |impl Ord[Coin]
          |    cmp(a: Coin, b: Coin) -> int = a.cents - b.cents
          |
          |type Comparator[T: Ord] = (T, T) -> int
          |""".stripMargin,
    )
    evalWithLibs(libs,
      """import cmplib.*
        |
        |go(a: Coin, b: Coin) -> int = Ord.cmp(a, b)
        |
        |main() -> int
        |    val c: Comparator[Coin] = go
        |    c(Coin(20), Coin(7))
        |""".stripMargin) shouldBe 13
  }

  "explicit multi-type-arg fn call survives module imports" in {
    val libs = Map(
      "twolib/two" ->
        """module twolib
          |
          |first[A, B](a: A, b: B) -> A = a
          |""".stripMargin,
    )
    evalWithLibs(libs,
      """import twolib.*
        |
        |main() -> int = first[int, int](21, 99)
        |""".stripMargin) shouldBe 21
  }
}
