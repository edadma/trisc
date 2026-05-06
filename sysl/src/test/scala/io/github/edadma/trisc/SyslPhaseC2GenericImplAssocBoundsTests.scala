package io.github.edadma.trisc

/** Phase C.2 — bound enforcement on generic-impl associated-type bindings.
 *
 *  The shape:
 *    trait Reader[I]
 *        type Token: Eq                       // bound on assoc type
 *        head(i: I) -> Self::Token
 *
 *    impl[T] Reader[Box[T]]
 *        type Token = T                       // binding to impl tvar — defer check
 *        head(b: Box[T]) -> T = b.v
 *
 *  Concrete impls can be checked at registration. Generic impls must defer
 *  to dispatch time, when `subst[T]` is known. Mirrors item B's
 *  `instantiateImpl` extension. */
class SyslPhaseC2GenericImplAssocBoundsTests extends SyslTestHelpers {

  // Minimal Eq trait + impl-friendly target types.
  private val eqTrait =
    """trait Eq[T]
      |    eq(a: T, b: T) -> bool
      |""".stripMargin

  private val eqImplCoin =
    """struct Coin
      |    cents: int
      |
      |impl Eq[Coin]
      |    eq(a: Coin, b: Coin) -> bool = a.cents == b.cents
      |""".stripMargin

  // ===== Parses =====

  "generic impl with assoc-type bound + tvar binding parses" in {
    eval(
      eqTrait +
      """struct Box[T]
        |    v: T
        |
        |trait Reader[I]
        |    type Token: Eq
        |    head(i: I) -> Self::Token
        |
        |impl[T] Reader[Box[T]]
        |    type Token = T
        |    head(b: Box[T]) -> T = b.v
        |
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  // ===== Satisfied at dispatch =====

  "generic impl assoc-bound satisfied — Coin impls Eq" in {
    eval(
      eqTrait + eqImplCoin +
      """struct Box[T]
        |    v: T
        |
        |trait Reader[I]
        |    type Token: Eq
        |    head(i: I) -> Self::Token
        |
        |impl[T] Reader[Box[T]]
        |    type Token = T
        |    head(b: Box[T]) -> T = b.v
        |
        |main() -> int
        |    val b = Box(Coin(99))
        |    val r = Reader.head(b)
        |    r.cents
        |""".stripMargin) shouldBe 99
  }

  // ===== Unsatisfied at dispatch =====

  "generic impl assoc-bound unsatisfied — NotEq doesn't impl Eq" in {
    an[Exception] should be thrownBy eval(
      eqTrait +
      """struct Box[T]
        |    v: T
        |
        |struct NotEq
        |    v: int
        |
        |trait Reader[I]
        |    type Token: Eq
        |    head(i: I) -> Self::Token
        |
        |impl[T] Reader[Box[T]]
        |    type Token = T
        |    head(b: Box[T]) -> T = b.v
        |
        |main() -> int
        |    val b = Box(NotEq(1))
        |    Reader.head(b)
        |    0
        |""".stripMargin)
  }

  // ===== Generic impl with multi-bound on assoc type =====

  "generic impl assoc-bound with multiple bounds, all satisfied" in {
    eval(
      """trait Eq[T]
        |    eq(a: T, b: T) -> bool
        |
        |trait Ord[T]
        |    cmp(a: T, b: T) -> int
        |
        |struct Coin
        |    cents: int
        |
        |impl Eq[Coin]
        |    eq(a: Coin, b: Coin) -> bool = a.cents == b.cents
        |
        |impl Ord[Coin]
        |    cmp(a: Coin, b: Coin) -> int = a.cents - b.cents
        |
        |struct Box[T]
        |    v: T
        |
        |trait Reader[I]
        |    type Token: Eq + Ord
        |    head(i: I) -> Self::Token
        |
        |impl[T] Reader[Box[T]]
        |    type Token = T
        |    head(b: Box[T]) -> T = b.v
        |
        |main() -> int
        |    val r = Reader.head(Box(Coin(7)))
        |    r.cents
        |""".stripMargin) shouldBe 7
  }

  "generic impl assoc-bound with multiple bounds, one unsatisfied" in {
    // Coin has Eq but not Ord; Reader.Token: Eq + Ord — should fail.
    an[Exception] should be thrownBy eval(
      eqTrait + eqImplCoin +
      """trait Ord[T]
        |    cmp(a: T, b: T) -> int
        |
        |struct Box[T]
        |    v: T
        |
        |trait Reader[I]
        |    type Token: Eq + Ord
        |    head(i: I) -> Self::Token
        |
        |impl[T] Reader[Box[T]]
        |    type Token = T
        |    head(b: Box[T]) -> T = b.v
        |
        |main() -> int
        |    Reader.head(Box(Coin(1)))
        |    0
        |""".stripMargin)
  }

  // ===== Concrete-binding-target inside generic impl still works =====

  "generic impl, assoc binding to a non-tvar concrete type" in {
    // Token bound to int regardless of T. int has Eq via the user impl.
    eval(
      """trait Eq[T]
        |    eq(a: T, b: T) -> bool
        |
        |impl Eq[int]
        |    eq(a: int, b: int) -> bool = a == b
        |
        |struct Box[T]
        |    v: T
        |
        |trait Reader[I]
        |    type Token: Eq
        |    head(i: I) -> Self::Token
        |
        |impl[T] Reader[Box[T]]
        |    type Token = int
        |    head(b: Box[T]) -> int = 42
        |
        |main() -> int = Reader.head(Box(Box(true)))
        |""".stripMargin) shouldBe 42
  }
}
