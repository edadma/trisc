package io.github.edadma.trisc

/** Phase C follow-up (continuation): trait bounds on trait, impl, and extension
 *  type-parameters. Companion to SyslEnumAliasBoundsTests, which closed the
 *  enum + alias gaps. Together with the previously-shipped struct + fn forms,
 *  this completes bound support across every type-param-bearing decl form.
 *
 *  - Trait bounds (`trait Container[T: Ord]`): enforced at impl registration —
 *    each impl target must satisfy the trait's tparam bounds.
 *  - Impl bounds  (`impl[T: Ord] Get[Box[T]]`): enforced at `instantiateImpl`
 *    when the impl is selected for a concrete type.
 *  - Extension bounds (`extension[T: Ord] (b: Box[T])`): rides through the
 *    extension→impl lowering, enforced via the impl path. */
class SyslTraitImplExtensionBoundsTests extends SyslTestHelpers {

  private val ordTrait =
    """trait Ord[T]
      |    cmp(a: T, b: T) -> int
      |""".stripMargin

  private val ordImplCoin =
    """struct Coin
      |    cents: int
      |
      |impl Ord[Coin]
      |    cmp(a: Coin, b: Coin) -> int = a.cents - b.cents
      |""".stripMargin

  // ===== Trait bounds — enforced at impl registration =====

  "trait with bound on type-param parses" in {
    eval(
      ordTrait +
      """trait Container[T: Ord]
        |    head(c: T) -> T
        |
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  "trait bound, satisfied at impl site" in {
    eval(
      ordTrait + ordImplCoin +
      """trait Container[T: Ord]
        |    head(c: T) -> T
        |
        |impl Container[Coin]
        |    head(c: Coin) -> Coin = c
        |
        |main() -> int
        |    val c = Container.head(Coin(42))
        |    c.cents
        |""".stripMargin) shouldBe 42
  }

  "trait bound, unsatisfied impl rejected" in {
    an[Exception] should be thrownBy eval(
      ordTrait +
      """struct NotOrd
        |    v: int
        |
        |trait Container[T: Ord]
        |    head(c: T) -> T
        |
        |impl Container[NotOrd]
        |    head(c: NotOrd) -> NotOrd = c
        |
        |main() -> int = 0
        |""".stripMargin)
  }

  "trait with multiple type params, mixed bounds" in {
    eval(
      ordTrait + ordImplCoin +
      """trait TwoArg[A: Ord, B]
        |    pick(a: A, b: B) -> A
        |
        |impl TwoArg[Coin, int]
        |    pick(a: Coin, b: int) -> Coin = a
        |
        |main() -> int
        |    val r = TwoArg.pick(Coin(7), 0)
        |    r.cents
        |""".stripMargin) shouldBe 7
  }

  // ===== Impl bounds — enforced at instantiateImpl =====

  "impl with bound on type-param parses" in {
    eval(
      ordTrait +
      """struct Box[T]
        |    v: T
        |
        |trait Get[B]
        |    getit(b: B) -> int
        |
        |impl[T: Ord] Get[Box[T]]
        |    getit(b: Box[T]) -> int = 0
        |
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  "impl bound, satisfied at dispatch" in {
    eval(
      ordTrait + ordImplCoin +
      """struct Box[T]
        |    v: T
        |
        |trait Get[B]
        |    getit(b: B) -> int
        |
        |impl[T: Ord] Get[Box[T]]
        |    getit(b: Box[T]) -> int = b.v.cents
        |
        |main() -> int = Get.getit(Box(Coin(99)))
        |""".stripMargin) shouldBe 99
  }

  "impl bound, unsatisfied substitution rejected at dispatch" in {
    an[Exception] should be thrownBy eval(
      ordTrait +
      """struct Box[T]
        |    v: T
        |
        |struct NotOrd
        |    v: int
        |
        |trait Get[B]
        |    getit(b: B) -> int
        |
        |impl[T: Ord] Get[Box[T]]
        |    getit(b: Box[T]) -> int = 0
        |
        |main() -> int = Get.getit(Box(NotOrd(1)))
        |""".stripMargin)
  }

  // ===== Extension bounds — enforced via the lowering =====

  "extension with bound on type-param parses" in {
    eval(
      ordTrait +
      """struct Box[T]
        |    v: T
        |
        |extension[T: Ord] (b: Box[T])
        |    def head() -> int = 0
        |
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  "extension bound, satisfied at call site" in {
    eval(
      ordTrait + ordImplCoin +
      """struct Box[T]
        |    v: T
        |
        |extension[T: Ord] (b: Box[T])
        |    def cents() -> int = 0
        |
        |main() -> int
        |    val b = Box(Coin(123))
        |    b.cents()
        |""".stripMargin) shouldBe 0
  }

  "extension bound, unsatisfied call rejected" in {
    an[Exception] should be thrownBy eval(
      ordTrait +
      """struct Box[T]
        |    v: T
        |
        |struct NotOrd
        |    v: int
        |
        |extension[T: Ord] (b: Box[T])
        |    def cents() -> int = 0
        |
        |main() -> int
        |    val b = Box(NotOrd(1))
        |    b.cents()
        |""".stripMargin)
  }

  // ===== Cross-module — bounds survive sibling-meta path =====

  "trait bound imported across modules" in {
    val libs = Map(
      "boundlib/b" ->
        """module boundlib
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
          |trait Container[T: Ord]
          |    head(c: T) -> T
          |
          |impl Container[Coin]
          |    head(c: Coin) -> Coin = c
          |""".stripMargin,
    )
    evalWithLibs(libs,
      """import boundlib.*
        |
        |main() -> int
        |    val c = Container.head(Coin(55))
        |    c.cents
        |""".stripMargin) shouldBe 55
  }
}
