package io.github.edadma.trisc

/** Phase C — bound enforcement on associated types.
 *
 *  When a trait declares `type Token: Bound1 + Bound2`, every impl of that
 *  trait must bind `Token` to a type that satisfies all the declared bounds
 *  (i.e. there must be an impl of each bound trait whose first target unifies
 *  with the bound type).
 *
 *  Phase A1 already parses + carries the bounds through the AST. Phase C is the
 *  validation step at impl-registration time.
 */
class SyslAssocTypeBoundsTests extends SyslTestHelpers {

  // ===== Happy path: bound is satisfied =====

  "impl whose assoc-type binding satisfies a single bound is accepted" in {
    eval(
      """trait Eq2[T]
        |    eq2(a: T, b: T) -> bool
        |
        |impl Eq2[int]
        |    eq2(a: int, b: int) -> bool = a == b
        |
        |trait Reader[I]
        |    type Token: Eq2
        |    head(i: I) -> Self::Token
        |
        |impl Reader[int]
        |    type Token = int
        |    head(i: int) -> int = i
        |
        |main() -> int = Reader.head(42)
        |""".stripMargin) shouldBe 42
  }

  "impl satisfying multiple bounds is accepted" in {
    eval(
      """trait Eq2[T]
        |    eq2(a: T, b: T) -> bool
        |trait Ord2[T]
        |    cmp2(a: T, b: T) -> int
        |
        |impl Eq2[int]
        |    eq2(a: int, b: int) -> bool = a == b
        |impl Ord2[int]
        |    cmp2(a: int, b: int) -> int = a - b
        |
        |trait Reader[I]
        |    type Token: Eq2 + Ord2
        |    head(i: I) -> Self::Token
        |
        |impl Reader[int]
        |    type Token = int
        |    head(i: int) -> int = i
        |
        |main() -> int = Reader.head(42)
        |""".stripMargin) shouldBe 42
  }

  // ===== Negative: bound is unsatisfied =====

  "impl whose assoc-type binding fails the bound is rejected" in {
    val ex = intercept[Exception](eval(
      """trait Eq2[T]
        |    eq2(a: T, b: T) -> bool
        |
        |impl Eq2[int]
        |    eq2(a: int, b: int) -> bool = a == b
        |
        |trait Reader[I]
        |    type Token: Eq2
        |    head(i: I) -> Self::Token
        |
        |impl Reader[bool]
        |    type Token = bool      // bool does NOT implement Eq2
        |    head(i: bool) -> bool = i
        |
        |main() -> int = 0
        |""".stripMargin))
    val msg = ex.getMessage
    msg should (include("Eq2") or include("bound") or include("Token") or include("does not satisfy"))
  }

  "impl rejected when only one of two bounds is satisfied" in {
    val ex = intercept[Exception](eval(
      """trait Eq2[T]
        |    eq2(a: T, b: T) -> bool
        |trait Ord2[T]
        |    cmp2(a: T, b: T) -> int
        |
        |impl Eq2[int]
        |    eq2(a: int, b: int) -> bool = a == b
        |// no Ord2 impl for int — should still trip the bound
        |
        |trait Reader[I]
        |    type Token: Eq2 + Ord2
        |    head(i: I) -> Self::Token
        |
        |impl Reader[int]
        |    type Token = int
        |    head(i: int) -> int = i
        |
        |main() -> int = 0
        |""".stripMargin))
    val msg = ex.getMessage
    msg should (include("Ord2") or include("bound") or include("does not satisfy"))
  }

  // ===== Unknown-trait bound =====

  "trait declaring an unknown bound on its assoc type is rejected" in {
    val ex = intercept[Exception](eval(
      """trait Reader[I]
        |    type Token: NotARealTrait
        |    head(i: I) -> Self::Token
        |
        |impl Reader[int]
        |    type Token = int
        |    head(i: int) -> int = i
        |
        |main() -> int = 0
        |""".stripMargin))
    val msg = ex.getMessage
    msg should (include("NotARealTrait") or include("unknown trait") or include("not a known trait"))
  }
}
