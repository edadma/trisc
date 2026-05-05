package io.github.edadma.trisc

/** Phase A3 — `T::Item` at *use sites*. When a generic function has a bound
 *  `T: SomeTrait`, references to `T::Assoc` inside its signature/body resolve
 *  by looking up `SomeTrait`'s impl for the substituted `T` and consulting
 *  that impl's `type Assoc = ...` binding. Phases A1/A2 covered the same
 *  projection inside an impl method's body during monomorphization; A3
 *  generalizes it to free generic functions.
 */
class SyslAssocTypesUseSiteTests extends SyslTestHelpers {

  // ===== Happy path: single-bound generic fn projects the bound's assoc =====

  "generic fn with bound projects assoc on its type-param" in {
    eval(
      """trait It[I]
        |    type Item
        |    head(i: I) -> Self::Item
        |
        |impl It[int]
        |    type Item = i64
        |    head(i: int) -> i64 = i64(i) * 2i64
        |
        |first[I: It](x: I) -> I::Item = It.head(x)
        |
        |main() -> int = i32(first(21))
        |""".stripMargin) shouldBe 42
  }

  "projection in parameter position of a generic fn" in {
    eval(
      """trait Reader[R]
        |    type Token
        |    accept(r: R, t: Self::Token) -> int
        |
        |impl Reader[int]
        |    type Token = i64
        |    accept(r: int, t: i64) -> int = r + i32(t)
        |
        |use_acc[R: Reader](r: R, t: R::Token) -> int = Reader.accept(r, t)
        |
        |main() -> int = use_acc(10, 32i64)
        |""".stripMargin) shouldBe 42
  }

  // ===== Two impls dispatch by the use-site's bound type =====

  "two impls bind assoc differently; generic fn dispatches via inference" in {
    eval(
      """trait It[I]
        |    type Item
        |    twice(i: I) -> Self::Item
        |
        |impl It[int]
        |    type Item = i64
        |    twice(i: int) -> i64 = i64(i) * 2i64
        |
        |impl It[i64]
        |    type Item = i32
        |    twice(i: i64) -> i32 = i32(i) * 3
        |
        |go[I: It](x: I) -> I::Item = It.twice(x)
        |
        |main() -> int
        |    a = i32(go(5))      // Item=i64, 10
        |    b = go(7i64)         // Item=i32, 21
        |    a + b                 // 31
        |""".stripMargin) shouldBe 31
  }

  // ===== Generic impl: assoc resolves under impl's tvar substitution =====

  "generic impl: T::Item picks up the impl tvar substitution at use site" in {
    eval(
      """struct Box[T]
        |    v: T
        |
        |trait It[I]
        |    type Item
        |    unwrap(b: I) -> Self::Item
        |
        |impl[T] It[Box[T]]
        |    type Item = T
        |    unwrap(b: Box[T]) -> T = b.v
        |
        |open[I: It](b: I) -> I::Item = It.unwrap(b)
        |
        |main() -> int
        |    var b = Box[int](42)
        |    open(b)
        |""".stripMargin) shouldBe 42
  }

  // ===== Negative: type-param without a relevant bound =====

  "projection on a type-param without a relevant bound errors clearly" in {
    val ex = intercept[Exception](eval(
      """trait It[I]
        |    type Item
        |    head(i: I) -> Self::Item
        |
        |impl It[int]
        |    type Item = i64
        |    head(i: int) -> i64 = i64(i)
        |
        |bad[T](x: T) -> T::Item = x   // T has no bound declaring `Item`
        |
        |main() -> int = i32(bad(7))   // force instantiation
        |""".stripMargin))
    // Either "no impl context" (assocBindingsEnv empty for unbounded T)
    // or the missing-member message, or the qualifier-not-known message.
    ex.getMessage should (include("Item") or include("not bound") or include("no impl context") or include("not a known type parameter"))
  }

  // ===== Cross-module: import a generic fn that projects, instantiate locally =====

  "generic fn with projection imported across modules" in {
    val libs = Map(
      "uselib/u" ->
        """module uselib
          |
          |trait It[I]
          |    type Item
          |    head(i: I) -> Self::Item
          |
          |first[I: It](x: I) -> I::Item = It.head(x)
          |""".stripMargin,
    )
    evalWithLibs(libs,
      """import uselib.*
        |
        |impl It[int]
        |    type Item = i64
        |    head(i: int) -> i64 = i64(i) + 1i64
        |
        |main() -> int = i32(first(41))
        |""".stripMargin) shouldBe 42
  }
}
