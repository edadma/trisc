package io.github.edadma.trisc

/** Multi-bound disambiguation for associated types.
 *
 *  When a type parameter carries `T: A + B` and both traits declare an
 *  associated type with the same name (e.g. both have `type Item`), a bare
 *  `T::Item` projection is ambiguous: the impl of A may bind it to one type
 *  and the impl of B to another.
 *
 *  Disambiguating fully would require a richer projection AST (`T::A.Item`)
 *  and resolver work. For now we only need to reject ambiguous cases — silently
 *  picking one and dropping the other (the prior last-write-wins behavior) was
 *  unsound. The simple-case (only one bound declares the assoc) still works.
 */
class SyslAssocTypeMultiBoundTests extends SyslTestHelpers {

  // ===== Happy path: only one of two bounds declares the assoc =====

  "two-bound generic fn: only one bound declares the assoc — projection is unambiguous" in {
    eval(
      """trait Eq2[T]
        |    eq2(a: T, b: T) -> bool
        |
        |impl Eq2[int]
        |    eq2(a: int, b: int) -> bool = a == b
        |
        |trait It[I]
        |    type Item
        |    head(i: I) -> Self::Item
        |
        |impl It[int]
        |    type Item = i64
        |    head(i: int) -> i64 = i64(i) * 2i64
        |
        |first[I: It + Eq2](x: I) -> I::Item = It.head(x)
        |
        |main() -> int = i32(first(21))
        |""".stripMargin) shouldBe 42
  }

  // ===== Ambiguous case: both bounds declare `type Item` differently =====

  "two-bound generic fn: both bounds declare `Item` differently — projection is rejected" in {
    val ex = intercept[Exception](eval(
      """trait A1[T]
        |    type Item
        |    a1head(i: T) -> Self::Item
        |
        |trait B1[T]
        |    type Item
        |    b1head(i: T) -> Self::Item
        |
        |impl A1[int]
        |    type Item = i64
        |    a1head(i: int) -> i64 = i64(i)
        |
        |impl B1[int]
        |    type Item = i32
        |    b1head(i: int) -> i32 = i
        |
        |both[T: A1 + B1](x: T) -> T::Item = A1.a1head(x)
        |
        |main() -> int = i32(both(7))
        |""".stripMargin))
    val msg = ex.getMessage
    msg should (include("Item") or include("ambiguous") or include("multiple"))
  }
}
