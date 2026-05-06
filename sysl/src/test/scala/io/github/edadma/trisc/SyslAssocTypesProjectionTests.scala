package io.github.edadma.trisc

/** Phase A2 — `Self::Item` / `T::Item` projection in type position resolves to
 *  the impl's binding when monomorphizing a trait method into an impl. Phase A1
 *  (assoc-type registration) is the prerequisite; A3 will generalize this to
 *  use sites where `T: SomeTrait` is a generic-fn bound.
 */
class SyslAssocTypesProjectionTests extends SyslTestHelpers {

  // ===== Self::Item in trait method signature =====

  "Self::Item in return type resolves to impl's binding (concrete impl)" in {
    eval(
      """trait It[I]
        |    type Item
        |    head(i: I) -> Self::Item
        |
        |impl It[int]
        |    type Item = i64
        |    head(i: int) -> i64 = i64(i) * 2i64
        |
        |main() -> int = i32(It.head(21))
        |""".stripMargin) shouldBe 42
  }

  "trait-param qualifier (I::Item) resolves the same as Self::Item" in {
    // Inside `trait It[I]`, writing `I::Item` is the explicit form; the
    // qualifier substitutes to the trait's first target, but the assoc lookup
    // is by member name, so resolution is identical to `Self::Item`.
    eval(
      """trait It[I]
        |    type Item
        |    head(i: I) -> I::Item
        |
        |impl It[int]
        |    type Item = i64
        |    head(i: int) -> i64 = i64(i) + 1i64
        |
        |main() -> int = i32(It.head(41))
        |""".stripMargin) shouldBe 42
  }

  "Self::Item in parameter position" in {
    eval(
      """trait Reader[R]
        |    type Token
        |    accept(r: R, t: Self::Token) -> int
        |
        |impl Reader[int]
        |    type Token = i64
        |    accept(r: int, t: i64) -> int = r + i32(t)
        |
        |main() -> int = Reader.accept(10, 32i64)
        |""".stripMargin) shouldBe 42
  }

  // ===== Two impls with different bindings =====

  "two impls bind Self::Item differently and dispatch chooses correctly" in {
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
        |main() -> int
        |    a = i32(It.twice(5))     // i64(10) → i32 = 10
        |    b = It.twice(7i64)        // i32 = 21
        |    a + b                      // 31
        |""".stripMargin) shouldBe 31
  }

  // ===== Generic impl with Self::Item =====

  "generic impl: Self::Item picks up the impl tvar substitution" in {
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
        |main() -> int
        |    var b = Box[int](42)
        |    It.unwrap(b)
        |""".stripMargin) shouldBe 42
  }

  // ===== Negative: projection where binding is missing =====

  "projection with unknown member errors clearly" in {
    val ex = intercept[Exception](eval(
      """trait It[I]
        |    type Item
        |    head(i: I) -> Self::Bogus
        |
        |impl It[int]
        |    type Item = i64
        |    head(i: int) -> i64 = i64(i)
        |
        |main() -> int = 0
        |""".stripMargin))
    // Phase A1 already rejects `type Bogus = ...` in the impl as undeclared,
    // but here the trait *signature* references an undeclared assoc — the
    // resolver fires and reports "not bound in current impl".
    ex.getMessage should (include("not bound") or include("Bogus"))
  }

  // ===== Cross-module SMETA round-trip with projection =====

  "trait+impl with Self::Item round-trip across modules" in {
    val libs = Map(
      "projlib/p" ->
        """module projlib
          |
          |struct Stream
          |    pos: int
          |
          |trait Reader[R]
          |    type Token
          |    head(r: R) -> Self::Token
          |
          |impl Reader[Stream]
          |    type Token = i64
          |    head(r: Stream) -> i64 = i64(r.pos) + 1i64
          |""".stripMargin,
    )
    evalWithLibs(libs,
      """import projlib.*
        |
        |main() -> int = i32(Reader.head(Stream(41)))
        |""".stripMargin) shouldBe 42
  }
}
