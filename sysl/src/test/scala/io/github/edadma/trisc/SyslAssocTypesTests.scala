package io.github.edadma.trisc

/** Phase A1 (associated-types scaffolding) — language-design tests for the
 *  `type Item [: Bound + Bound]` / `type Item = ConcreteType` syntax inside
 *  traits and impls.
 *
 *  These tests exercise the parser/AST/analyzer registration only: projection
 *  resolution (`I::Item` at use sites) lands in Phase A3. Until then a trait
 *  may declare assoc types and impls must bind them, but no body-level
 *  reference to the assoc type exists yet.
 */
class SyslAssocTypesTests extends SyslTestHelpers {

  // ===== Happy path: registration only (no projection yet) =====

  "trait with one associated type and an impl that binds it parses + analyzes" in {
    eval(
      """trait Reader[I]
        |    type Token
        |    at_end(inp: I) -> bool
        |
        |impl Reader[int]
        |    type Token = i64
        |    at_end(inp: int) -> bool = inp == 0
        |
        |main() -> int = if Reader.at_end(0) then 1 else 2
        |""".stripMargin) shouldBe 1
  }

  "trait with multiple associated types, all bound" in {
    eval(
      """trait Stream[S]
        |    type Token
        |    type Cursor
        |    next(s: S) -> int
        |
        |impl Stream[int]
        |    type Token = i64
        |    type Cursor = i32
        |    next(s: int) -> int = s + 1
        |
        |main() -> int = Stream.next(41)
        |""".stripMargin) shouldBe 42
  }

  "trait with associated type carrying bounds parses (bounds enforced as of Phase C)" in {
    // Phase A1 accepts the bound syntax; Phase C now enforces it. Bounds Eq + Ord
    // are not declared in this snippet, so the impl trips the unknown-trait check.
    // SyslAssocTypeBoundsTests covers the satisfied-bound positive paths.
    val ex = intercept[Exception](eval(
      """trait It[I]
        |    type Item: Eq + Ord
        |    head(i: I) -> int
        |
        |impl It[int]
        |    type Item = i64
        |    head(i: int) -> int = i * 2
        |
        |main() -> int = It.head(21)
        |""".stripMargin))
    ex.getMessage should (include("Eq") or include("Ord") or include("unknown trait"))
  }

  "associated type alongside no-arg trait method" in {
    eval(
      """trait Empty[E]
        |    type Item
        |    zero(e: E) -> int
        |
        |impl Empty[int]
        |    type Item = i32
        |    zero(e: int) -> int = 0
        |
        |main() -> int = Empty.zero(7)
        |""".stripMargin) shouldBe 0
  }

  // ===== Negative cases: registration validation =====

  "impl missing associated-type binding is error" in {
    val ex = intercept[Exception](eval(
      """trait It[I]
        |    type Item
        |    head(i: I) -> int
        |
        |impl It[int]
        |    head(i: int) -> int = i
        |
        |main() -> int = It.head(0)
        |""".stripMargin))
    ex.getMessage should include("Item")
  }

  "impl binds undeclared associated type is error" in {
    val ex = intercept[Exception](eval(
      """trait It[I]
        |    type Item
        |    head(i: I) -> int
        |
        |impl It[int]
        |    type Item = i64
        |    type Bogus = i32
        |    head(i: int) -> int = i
        |
        |main() -> int = It.head(0)
        |""".stripMargin))
    ex.getMessage should include("Bogus")
  }

  "duplicate associated-type declaration in trait is error" in {
    an[Exception] should be thrownBy eval(
      """trait It[I]
        |    type Item
        |    type Item
        |    head(i: I) -> int
        |
        |impl It[int]
        |    type Item = i64
        |    head(i: int) -> int = i
        |
        |main() -> int = 0
        |""".stripMargin)
  }

  "duplicate associated-type binding in impl is error" in {
    an[Exception] should be thrownBy eval(
      """trait It[I]
        |    type Item
        |    head(i: I) -> int
        |
        |impl It[int]
        |    type Item = i64
        |    type Item = i32
        |    head(i: int) -> int = i
        |
        |main() -> int = 0
        |""".stripMargin)
  }

  "associated type using a reserved T::Attr name is error" in {
    val ex = intercept[Exception](eval(
      """trait It[I]
        |    type First
        |    head(i: I) -> int
        |
        |impl It[int]
        |    type First = i64
        |    head(i: int) -> int = i
        |
        |main() -> int = 0
        |""".stripMargin))
    ex.getMessage should include("reserved")
  }

  "associated type shadowing the trait's own type parameter is error" in {
    val ex = intercept[Exception](eval(
      """trait It[I]
        |    type I
        |    head(i: I) -> int
        |
        |impl It[int]
        |    type I = i64
        |    head(i: int) -> int = i
        |
        |main() -> int = 0
        |""".stripMargin))
    ex.getMessage.toLowerCase should include("shadow")
  }

  "associated type sharing a method name is error" in {
    val ex = intercept[Exception](eval(
      """trait It[I]
        |    head(i: I) -> int
        |    type head
        |
        |impl It[int]
        |    type head = i64
        |    head(i: int) -> int = i
        |
        |main() -> int = 0
        |""".stripMargin))
    ex.getMessage.toLowerCase should include("shadow")
  }

  // ===== Two impls with different bindings =====

  "two impls of the same trait carry different associated-type bindings" in {
    eval(
      """trait It[I]
        |    type Item
        |    head(i: I) -> int
        |
        |impl It[int]
        |    type Item = i64
        |    head(i: int) -> int = i
        |
        |impl It[i64]
        |    type Item = i32
        |    head(i: i64) -> int = i32(i) * 2
        |
        |main() -> int
        |    a = It.head(5)
        |    b = It.head(7i64)
        |    a + b
        |""".stripMargin) shouldBe 19
  }

  // ===== Generic impl with associated-type binding =====

  "generic impl with associated-type binding parses and dispatches" in {
    eval(
      """struct Box[T]
        |    v: T
        |
        |trait It[I]
        |    type Item
        |    unwrap(b: I) -> int
        |
        |impl[T] It[Box[T]]
        |    type Item = T
        |    unwrap(b: Box[T]) -> int = 42
        |
        |main() -> int
        |    var b = Box[int](7)
        |    It.unwrap(b)
        |""".stripMargin) shouldBe 42
  }

  // ===== Cross-file SMETA round-trip (v15) =====

  "trait with assoc types and concrete impl in a separate module imported by main" in {
    // Trait + impl + target struct all in `demolib`. main imports and dispatches.
    // Exercises the SMETA v15 TEMPLATES round-trip for assoc-binding impls.
    val libs = Map(
      "demolib/demo" ->
        """module demolib
          |
          |struct Tag
          |    n: int
          |
          |trait It[I]
          |    type Item
          |    head(i: I) -> int
          |
          |impl It[Tag]
          |    type Item = i64
          |    head(i: Tag) -> int = i.n + 1
          |""".stripMargin,
    )
    evalWithLibs(libs,
      """import demolib.*
        |
        |main() -> int = It.head(Tag(41))
        |""".stripMargin) shouldBe 42
  }

  "generic impl with assoc binding declared in one file, used in main (cross-module)" in {
    val libs = Map(
      "boxlib/box" ->
        """module boxlib
          |
          |struct Box[T]
          |    v: T
          |
          |trait It[I]
          |    type Item
          |    unwrap(b: I) -> int
          |
          |impl[T] It[Box[T]]
          |    type Item = T
          |    unwrap(b: Box[T]) -> int = 100
          |""".stripMargin,
    )
    evalWithLibs(libs,
      """import boxlib.*
        |
        |main() -> int
        |    var b = Box[int](9)
        |    It.unwrap(b)
        |""".stripMargin) shouldBe 100
  }

  "concrete impl with assoc binding round-trips across module imports" in {
    // Trait + concrete impl + impl target struct all live in `rdrlib`; main
    // imports them and dispatches. Exercises the v15 SMETA TEMPLATES path for
    // an assoc-binding impl.
    val libs = Map(
      "rdrlib/rdr" ->
        """module rdrlib
          |
          |struct Stream
          |    pos: int
          |
          |trait Reader[R]
          |    type Token
          |    at_end(r: R) -> bool
          |
          |impl Reader[Stream]
          |    type Token = i64
          |    at_end(r: Stream) -> bool = r.pos == 0
          |""".stripMargin,
    )
    evalWithLibs(libs,
      """import rdrlib.*
        |
        |main() -> int = if Reader.at_end(Stream(0)) then 7 else 8
        |""".stripMargin) shouldBe 7
  }
}
