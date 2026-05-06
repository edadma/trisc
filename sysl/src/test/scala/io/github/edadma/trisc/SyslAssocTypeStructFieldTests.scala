package io.github.edadma.trisc

/** Generic struct fields whose type is an associated-type projection.
 *
 *  `struct Foo[T: Reader] { x: T::Token }` — when Foo is instantiated for
 *  concrete T, the field type must resolve via T's matching impl of Reader.
 *  This needs `assocBindingsEnv` plumbed through generic-struct instantiation,
 *  same way Phase A3 plumbed it through generic-fn instantiation.
 */
class SyslAssocTypeStructFieldTests extends SyslTestHelpers {

  "generic struct field of type T::Item resolves via the bound's impl" in {
    eval(
      """trait It[I]
        |    type Item
        |    head(i: I) -> Self::Item
        |
        |impl It[int]
        |    type Item = i64
        |    head(i: int) -> i64 = i64(i) * 2i64
        |
        |struct Wrap[T: It]
        |    raw: T
        |    pulled: T::Item
        |
        |main() -> int
        |    var w = Wrap[int](7, It.head(7))
        |    i32(w.pulled)        // 14
        |""".stripMargin) shouldBe 14
  }
}
