package io.github.edadma.trisc

class SyslExtensionTests extends SyslTestHelpers {

  "extension dispatch" - {

    "extension on user struct: receiver in scope, method callable via dot" in {
      eval(
        """struct Box
          |    v: int
          |
          |extension (b: Box)
          |    def doubled(extra: int) -> int = b.v * 2 + extra
          |
          |main() -> int
          |    bx = Box(5)
          |    bx.doubled(3)
          |""".stripMargin) shouldBe 13
      // 5 * 2 + 3 = 13
    }

    "extension on i32 (non-struct receiver, the headline ergonomics win)" in {
      eval(
        """extension (x: i32)
          |    def plus_one_then_double -> i32 = (x + 1) * 2
          |
          |main() -> int
          |    n = 7
          |    n.plus_one_then_double
          |""".stripMargin) shouldBe 16
      // (7 + 1) * 2 = 16
    }

    "extension method missing — error names receiver and method (no silent dispatch)" in {
      val ex = intercept[Exception] {
        eval(
          """main() -> int
            |    n = 7
            |    n.no_such_method
            |""".stripMargin)
      }
      val msg = ex.getMessage.toLowerCase
      assert(msg.contains("no_such_method") || msg.contains("cannot call method"),
        s"expected dispatch error, got: ${ex.getMessage}")
    }

    "two extensions of the same method on different types coexist" in {
      eval(
        """extension (x: i32)
          |    def kind -> i32 = 1
          |
          |extension (s: string)
          |    def kind -> i32 = 2
          |
          |main() -> int
          |    a = (5).kind
          |    b = "hi".kind
          |    a * 10 + b
          |""".stripMargin) shouldBe 12
    }

    "real struct method takes precedence over extension with same name" in {
      eval(
        """struct Box
          |    v: int
          |
          |Box_get(self: *Box) -> int = self.v
          |
          |extension (b: Box)
          |    def get -> int = 999
          |
          |main() -> int
          |    bx = Box(7)
          |    bx.get
          |""".stripMargin) shouldBe 7
      // Real method `Box_get` wins; extension never fires.
    }
  }

  "cross-module extension visibility (Phase 2b)" - {

    "wildcard import brings extension into scope" in {
      val libs = Map(
        "mylib/strings" ->
          """module mylib
            |
            |extension (x: i32)
            |    def doubled -> i32 = x * 2
            |""".stripMargin)
      evalWithLibs(libs,
        """import mylib.*
          |
          |main() -> int
          |    n = 21
          |    n.doubled
          |""".stripMargin) shouldBe 42
    }

    "named import also brings extensions (Scala-3 style)" in {
      val libs = Map(
        "mylib/x" ->
          """module mylib
            |
            |add_one(n: i32) -> i32 = n + 1
            |
            |extension (x: i32)
            |    def tripled -> i32 = x * 3
            |""".stripMargin)
      evalWithLibs(libs,
        """import mylib.{add_one}
          |
          |main() -> int
          |    n = 5
          |    n.tripled + add_one(0)
          |""".stripMargin) shouldBe 16
      // 5 * 3 + 1 = 16
    }

    "extension on string receiver dispatches across modules" in {
      // Predef trick (visibility-without-import) is gated by
      // `primitiveDefiningModule` in the analyzer but additionally requires the
      // driver to auto-register Predef modules (deferred — see handoff memo).
      // For now an explicit `import` exercises the cross-module dispatch path.
      val libs = Map(
        "std/string/string" ->
          """module std.string
            |
            |extension (s: string)
            |    def double_it -> i32 = 4
            |""".stripMargin)
      evalWithLibs(libs,
        """import std.string.*
          |
          |main() -> int
          |    "hi".double_it
          |""".stripMargin) shouldBe 4
    }

    "extension defined in unimported module is not dispatched" in {
      val libs = Map(
        "otherlib/x" ->
          """module otherlib
            |
            |extension (x: i32)
            |    def hidden -> i32 = 999
            |""".stripMargin)
      val ex = intercept[Exception] {
        evalWithLibs(libs,
          """main() -> int
            |    n = 7
            |    n.hidden
            |""".stripMargin)
      }
      val msg = ex.getMessage.toLowerCase
      assert(msg.contains("hidden") || msg.contains("cannot call method"),
        s"expected dispatch error, got: ${ex.getMessage}")
    }
  }
}
