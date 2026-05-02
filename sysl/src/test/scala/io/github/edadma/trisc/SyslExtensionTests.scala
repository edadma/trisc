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
}
