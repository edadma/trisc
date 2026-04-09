package io.github.edadma.trisc

class SyslLLVMAdvancedTests extends SyslLLVMTestHelpers {

  // ===== Unsigned ops =====

  "unsigned division" in {
    llvmExit(
      """main() -> int
        |    x: u32 = 200
        |    y: u32 = 10
        |    x / y
        |""".stripMargin) shouldBe 20
  }

  // ===== Match on integers =====

  "match value pattern" in {
    llvmExit(
      """main() -> int
        |    x = 2
        |    x match
        |        1 -> 10
        |        2 -> 42
        |        3 -> 30
        |        else -> 0
        |""".stripMargin) shouldBe 42
  }

  "match with default" in {
    llvmExit(
      """main() -> int
        |    x = 99
        |    x match
        |        1 -> 10
        |        2 -> 20
        |        else -> 42
        |""".stripMargin) shouldBe 42
  }

  // ===== Defer =====

  "defer runs before return" in {
    llvmOutput(
      """main() -> int
        |    puts("start")
        |    defer puts("deferred")
        |    puts("end")
        |    0
        |""".stripMargin) shouldBe "start\nend\ndeferred"
  }

  "multiple defers run LIFO" in {
    llvmOutput(
      """main() -> int
        |    defer puts("first")
        |    defer puts("second")
        |    defer puts("third")
        |    0
        |""".stripMargin) shouldBe "third\nsecond\nfirst"
  }

  // ===== Tagged unions (data enums) =====

  "enum construction and match" in {
    llvmExit(
      """enum Shape
        |    Circle(radius: int)
        |    Rect(w: int, h: int)
        |
        |main() -> int
        |    s = Circle(5)
        |    s match
        |        Circle(r) -> r
        |        Rect(w, h) -> w * h
        |""".stripMargin) shouldBe 5
  }

  "enum match second variant" in {
    llvmExit(
      """enum Shape
        |    Circle(radius: int)
        |    Rect(w: int, h: int)
        |
        |main() -> int
        |    s = Rect(6, 7)
        |    s match
        |        Circle(r) -> r
        |        Rect(w, h) -> w * h
        |""".stripMargin) shouldBe 42
  }
}
