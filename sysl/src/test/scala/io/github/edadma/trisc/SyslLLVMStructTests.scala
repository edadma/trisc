package io.github.edadma.trisc

class SyslLLVMStructTests extends SyslLLVMTestHelpers {

  // ===== Basic struct construction and field access =====

  "struct construction and field read" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p = Point(3, 4)
        |    p.x + p.y
        |""".stripMargin) shouldBe 7
  }

  "struct field assignment" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    var p = Point(0, 0)
        |    p.x = 10
        |    p.y = 32
        |    p.x + p.y
        |""".stripMargin) shouldBe 42
  }

  "struct zero-initialized" in {
    llvmExit(
      """struct Pair
        |    a: int
        |    b: int
        |
        |main() -> int
        |    p = Pair(0, 0)
        |    p.a + p.b
        |""".stripMargin) shouldBe 0
  }

  "struct with mixed types" in {
    llvmOutput(
      """struct Record
        |    name: string
        |    age: int
        |
        |main() -> int
        |    r = Record("Alice", 30)
        |    puts(r.name)
        |    println(r.age)
        |    0
        |""".stripMargin) shouldBe "Alice\n30"
  }

  "struct passed to function" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |sum(p: Point) -> int = p.x + p.y
        |
        |main() -> int
        |    p = Point(20, 22)
        |    sum(p)
        |""".stripMargin) shouldBe 42
  }

  "struct with interpolation" in {
    llvmOutput(
      """struct Vec2
        |    x: int
        |    y: int
        |
        |main() -> int
        |    v = Vec2(3, 4)
        |    puts(s"${v.x}, ${v.y}")
        |    0
        |""".stripMargin) shouldBe "3, 4"
  }

  "nested struct field read" in {
    llvmExit(
      """struct Inner
        |    v: int
        |
        |struct Outer
        |    a: Inner
        |    b: int
        |
        |main() -> int
        |    o = Outer(Inner(42), 0)
        |    o.a.v
        |""".stripMargin) shouldBe 42
  }
}
