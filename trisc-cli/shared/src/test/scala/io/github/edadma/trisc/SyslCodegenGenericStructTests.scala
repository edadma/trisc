package io.github.edadma.trisc

class SyslCodegenGenericStructTests extends SyslCodegenHelpers {

  "generic struct basic" in {
    compileAndRun(
      """struct Box[T]
        |    value: T
        |
        |main() -> int
        |    b = Box(42)
        |    b.value
        |""".stripMargin) shouldBe 42
  }

  "generic Pair with int" in {
    compileAndRun(
      """struct Pair[T]
        |    a: T
        |    b: T
        |
        |main() -> int
        |    p = Pair(10, 20)
        |    p.a * 100 + p.b
        |""".stripMargin) shouldBe 1020
  }

  "generic function swapping generic struct fields" in {
    compileAndRun(
      """struct Pair[T]
        |    a: T
        |    b: T
        |
        |swapPair[T](p: *Pair[T])
        |    var tmp: T = p.a
        |    p.a = p.b
        |    p.b = tmp
        |
        |main() -> int
        |    var p = Pair(1, 99)
        |    swapPair(&p)
        |    p.a * 100 + p.b
        |""".stripMargin) shouldBe 9901
  }

  "different instantiations" in {
    compileAndRun(
      """struct Box[T]
        |    v: T
        |
        |main() -> int
        |    b1 = Box(5)
        |    b2 = Box(7)
        |    b1.v * b2.v
        |""".stripMargin) shouldBe 35
  }
}
