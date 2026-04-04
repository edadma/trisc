package io.github.edadma.trisc

class SyslCodegenTupleTests extends SyslCodegenHelpers {

  "basic tuple literal and access" in {
    compileAndRun(
      """main() -> int
        |    val t = (10, 20)
        |    t.0 + t.1
        |""".stripMargin) shouldBe 30
  }

  "three-element tuple" in {
    compileAndRun(
      """main() -> int
        |    val t = (1, 2, 3)
        |    t.0 + t.1 + t.2
        |""".stripMargin) shouldBe 6
  }

  "tuple destructuring" in {
    compileAndRun(
      """main() -> int
        |    val (a, b) = (10, 32)
        |    a + b
        |""".stripMargin) shouldBe 42
  }

  "destructuring three elements" in {
    compileAndRun(
      """main() -> int
        |    val (x, y, z) = (10, 20, 12)
        |    x + y + z
        |""".stripMargin) shouldBe 42
  }

  "tuple with expressions" in {
    compileAndRun(
      """main() -> int
        |    val a = 5
        |    val t = (a * 2, a + 3)
        |    t.0 + t.1
        |""".stripMargin) shouldBe 18
  }

  "tuple with function call in element" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |
        |main() -> int
        |    val t = (dbl(5), dbl(7))
        |    t.0 + t.1
        |""".stripMargin) shouldBe 24
  }

  "destructure from function returning tuple" in {
    compileAndRun(
      """make_pair(a: int, b: int) -> int
        |    val t = (a, b)
        |    t.0 * 10 + t.1
        |
        |main() -> int
        |    make_pair(4, 2)
        |""".stripMargin) shouldBe 42
  }

  // ===== Go-style paren-free destructuring =====

  "Go-style destructure: a, b = f()" in {
    compileAndRun(
      """divmod(a: int, b: int) -> (int, int)
        |    (a / b, a % b)
        |
        |main() -> int
        |    q, r = divmod(17, 5)
        |    q * 10 + r
        |""".stripMargin) shouldBe 32
  }

  "Go-style destructure with val" in {
    compileAndRun(
      """swap(a: int, b: int) -> (int, int) = (b, a)
        |
        |main() -> int
        |    val x, y = swap(10, 20)
        |    x * 100 + y
        |""".stripMargin) shouldBe 2010
  }

  "Go-style destructure with var" in {
    compileAndRun(
      """pair() -> (int, int) = (1, 2)
        |
        |main() -> int
        |    var a, b = pair()
        |    a += 10
        |    b += 20
        |    a * 100 + b
        |""".stripMargin) shouldBe 1122
  }
}
