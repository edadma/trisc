package io.github.edadma.trisc

class SyslVariableTests extends SyslTestHelpers {

  // ===== Variables =====

  "local variable with type annotation" in {
    eval(
      """main() -> int
        |    x: int = 42
        |    return x
        |""".stripMargin) shouldBe 42
  }

  "local variable inferred" in {
    eval(
      """main() -> int
        |    x = 42
        |    return x
        |""".stripMargin) shouldBe 42
  }

  "variable assignment" in {
    eval(
      """main() -> int
        |    x = 1
        |    x = 42
        |    return x
        |""".stripMargin) shouldBe 42
  }

  "multiple variables" in {
    eval(
      """main() -> int
        |    a = 10
        |    b = 20
        |    return a + b
        |""".stripMargin) shouldBe 30
  }

  // ===== Global variables =====

  "global variable" in {
    eval(
      """counter = 0
        |
        |increment()
        |    counter = counter + 1
        |
        |main() -> int
        |    increment()
        |    increment()
        |    increment()
        |    return counter
        |""".stripMargin) shouldBe 3
  }

  "global variable with expression initializer" in {
    eval(
      """x = 2 + 3
        |
        |main() -> int = x
        |""".stripMargin) shouldBe 5
  }

  // ===== Syntactic edge cases: variables =====

  "typed variable declaration" in {
    eval(
      """main() -> int
        |    x: int = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "untyped variable then reassign" in {
    eval(
      """main() -> int
        |    x = 1
        |    x = 2
        |    x = 3
        |    x
        |""".stripMargin) shouldBe 3
  }

  "variable used in its own initialization expression" in {
    eval(
      """main() -> int
        |    x = 10
        |    x = x + 5
        |    x
        |""".stripMargin) shouldBe 15
  }

  // ===== Uninitialized typed declarations =====

  "uninit scalar keyword type" in {
    eval(
      """main() -> int
        |    x: int
        |    x
        |""".stripMargin) shouldBe 0
  }

  "uninit pointer type" in {
    eval(
      """main() -> int
        |    p: *i8
        |    i64(p)
        |""".stripMargin) shouldBe 0
  }

  "uninit double pointer type" in {
    eval(
      """main() -> int
        |    p: **int
        |    i64(p)
        |""".stripMargin) shouldBe 0
  }

  "uninit i8 type" in {
    eval(
      """main() -> int
        |    b: i8
        |    int(b)
        |""".stripMargin) shouldBe 0
  }

  "uninit u32 type" in {
    eval(
      """main() -> int
        |    c: u32
        |    int(c)
        |""".stripMargin) shouldBe 0
  }

  "uninit bool type" in {
    eval(
      """main() -> int
        |    b: bool
        |    if b then return 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "uninit scalar then assign" in {
    eval(
      """main() -> int
        |    x: int
        |    x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "uninit pointer then assign" in {
    eval(
      """main() -> int
        |    var buf: [3]int
        |    buf[0] = 99
        |    p: *int
        |    p = &buf[0]
        |    *p
        |""".stripMargin) shouldBe 99
  }

  "uninit struct type" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p: Point
        |    p.x = 10
        |    p.y = 20
        |    p.x + p.y
        |""".stripMargin) shouldBe 30
  }

  "uninit array type" in {
    eval(
      """main() -> int
        |    var a: [3]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 6
  }

  "uninit global scalar" in {
    eval(
      """x: int
        |
        |main() -> int
        |    x = 7
        |    x
        |""".stripMargin) shouldBe 7
  }

  "uninit global pointer" in {
    eval(
      """p: *int
        |
        |main() -> int
        |    var v: int = 42
        |    p = &v
        |    *p
        |""".stripMargin) shouldBe 42
  }

  // ===== Error cases =====

  "undefined variable error" in {
    val Right(ast) = (new SyslParser).parseProgram("main() -> int = x\n"): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }
}
