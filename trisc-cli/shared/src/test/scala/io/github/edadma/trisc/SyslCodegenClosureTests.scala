package io.github.edadma.trisc

class SyslCodegenClosureTests extends SyslCodegenHelpers {

  // Allocator sources needed for tests with captures (malloc)
  private val allocSource = scala.io.Source.fromFile("posix/stdlib/alloc.sysl").mkString
  private val stringSource = scala.io.Source.fromFile("posix/string/string.sysl").mkString
  private val ctypeSource = scala.io.Source.fromFile("posix/ctype/ctype.sysl").mkString

  private def sbrkModule(heapSize: Int = 16384): String =
    s"""module posix.unistd
       |
       |var _heap: [$heapSize]i8
       |var _brk: *i8 = *i8(0)
       |var _brk_initialized = false
       |
       |sbrk(increment: int) -> *i8
       |    if !_brk_initialized
       |        _brk = &_heap
       |        _brk_initialized = true
       |
       |    if increment == 0 then return _brk
       |
       |    val old_brk = _brk
       |    val new_brk = old_brk + increment
       |
       |    if i64(new_brk) > i64(&_heap + $heapSize) then return *i8(-1)
       |
       |    _brk = new_brk
       |    old_brk
       |""".stripMargin

  private def allocSources(mainSource: String): Map[String, String] =
    Map(
      "posix/unistd/sbrk" -> sbrkModule(),
      "posix/string/string" -> stringSource,
      "posix/ctype/ctype" -> ctypeSource,
      "posix/stdlib/alloc" -> allocSource,
      "main" -> mainSource,
    )

  // ===== Basic closures (no captures) =====

  "zero-capture closure" in {
    compileAndRun(
      """apply(f: func(int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(x -> x + 1, 41)
        |""".stripMargin) shouldBe 42
  }

  "multi-param closure" in {
    compileAndRun(
      """apply2(f: func(int, int) -> int, a: int, b: int) -> int = f(a, b)
        |
        |main() -> int = apply2((x, y) -> x + y, 20, 22)
        |""".stripMargin) shouldBe 42
  }

  "zero-param closure" in {
    compileAndRun(
      """call(f: func() -> int) -> int = f()
        |
        |main() -> int = call(() -> 42)
        |""".stripMargin) shouldBe 42
  }

  "closure assigned to variable" in {
    compileAndRun(
      """main() -> int
        |    val f: func(int) -> int = x -> x * 2
        |    f(21)
        |""".stripMargin) shouldBe 42
  }

  // ===== Capture by value =====

  "capture local variable" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |apply(f: func(int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    val a = 10
        |    apply(x -> x + a, 32)
        |""".stripMargin)) shouldBe 42
  }

  "capture is frozen (by value)" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |apply(f: func(int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    var a = 10
        |    val f: func(int) -> int = x -> x + a
        |    a = 100
        |    f(32)
        |""".stripMargin)) shouldBe 42
  }

  "capture multiple variables" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |apply(f: func(int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    val a = 10
        |    val b = 20
        |    apply(x -> x + a + b, 12)
        |""".stripMargin)) shouldBe 42
  }

  // ===== Higher-order functions =====

  "closure passed to higher-order function" in {
    compileAndRun(
      """apply(f: func(int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(x -> x * 2, 21)
        |""".stripMargin) shouldBe 42
  }

  "closure as return value" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |make_adder(n: int) -> func(int) -> int
        |    val captured = n
        |    x -> x + captured
        |
        |main() -> int
        |    val add10 = make_adder(10)
        |    add10(32)
        |""".stripMargin)) shouldBe 42
  }

  // ===== Type-annotated parameters =====

  "closure with typed parameters" in {
    compileAndRun(
      """apply(f: func(int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply((x: int) -> x + 1, 41)
        |""".stripMargin) shouldBe 42
  }

  // ===== Expressions =====

  "closure in arithmetic expression" in {
    compileAndRun(
      """apply(f: func(int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(x -> x + 1, 20) + apply(x -> x + 1, 20)
        |""".stripMargin) shouldBe 42
  }
}
