package io.github.edadma.trisc

class SyslCodegenDynArrayTests extends SyslCodegenHelpers {

  private val allocSource = scala.io.Source.fromFile("posix/stdlib/alloc.sysl").mkString
  private val stringSource = scala.io.Source.fromFile("posix/string/string.sysl").mkString

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

  private def arraySources(mainSource: String): Map[String, String] =
    Map(
      "posix/unistd/sbrk" -> sbrkModule(),
      "posix/string/string" -> stringSource,
      "posix/stdlib/alloc" -> allocSource,
      "main" -> mainSource,
    )

  "new array write and read" in {
    compileMultiAndRun(arraySources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    val a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    a[0] + a[1] + a[2]
        |""".stripMargin)) shouldBe 60
  }

  "new array with runtime size" in {
    compileMultiAndRun(arraySources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    val n = 4
        |    val a = new [n]int
        |    var i = 0
        |    while i < n
        |        a[i] = i * 10
        |        i++
        |    a[0] + a[1] + a[2] + a[3]
        |""".stripMargin)) shouldBe 60
  }

  "new array len" in {
    compileMultiAndRun(arraySources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    val a = new [7]int
        |    len(a)
        |""".stripMargin)) shouldBe 7
  }

  "new array shared ref" in {
    compileMultiAndRun(arraySources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    val a = new [3]int
        |    val b = a
        |    a[1] = 42
        |    b[1]
        |""".stripMargin)) shouldBe 42
  }

  "new array passed to function" in {
    compileMultiAndRun(arraySources(
      """import posix.stdlib.{malloc, free}
        |
        |sum(a: &[]int, n: int) -> int
        |    var total = 0
        |    var i = 0
        |    while i < n
        |        total += a[i]
        |        i++
        |    total
        |
        |main() -> int
        |    val a = new [4]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    a[3] = 4
        |    sum(a, 4)
        |""".stripMargin)) shouldBe 10
  }

  "new array returned from function" in {
    compileMultiAndRun(arraySources(
      """import posix.stdlib.{malloc, free}
        |
        |make_range(n: int) -> &[]int
        |    val a = new [n]int
        |    var i = 0
        |    while i < n
        |        a[i] = i
        |        i++
        |    a
        |
        |main() -> int
        |    val a = make_range(5)
        |    a[0] + a[1] + a[2] + a[3] + a[4]
        |""".stripMargin)) shouldBe 10
  }

  "new array using len in function" in {
    compileMultiAndRun(arraySources(
      """import posix.stdlib.{malloc, free}
        |
        |sum_all(a: &[]int) -> int
        |    var total = 0
        |    var i = 0
        |    while i < len(a)
        |        total += a[i]
        |        i++
        |    total
        |
        |main() -> int
        |    val a = new [3]int
        |    a[0] = 100
        |    a[1] = 200
        |    a[2] = 300
        |    sum_all(a)
        |""".stripMargin)) shouldBe 600
  }

  "multiple independent arrays" in {
    compileMultiAndRun(arraySources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    val a = new [2]int
        |    val b = new [2]int
        |    a[0] = 10
        |    b[0] = 20
        |    a[0] + b[0]
        |""".stripMargin)) shouldBe 30
  }

  // ===== Refcount reaches zero — memory reuse =====

  "tight loop array allocation with small heap" in {
    compileMultiAndRun(arraySources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 50
        |        val a = new [4]int
        |        a[0] = i
        |        sum += a[0]
        |        i++
        |    sum
        |""".stripMargin)) shouldBe 1225
  }

  "reassign array in loop — old freed" in {
    compileMultiAndRun(arraySources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    var a = new [2]int
        |    var i = 0
        |    while i < 50
        |        a = new [2]int
        |        a[0] = i
        |        i++
        |    a[0]
        |""".stripMargin)) shouldBe 49
  }

  "array in loop" in {
    compileMultiAndRun(arraySources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 5
        |        val a = new [1]int
        |        a[0] = i * 10
        |        sum += a[0]
        |        i++
        |    sum
        |""".stripMargin)) shouldBe 100
  }
}
