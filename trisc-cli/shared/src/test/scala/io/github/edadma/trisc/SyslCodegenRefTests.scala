package io.github.edadma.trisc

class SyslCodegenRefTests extends SyslCodegenHelpers {

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

  private def refSources(mainSource: String, heapSize: Int = 16384): Map[String, String] =
    Map(
      "posix/unistd/sbrk" -> sbrkModule(heapSize),
      "posix/string/string" -> stringSource,
      "posix/stdlib/alloc" -> allocSource,
      "main" -> mainSource,
    )

  // ===== Basic new =====

  "new creates and reads struct fields" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    val p = new Point(10, 20)
        |    p.x + p.y
        |""".stripMargin)) shouldBe 30
  }

  "new field assignment" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |struct Box
        |    value: int
        |
        |main() -> int
        |    val b = new Box(0)
        |    b.value = 42
        |    b.value
        |""".stripMargin)) shouldBe 42
  }

  "multiple new allocations" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |struct Box
        |    value: int
        |
        |main() -> int
        |    val a = new Box(10)
        |    val b = new Box(20)
        |    val c = new Box(30)
        |    a.value + b.value + c.value
        |""".stripMargin)) shouldBe 60
  }

  "new with many fields" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |struct Vec4
        |    x: int
        |    y: int
        |    z: int
        |    w: int
        |
        |main() -> int
        |    val v = new Vec4(1, 2, 3, 4)
        |    v.x + v.y + v.z + v.w
        |""".stripMargin)) shouldBe 10
  }

  "new in loop" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |struct Box
        |    value: int
        |
        |main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 5
        |        val b = new Box(i * 10)
        |        sum += b.value
        |        i++
        |    sum
        |""".stripMargin)) shouldBe 100
  }

  "new passed to function" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |struct Box
        |    value: int
        |
        |get(b: &Box) -> int = b.value
        |
        |main() -> int
        |    val b = new Box(77)
        |    get(b)
        |""".stripMargin)) shouldBe 77
  }

  "new modified through function" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |struct Box
        |    value: int
        |
        |set(b: &Box, v: int)
        |    b.value = v
        |
        |main() -> int
        |    val b = new Box(0)
        |    set(b, 55)
        |    b.value
        |""".stripMargin)) shouldBe 55
  }

  "new returned from function" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |struct Box
        |    value: int
        |
        |make(v: int) -> &Box = new Box(v)
        |
        |main() -> int
        |    val b = make(42)
        |    b.value
        |""".stripMargin)) shouldBe 42
  }

  "shared ref sees mutation" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |struct Counter
        |    n: int
        |
        |main() -> int
        |    val a = new Counter(0)
        |    val b = a
        |    a.n = 42
        |    b.n
        |""".stripMargin)) shouldBe 42
  }
}
