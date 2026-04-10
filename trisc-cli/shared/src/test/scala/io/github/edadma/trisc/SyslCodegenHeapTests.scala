package io.github.edadma.trisc

class SyslCodegenHeapTests extends SyslCodegenHelpers {

  private val allocSource = scala.io.Source.fromFile("posix/stdlib/alloc.sysl").mkString
  private val stringSource = scala.io.Source.fromFile("posix/string/string.sysl").mkString
  private val ctypeSource = scala.io.Source.fromFile("posix/ctype/ctype.sysl").mkString

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val heapSysl: String = readLsysl("std/heap/heap.lsysl")

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

  private def heapSources(mainSource: String): Map[String, String] =
    Map(
      "posix/unistd/sbrk" -> sbrkModule(),
      "posix/string/string" -> stringSource,
      "posix/ctype/ctype" -> ctypeSource,
      "posix/stdlib/alloc" -> allocSource,
      "std/heap/heap" -> heapSysl,
      "main" -> mainSource,
    )

  "push and pop single element" in {
    compileMultiAndRun(heapSources(
      """import std.heap.*
        |
        |int_less(a: int, b: int) -> bool = a < b
        |
        |main() -> int
        |    var h = new_min_heap(int_less)
        |    h.push(42)
        |    h.pop()
        |""".stripMargin)) shouldBe 42
  }

  "pop returns minimum" in {
    compileMultiAndRun(heapSources(
      """import std.heap.*
        |
        |int_less(a: int, b: int) -> bool = a < b
        |
        |main() -> int
        |    var h = new_min_heap(int_less)
        |    h.push(5)
        |    h.push(3)
        |    h.push(7)
        |    h.push(1)
        |    h.push(4)
        |    h.pop()
        |""".stripMargin)) shouldBe 1
  }

  "sorted extraction" in {
    compileMultiAndRun(heapSources(
      """import std.heap.*
        |
        |int_less(a: int, b: int) -> bool = a < b
        |
        |main() -> int
        |    var h = new_min_heap(int_less)
        |    h.push(30)
        |    h.push(10)
        |    h.push(50)
        |    h.push(20)
        |    h.push(40)
        |    val a = h.pop()
        |    val b = h.pop()
        |    val c = h.pop()
        |    val d = h.pop()
        |    val e = h.pop()
        |    // Encode correctness: a=10, b=20, c=30, d=40, e=50
        |    // Return 1 if all correct, 0 otherwise
        |    if a == 10 && b == 20 && c == 30 && d == 40 && e == 50 then 1 else 0
        |""".stripMargin)) shouldBe 1
  }

  "peek does not remove" in {
    compileMultiAndRun(heapSources(
      """import std.heap.*
        |
        |int_less(a: int, b: int) -> bool = a < b
        |
        |main() -> int
        |    var h = new_min_heap(int_less)
        |    h.push(5)
        |    h.push(3)
        |    val p = h.peek()
        |    // p should be 3, and len should still be 2
        |    if p == 3 && h.len() == 2 then 1 else 0
        |""".stripMargin)) shouldBe 1
  }

  "interleaved push and pop" in {
    compileMultiAndRun(heapSources(
      """import std.heap.*
        |
        |int_less(a: int, b: int) -> bool = a < b
        |
        |main() -> int
        |    var h = new_min_heap(int_less)
        |    h.push(5)
        |    h.push(3)
        |    val a = h.pop()  // 3
        |    h.push(1)
        |    h.push(4)
        |    val b = h.pop()  // 1
        |    val c = h.pop()  // 4
        |    val d = h.pop()  // 5
        |    if a == 3 && b == 1 && c == 4 && d == 5 then 1 else 0
        |""".stripMargin)) shouldBe 1
  }

  "duplicates handled correctly" in {
    compileMultiAndRun(heapSources(
      """import std.heap.*
        |
        |int_less(a: int, b: int) -> bool = a < b
        |
        |main() -> int
        |    var h = new_min_heap(int_less)
        |    h.push(3)
        |    h.push(1)
        |    h.push(3)
        |    h.push(1)
        |    val a = h.pop()
        |    val b = h.pop()
        |    val c = h.pop()
        |    val d = h.pop()
        |    if a == 1 && b == 1 && c == 3 && d == 3 then 1 else 0
        |""".stripMargin)) shouldBe 1
  }

  "remove_at root" in {
    compileMultiAndRun(heapSources(
      """import std.heap.*
        |
        |int_less(a: int, b: int) -> bool = a < b
        |
        |main() -> int
        |    var h = new_min_heap(int_less)
        |    h.push(1)
        |    h.push(3)
        |    h.push(2)
        |    h.remove_at(0)
        |    val a = h.pop()
        |    val b = h.pop()
        |    if a == 2 && b == 3 && h.empty() then 1 else 0
        |""".stripMargin)) shouldBe 1
  }

  "20 elements sort correctly" in {
    compileMultiAndRun(heapSources(
      """import std.heap.*
        |
        |int_less(a: int, b: int) -> bool = a < b
        |
        |main() -> int
        |    var h = new_min_heap(int_less)
        |    h.push(15)
        |    h.push(3)
        |    h.push(18)
        |    h.push(7)
        |    h.push(1)
        |    h.push(12)
        |    h.push(20)
        |    h.push(9)
        |    h.push(5)
        |    h.push(14)
        |    h.push(2)
        |    h.push(11)
        |    h.push(19)
        |    h.push(6)
        |    h.push(4)
        |    h.push(17)
        |    h.push(8)
        |    h.push(13)
        |    h.push(16)
        |    h.push(10)
        |    var prev = h.pop()
        |    var ok = 1
        |    for var i = 1; i < 20; i++
        |        val cur = h.pop()
        |        if cur < prev then ok = 0
        |        prev = cur
        |    ok
        |""".stripMargin)) shouldBe 1
  }

  "empty after all pops" in {
    compileMultiAndRun(heapSources(
      """import std.heap.*
        |
        |int_less(a: int, b: int) -> bool = a < b
        |
        |main() -> int
        |    var h = new_min_heap(int_less)
        |    h.push(1)
        |    h.push(2)
        |    h.push(3)
        |    val _ = h.pop()
        |    val _ = h.pop()
        |    val _ = h.pop()
        |    if h.empty() then 1 else 0
        |""".stripMargin)) shouldBe 1
  }
}
