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
        |    var h = new_min_heap[int](int_less)
        |    heap_push[int](&h, 42)
        |    heap_pop(&h)
        |""".stripMargin)) shouldBe 42
  }

  "pop returns minimum" in {
    compileMultiAndRun(heapSources(
      """import std.heap.*
        |
        |int_less(a: int, b: int) -> bool = a < b
        |
        |main() -> int
        |    var h = new_min_heap[int](int_less)
        |    heap_push[int](&h, 5)
        |    heap_push[int](&h, 3)
        |    heap_push[int](&h, 7)
        |    heap_push[int](&h, 1)
        |    heap_push[int](&h, 4)
        |    heap_pop(&h)
        |""".stripMargin)) shouldBe 1
  }

  "sorted extraction" in {
    compileMultiAndRun(heapSources(
      """import std.heap.*
        |
        |int_less(a: int, b: int) -> bool = a < b
        |
        |main() -> int
        |    var h = new_min_heap[int](int_less)
        |    heap_push[int](&h, 30)
        |    heap_push[int](&h, 10)
        |    heap_push[int](&h, 50)
        |    heap_push[int](&h, 20)
        |    heap_push[int](&h, 40)
        |    val a = heap_pop(&h)
        |    val b = heap_pop(&h)
        |    val c = heap_pop(&h)
        |    val d = heap_pop(&h)
        |    val e = heap_pop(&h)
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
        |    var h = new_min_heap[int](int_less)
        |    heap_push[int](&h, 5)
        |    heap_push[int](&h, 3)
        |    val p = heap_peek(&h)
        |    // p should be 3, and len should still be 2
        |    if p == 3 && heap_len(&h) == 2 then 1 else 0
        |""".stripMargin)) shouldBe 1
  }

  "interleaved push and pop" in {
    compileMultiAndRun(heapSources(
      """import std.heap.*
        |
        |int_less(a: int, b: int) -> bool = a < b
        |
        |main() -> int
        |    var h = new_min_heap[int](int_less)
        |    heap_push[int](&h, 5)
        |    heap_push[int](&h, 3)
        |    val a = heap_pop(&h)  // 3
        |    heap_push[int](&h, 1)
        |    heap_push[int](&h, 4)
        |    val b = heap_pop(&h)  // 1
        |    val c = heap_pop(&h)  // 4
        |    val d = heap_pop(&h)  // 5
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
        |    var h = new_min_heap[int](int_less)
        |    heap_push[int](&h, 3)
        |    heap_push[int](&h, 1)
        |    heap_push[int](&h, 3)
        |    heap_push[int](&h, 1)
        |    val a = heap_pop(&h)
        |    val b = heap_pop(&h)
        |    val c = heap_pop(&h)
        |    val d = heap_pop(&h)
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
        |    var h = new_min_heap[int](int_less)
        |    heap_push[int](&h, 1)
        |    heap_push[int](&h, 3)
        |    heap_push[int](&h, 2)
        |    heap_remove_at(&h, 0)
        |    val a = heap_pop(&h)
        |    val b = heap_pop(&h)
        |    if a == 2 && b == 3 && heap_empty(&h) then 1 else 0
        |""".stripMargin)) shouldBe 1
  }

  "20 elements sort correctly" in {
    compileMultiAndRun(heapSources(
      """import std.heap.*
        |
        |int_less(a: int, b: int) -> bool = a < b
        |
        |main() -> int
        |    var h = new_min_heap[int](int_less)
        |    heap_push[int](&h, 15)
        |    heap_push[int](&h, 3)
        |    heap_push[int](&h, 18)
        |    heap_push[int](&h, 7)
        |    heap_push[int](&h, 1)
        |    heap_push[int](&h, 12)
        |    heap_push[int](&h, 20)
        |    heap_push[int](&h, 9)
        |    heap_push[int](&h, 5)
        |    heap_push[int](&h, 14)
        |    heap_push[int](&h, 2)
        |    heap_push[int](&h, 11)
        |    heap_push[int](&h, 19)
        |    heap_push[int](&h, 6)
        |    heap_push[int](&h, 4)
        |    heap_push[int](&h, 17)
        |    heap_push[int](&h, 8)
        |    heap_push[int](&h, 13)
        |    heap_push[int](&h, 16)
        |    heap_push[int](&h, 10)
        |    var prev = heap_pop(&h)
        |    var ok = 1
        |    for var i = 1; i < 20; i++
        |        val cur = heap_pop(&h)
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
        |    var h = new_min_heap[int](int_less)
        |    heap_push[int](&h, 1)
        |    heap_push[int](&h, 2)
        |    heap_push[int](&h, 3)
        |    val _ = heap_pop(&h)
        |    val _ = heap_pop(&h)
        |    val _ = heap_pop(&h)
        |    if heap_empty(&h) then 1 else 0
        |""".stripMargin)) shouldBe 1
  }
}
