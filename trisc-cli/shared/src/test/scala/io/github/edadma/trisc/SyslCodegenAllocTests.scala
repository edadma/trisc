package io.github.edadma.trisc

class SyslCodegenAllocTests extends SyslCodegenHelpers {

  private val allocSource = scala.io.Source.fromFile("posix/stdlib/alloc.sysl").mkString
  private val stringSource = scala.io.Source.fromFile("posix/string/string.sysl").mkString
  private val ctypeSource = scala.io.Source.fromFile("posix/ctype/ctype.sysl").mkString

  // Bare-metal sbrk backed by a static array
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

  private def allocSources(mainSource: String, heapSize: Int = 16384): Map[String, String] =
    Map(
      "posix/unistd/sbrk" -> sbrkModule(heapSize),
      "posix/string/string" -> stringSource,
      "posix/ctype/ctype" -> ctypeSource,
      "posix/stdlib/alloc" -> allocSource,
      "main" -> mainSource,
    )

  "alloc and write/read single i64" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val ptr = malloc(8)
        |    val p: *i64 = *i64(i64(ptr))
        |    *p = 42
        |    int(*p)
        |""".stripMargin)) shouldBe 42
  }

  "alloc two separate blocks" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val a = malloc(8)
        |    val b = malloc(8)
        |    val pa: *i64 = *i64(i64(a))
        |    val pb: *i64 = *i64(i64(b))
        |    *pa = 10
        |    *pb = 32
        |    int(*pa + *pb)
        |""".stripMargin)) shouldBe 42
  }

  "alloc and free then reuse" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val a = malloc(8)
        |    val pa: *i64 = *i64(i64(a))
        |    *pa = 99
        |    free(a)
        |    val b = malloc(8)
        |    val pb: *i64 = *i64(i64(b))
        |    *pb = 42
        |    int(*pb)
        |""".stripMargin)) shouldBe 42
  }

  "alloc from exhausted heap returns null" in {
    val failSbrk =
      """module posix.unistd
        |
        |sbrk(increment: int) -> *i8
        |    *i8(-1)
        |""".stripMargin
    compileMultiAndRun(Map(
      "posix/unistd/sbrk" -> failSbrk,
      "posix/string/string" -> stringSource,
      "posix/ctype/ctype" -> ctypeSource,
      "posix/stdlib/alloc" -> allocSource,
      "main" ->
        """import posix.stdlib.*
          |
          |main() -> int
          |    val p = malloc(8)
          |    if i64(p) == 0 then 1 else 0
          |""".stripMargin,
    )) shouldBe 1
  }

  "multiple alloc/free cycles" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    var sum = 0
        |    var i = 0
        |
        |    while i < 10
        |        val p = malloc(8)
        |        val pi: *i64 = *i64(i64(p))
        |        *pi = i64(i)
        |        sum += int(*pi)
        |        free(p)
        |        i += 1
        |
        |    sum
        |""".stripMargin)) shouldBe 45
  }

  "coalescing: free adjacent blocks then alloc large" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val a = malloc(8)
        |    val b = malloc(8)
        |    val c = malloc(8)
        |    free(a)
        |    free(b)
        |    free(c)
        |    val big = malloc(48)
        |    if i64(big) != 0 then 1 else 0
        |""".stripMargin)) shouldBe 1
  }

  "realloc grows allocation" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    var p = realloc(*i8(0), 8)
        |    val pi: *i64 = *i64(i64(p))
        |    *pi = 42
        |    p = realloc(p, 64)
        |    val pi2: *i64 = *i64(i64(p))
        |    int(*pi2)
        |""".stripMargin)) shouldBe 42
  }

  "realloc with null is alloc" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val p = realloc(*i8(0), 8)
        |    if i64(p) != 0 then 1 else 0
        |""".stripMargin)) shouldBe 1
  }

  "many small allocations" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    var count = 0
        |    var i = 0
        |
        |    while i < 100
        |        val p = malloc(8)
        |
        |        if i64(p) != 0
        |            count += 1
        |
        |        i += 1
        |
        |    count
        |""".stripMargin)) shouldBe 100
  }

  "size classes: small and large allocations coexist" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val small1 = malloc(8)
        |    val big1 = malloc(4000)
        |    val small2 = malloc(8)
        |    val ps1: *i64 = *i64(i64(small1))
        |    val ps2: *i64 = *i64(i64(small2))
        |    *ps1 = 10
        |    *ps2 = 32
        |    int(*ps1 + *ps2)
        |""".stripMargin)) shouldBe 42
  }

  "calloc zeroes memory" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val p = calloc(4, 8)
        |    val pi: *i64 = *i64(i64(p))
        |    int(*pi)
        |""".stripMargin)) shouldBe 0
  }
}
