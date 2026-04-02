package io.github.edadma.trisc

class SyslCodegenAllocTests extends SyslCodegenHelpers {

  private val allocSource = scala.io.Source.fromFile("sysl/lib/alloc.sysl").mkString

  private def withAlloc(mainSource: String): String =
    allocSource + "\n" + mainSource

  "alloc and write/read single i64" in {
    compileAndRun(withAlloc(
      """var heap: [1024]i8
        |
        |main() -> int
        |    _sysl_alloc_init(&heap, &heap + 1024)
        |    val ptr = _sysl_alloc(8)
        |    val p: *i64 = *i64(i64(ptr))
        |    *p = 42
        |    int(*p)
        |""".stripMargin)) shouldBe 42
  }

  "alloc two separate blocks" in {
    compileAndRun(withAlloc(
      """var heap: [4096]i8
        |
        |main() -> int
        |    _sysl_alloc_init(&heap, &heap + 4096)
        |    val a = _sysl_alloc(8)
        |    val b = _sysl_alloc(8)
        |    val pa: *i64 = *i64(i64(a))
        |    val pb: *i64 = *i64(i64(b))
        |    *pa = 10
        |    *pb = 32
        |    int(*pa + *pb)
        |""".stripMargin)) shouldBe 42
  }

  "alloc and free then reuse" in {
    compileAndRun(withAlloc(
      """var heap: [4096]i8
        |
        |main() -> int
        |    _sysl_alloc_init(&heap, &heap + 4096)
        |    val a = _sysl_alloc(8)
        |    val pa: *i64 = *i64(i64(a))
        |    *pa = 99
        |    _sysl_free(a)
        |    // Should reuse the freed block
        |    val b = _sysl_alloc(8)
        |    val pb: *i64 = *i64(i64(b))
        |    *pb = 42
        |    int(*pb)
        |""".stripMargin)) shouldBe 42
  }

  "alloc uses expected space" in {
    compileAndRun(withAlloc(
      """var heap: [48]i8
        |
        |main() -> int
        |    _sysl_alloc_init(&heap, &heap + 48)
        |    val a = _sysl_alloc(8)
        |    val b = _sysl_alloc(8)
        |    int(i64(heap_top) - i64(heap_base))
        |""".stripMargin)) shouldBe 48
  }

  "alloc from zero-size heap returns null" in {
    compileAndRun(withAlloc(
      """var heap: [8]i8
        |
        |main() -> int
        |    _sysl_alloc_init(&heap, &heap)
        |    val p = _sysl_alloc(8)
        |    if i64(p) == 0 then 1 else 0
        |""".stripMargin)) shouldBe 1
  }

  "multiple alloc/free cycles" in {
    compileAndRun(withAlloc(
      """var heap: [4096]i8
        |
        |main() -> int
        |    _sysl_alloc_init(&heap, &heap + 4096)
        |    var sum = 0
        |    var i = 0
        |    while i < 10
        |        val p = _sysl_alloc(8)
        |        val pi: *i64 = *i64(i64(p))
        |        *pi = i64(i)
        |        sum += int(*pi)
        |        _sysl_free(p)
        |        i += 1
        |    sum
        |""".stripMargin)) shouldBe 45  // 0+1+2+...+9
  }

  "coalescing: free adjacent blocks then alloc large" in {
    compileAndRun(withAlloc(
      """var heap: [4096]i8
        |
        |main() -> int
        |    _sysl_alloc_init(&heap, &heap + 4096)
        |    val a = _sysl_alloc(8)
        |    val b = _sysl_alloc(8)
        |    val c = _sysl_alloc(8)
        |    _sysl_free(a)
        |    _sysl_free(b)
        |    _sysl_free(c)
        |    // All three freed and coalesced — should be able to alloc a large block
        |    val big = _sysl_alloc(48)
        |    if i64(big) != 0 then 1 else 0
        |""".stripMargin)) shouldBe 1
  }

  "realloc grows allocation" in {
    compileAndRun(withAlloc(
      """var heap: [4096]i8
        |
        |main() -> int
        |    _sysl_alloc_init(&heap, &heap + 4096)
        |    var p = _sysl_realloc(*i8(0), 8)
        |    val pi: *i64 = *i64(i64(p))
        |    *pi = 42
        |    p = _sysl_realloc(p, 64)
        |    val pi2: *i64 = *i64(i64(p))
        |    int(*pi2)
        |""".stripMargin)) shouldBe 42  // data preserved after realloc
  }

  "realloc with null is alloc" in {
    compileAndRun(withAlloc(
      """var heap: [4096]i8
        |
        |main() -> int
        |    _sysl_alloc_init(&heap, &heap + 4096)
        |    val p = _sysl_realloc(*i8(0), 8)
        |    if i64(p) != 0 then 1 else 0
        |""".stripMargin)) shouldBe 1
  }

  "many small allocations" in {
    compileAndRun(withAlloc(
      """var heap: [8192]i8
        |
        |main() -> int
        |    _sysl_alloc_init(&heap, &heap + 8192)
        |    var count = 0
        |    var i = 0
        |    while i < 100
        |        val p = _sysl_alloc(8)
        |        if i64(p) != 0
        |            count += 1
        |        i += 1
        |    count
        |""".stripMargin)) shouldBe 100
  }

  "size classes: small and large allocations coexist" in {
    compileAndRun(withAlloc(
      """var heap: [16384]i8
        |
        |main() -> int
        |    _sysl_alloc_init(&heap, &heap + 16384)
        |    val small1 = _sysl_alloc(8)
        |    val big1 = _sysl_alloc(4000)
        |    val small2 = _sysl_alloc(8)
        |    val ps1: *i64 = *i64(i64(small1))
        |    val ps2: *i64 = *i64(i64(small2))
        |    *ps1 = 10
        |    *ps2 = 32
        |    int(*ps1 + *ps2)
        |""".stripMargin)) shouldBe 42
  }
}
