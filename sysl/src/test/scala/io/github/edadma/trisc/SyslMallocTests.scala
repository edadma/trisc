package io.github.edadma.trisc

class SyslMallocTests extends SyslTestHelpers {

  // ===== Basic malloc/free =====

  "malloc returns non-null" in {
    eval(
      """main() -> int
        |    val p = malloc(16i64)
        |    if i64(p) != 0 then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "malloc and write through pointer" in {
    eval(
      """main() -> int
        |    val p: *int = *int(i64(malloc(8i64)))
        |    *p = 42
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "free null is safe" in {
    eval(
      """main() -> int
        |    free(*i8(0))
        |    1
        |""".stripMargin) shouldBe 1
  }

  "malloc two separate allocations" in {
    eval(
      """main() -> int
        |    val a: *int = *int(i64(malloc(8i64)))
        |    val b: *int = *int(i64(malloc(8i64)))
        |    *a = 10
        |    *b = 20
        |    *a + *b
        |""".stripMargin) shouldBe 30
  }

  "malloc allocations are independent" in {
    eval(
      """main() -> int
        |    val a: *int = *int(i64(malloc(4i64)))
        |    val b: *int = *int(i64(malloc(4i64)))
        |    *a = 111
        |    *b = 222
        |    // writing to b must not clobber a
        |    *a
        |""".stripMargin) shouldBe 111
  }

  "free then reuse" in {
    eval(
      """main() -> int
        |    val p: *int = *int(i64(malloc(8i64)))
        |    *p = 42
        |    free(*i8(i64(p)))
        |    val q: *int = *int(i64(malloc(8i64)))
        |    *q = 99
        |    *q
        |""".stripMargin) shouldBe 99
  }

  // ===== calloc =====

  "calloc returns zeroed memory" in {
    eval(
      """main() -> int
        |    val p: *int = *int(i64(calloc(4i64, 8i64)))
        |    p[0] + p[1] + p[2] + p[3]
        |""".stripMargin) shouldBe 0
  }

  "calloc write and read" in {
    eval(
      """main() -> int
        |    val p: *int = *int(i64(calloc(3i64, 8i64)))
        |    p[0] = 10
        |    p[1] = 20
        |    p[2] = 30
        |    p[0] + p[1] + p[2]
        |""".stripMargin) shouldBe 60
  }

  // ===== realloc =====

  "realloc preserves data" in {
    eval(
      """main() -> int
        |    var p: *int = *int(i64(malloc(8i64)))
        |    *p = 99
        |    p = *int(i64(realloc(*i8(i64(p)), 16i64)))
        |    *p
        |""".stripMargin) shouldBe 99
  }

  "realloc null acts as malloc" in {
    eval(
      """main() -> int
        |    val p: *int = *int(i64(realloc(*i8(0), 8i64)))
        |    *p = 7
        |    *p
        |""".stripMargin) shouldBe 7
  }

  "realloc preserves multiple values" in {
    eval(
      """main() -> int
        |    var p: *int = *int(i64(malloc(16i64)))
        |    p[0] = 10
        |    p[1] = 20
        |    p = *int(i64(realloc(*i8(i64(p)), 32i64)))
        |    p[0] + p[1]
        |""".stripMargin) shouldBe 30
  }

  // ===== sbrk =====

  "sbrk zero returns current break" in {
    eval(
      """main() -> int
        |    val p = sbrk(0)
        |    if i64(p) != 0 then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "sbrk advances break" in {
    eval(
      """main() -> int
        |    val a = sbrk(0)
        |    val b = sbrk(100)
        |    val c = sbrk(0)
        |    // b should equal old break (a), c should be a+100
        |    if i64(b) == i64(a) then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "sbrk write and read through returned pointer" in {
    eval(
      """main() -> int
        |    val p: *int = *int(i64(sbrk(8)))
        |    *p = 55
        |    *p
        |""".stripMargin) shouldBe 55
  }

  // ===== Pointer ↔ integer round-tripping =====

  "i64 of local pointer round-trips" in {
    eval(
      """main() -> int
        |    var x = 42
        |    var p: *int = &x
        |    val addr = i64(p)
        |    var q: *int = *int(addr)
        |    *q
        |""".stripMargin) shouldBe 42
  }

  "i64 of array pointer round-trips" in {
    eval(
      """main() -> int
        |    var arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    var p: *int = &arr[1]
        |    val addr = i64(p)
        |    var q: *int = *int(addr)
        |    *q
        |""".stripMargin) shouldBe 20
  }

  "null pointer is address zero" in {
    eval(
      """main() -> int
        |    var p: *int = *int(0)
        |    i64(p)
        |""".stripMargin) shouldBe 0
  }

  "pointer to int and back preserves identity" in {
    eval(
      """main() -> int
        |    var x = 99
        |    var p: *int = &x
        |    val a1 = i64(p)
        |    var q: *int = *int(a1)
        |    val a2 = i64(q)
        |    if a1 == a2 then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "write through round-tripped pointer" in {
    eval(
      """main() -> int
        |    var x = 0
        |    var p: *int = &x
        |    val addr = i64(p)
        |    var q: *int = *int(addr)
        |    *q = 77
        |    x
        |""".stripMargin) shouldBe 77
  }

  "pointer arithmetic via integer" in {
    eval(
      """main() -> int
        |    var arr: [4]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    arr[3] = 40
        |    var p: *int = &arr[0]
        |    val base = i64(p)
        |    // offset by 2 elements
        |    var q: *int = *int(base + 2)
        |    *q
        |""".stripMargin) shouldBe 30
  }

  // ===== Allocated memory: pointer walk and indexing =====

  "malloc array write and index read" in {
    eval(
      """main() -> int
        |    val p: *int = *int(i64(malloc(32i64)))
        |    p[0] = 1
        |    p[1] = 2
        |    p[2] = 3
        |    p[3] = 4
        |    p[0] + p[1] + p[2] + p[3]
        |""".stripMargin) shouldBe 10
  }

  "malloc pointer walk with increment" in {
    eval(
      """main() -> int
        |    val base: *int = *int(i64(malloc(24i64)))
        |    base[0] = 10
        |    base[1] = 20
        |    base[2] = 30
        |    var p = base
        |    var sum = 0
        |    var i = 0
        |    while i < 3
        |        sum += *p
        |        p = p + 1
        |        i++
        |    sum
        |""".stripMargin) shouldBe 60
  }

  "malloc pointer arithmetic add" in {
    eval(
      """main() -> int
        |    val p: *int = *int(i64(malloc(16i64)))
        |    p[0] = 100
        |    p[1] = 200
        |    val q = p + 1
        |    *q
        |""".stripMargin) shouldBe 200
  }

  "malloc used as dynamic array in function" in {
    eval(
      """fill(p: *int, n: int, v: int)
        |    var i = 0
        |    while i < n
        |        p[i] = v
        |        i++
        |
        |sum(p: *int, n: int) -> int
        |    var total = 0
        |    var i = 0
        |    while i < n
        |        total += p[i]
        |        i++
        |    total
        |
        |main() -> int
        |    val buf: *int = *int(i64(malloc(40i64)))
        |    fill(buf, 5, 7)
        |    sum(buf, 5)
        |""".stripMargin) shouldBe 35
  }

  "malloc struct-like layout" in {
    eval(
      """main() -> int
        |    // allocate 2 cells, use as x/y pair via indexing
        |    val p: *int = *int(i64(malloc(16i64)))
        |    p[0] = 10
        |    p[1] = 20
        |    p[0] + p[1]
        |""".stripMargin) shouldBe 30
  }

  // ===== User function overrides builtin =====

  "user-defined malloc overrides builtin" in {
    eval(
      """malloc(size: i64) -> *i8 = *i8(0)
        |
        |main() -> int
        |    val p = malloc(16i64)
        |    if i64(p) == 0 then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "user-defined free overrides builtin" in {
    eval(
      """var freed = false
        |
        |free(ptr: *i8)
        |    freed = true
        |
        |main() -> int
        |    val p = malloc(8i64)
        |    free(p)
        |    if freed then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }
}
