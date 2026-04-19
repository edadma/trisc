package io.github.edadma.trisc

class SyslTriscStringRefcountTests extends SyslCodegenHelpers {

  // Re-use the standard posix allocator setup for tests that exercise heap strings.
  private val allocSource = scala.io.Source.fromFile("posix/stdlib/alloc.sysl").mkString
  private val stringSource = scala.io.Source.fromFile("posix/string/string.sysl").mkString
  private val ctypeSource = scala.io.Source.fromFile("posix/ctype/ctype.sysl").mkString

  private def sbrkModule(heapSize: Int): String =
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

  private def runWithAlloc(source: String, heapSize: Int = 16384): Long =
    compileMultiAndRun(allocSources(s"import posix.stdlib.*\n\n$source", heapSize))

  // ====================================================================
  // 1. Literal layout (asm-level)
  // ====================================================================

  "literal global has immortal refcount sentinel" in {
    val asm = compile(
      """main()
        |    val s = "hello"
        |""".stripMargin)
    asm should include("dl -1")
  }

  "concat asm sets refcount=1 in newly allocated buffer" in {
    val asm = compile(
      """main()
        |    val s = "abc" + "def"
        |""".stripMargin)
    asm should include("ldi r2, 1")
  }

  "string(ptr,len) asm calls malloc and stores rc=1" in {
    val asm = compile(
      """main()
        |    var arr = new [3]byte
        |    arr[0] = 65
        |    val s = string(&arr[0], 3)
        |""".stripMargin)
    asm should include("malloc")
    asm should include("ldi r2, 1")
  }

  // ====================================================================
  // 2. Aliasing — no malloc needed for literals
  // ====================================================================

  "literal aliasing many times does not segfault" in {
    compileAndRun(
      """main() -> int
        |    val s = "hello"
        |    val t = s
        |    val u = t
        |    val v = u
        |    if v == "hello" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "string passed to function: caller still owns" in {
    compileAndRun(
      """get_len(s: string) -> int = len(s)
        |
        |main() -> int
        |    val s = "hello world"
        |    val n = get_len(s)
        |    if n == 11 && s == "hello world" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "literal in loop body — no leak (immortal incr/decr)" in {
    compileAndRun(
      """main() -> int
        |    var i = 0
        |    while i < 100
        |        val s = "abc"
        |        val t = s
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 3. Concat (heap allocation)
  // ====================================================================

  "concat result freed after consumption (no heap exhaustion in loop)" in {
    // CPU is capped at 100k cycles. Use a modest iteration count that still
    // demonstrates buffer reuse — without proper free, 50 × ~16B would still fit
    // in 16K heap, so this is mostly a no-crash check.
    runWithAlloc(
      """main() -> int
        |    var i = 0
        |    while i < 50
        |        val s = "abc" + "def"
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "self-concat does not free source mid-evaluation" in {
    runWithAlloc(
      """main() -> int
        |    var s = "abc" + "def"
        |    s = s + "ghi"
        |    if s == "abcdefghi" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "repeated self-concat in loop frees old buffers" in {
    runWithAlloc(
      """main() -> int
        |    var s = "x"
        |    var i = 0
        |    while i < 20
        |        s = s + "y"
        |        i += 1
        |    if len(s) == 21 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "reassign concat→concat decrements old" in {
    runWithAlloc(
      """main() -> int
        |    var s = "abc" + "def"
        |    s = "xyz" + "qrs"
        |    if s == "xyzqrs" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "long concat chain — temporaries freed" in {
    runWithAlloc(
      """main() -> int
        |    val s = "a" + "b" + "c" + "d" + "e" + "f"
        |    if s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "nested concat (a+b)+(c+d) — temporaries freed in loop" in {
    runWithAlloc(
      """main() -> int
        |    var i = 0
        |    while i < 20
        |        val s = ("ab" + "cd") + ("ef" + "gh")
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 4. string(ptr,len) — copy semantics
  // ====================================================================

  "string(ptr,len) copies — outlives source array" in {
    runWithAlloc(
      """build() -> string
        |    var arr = new [5]byte
        |    arr[0] = 104
        |    arr[1] = 101
        |    arr[2] = 108
        |    arr[3] = 108
        |    arr[4] = 111
        |    string(&arr[0], 5)
        |
        |main() -> int
        |    val s = build()
        |    if s == "hello" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 5. Function parameters & returns
  // ====================================================================

  "function returns concat repeatedly without leak" in {
    runWithAlloc(
      """make() -> string = "aaaa" + "bbbb"
        |
        |main() -> int
        |    var i = 0
        |    while i < 30
        |        val s = make()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "function returns its own param — caller's share preserved" in {
    runWithAlloc(
      """passthrough(s: string) -> string = s
        |
        |main() -> int
        |    val src = "abc" + "def"
        |    val r = passthrough(src)
        |    if r == "abcdef" && src == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "string param passed through multiple functions" in {
    runWithAlloc(
      """f1(s: string) -> int = f2(s)
        |f2(s: string) -> int = f3(s)
        |f3(s: string) -> int = len(s)
        |
        |main() -> int
        |    val s = "abc" + "def"
        |    val n = f1(s)
        |    if n == 6 && s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 6. Struct fields
  // ====================================================================

  "struct holds concat — buffer survives source going out of scope" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |build() -> Holder
        |    val tmp = "abc" + "def"
        |    Holder(tmp)
        |
        |main() -> int
        |    val h = build()
        |    if h.s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "string field reassignment frees old field buffer" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var h = Holder("abc" + "def")
        |    h.s = "xyz" + "qrs"
        |    if h.s == "xyzqrs" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 7. If-expr branches
  // ====================================================================

  "if-expr returning literal vs concat" in {
    runWithAlloc(
      """build(b: bool) -> string =
        |    if b then "literal" else "abc" + "def"
        |
        |main() -> int
        |    val a = build(true)
        |    val b = build(false)
        |    if a == "literal" && b == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "if-expr returning concat both branches in loop" in {
    runWithAlloc(
      """main() -> int
        |    var i = 0
        |    var s = "init"
        |    while i < 20
        |        s = if i % 2 == 0 then "even" + "_" else "odd" + "_"
        |        i += 1
        |    if s == "odd_" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 8. Match expression
  // ====================================================================

  "match expr returning literal vs concat" in {
    runWithAlloc(
      """build(n: int) -> string =
        |    n match
        |        0 -> "zero"
        |        else -> "abc" + "def"
        |
        |main() -> int
        |    val a = build(0)
        |    val b = build(1)
        |    if a == "zero" && b == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 9. Globals
  // ====================================================================

  "global string assigned concat" in {
    runWithAlloc(
      """var g: string
        |
        |main() -> int
        |    g = "global" + "_value"
        |    if g == "global_value" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "global string reassigned in loop" in {
    runWithAlloc(
      """var g: string
        |
        |main() -> int
        |    g = "init"
        |    var i = 0
        |    while i < 30
        |        g = "iter" + "_value"
        |        i += 1
        |    if g == "iter_value" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 10. Comparison & length
  // ====================================================================

  "comparison does not consume the strings" in {
    runWithAlloc(
      """main() -> int
        |    val s = "abc" + "def"
        |    val t = "abc" + "def"
        |    val eq1 = s == t
        |    val eq2 = s == t
        |    val eq3 = s == "abcdef"
        |    if eq1 && eq2 && eq3 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "len of concat result, then use the string" in {
    runWithAlloc(
      """main() -> int
        |    val s = "abc" + "def"
        |    val n = len(s)
        |    if n == 6 && s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 11. Early returns
  // ====================================================================

  "early return decrements live string locals" in {
    runWithAlloc(
      """check(b: bool) -> int
        |    val s = "abc" + "def"
        |    if !b then return 1
        |    if len(s) == 6 then 0 else 2
        |
        |main() -> int = check(true)
        |""".stripMargin) shouldBe 0
  }

  "early return with concat returned" in {
    runWithAlloc(
      """make(b: bool) -> string
        |    if b then return "abc" + "def"
        |    "xyz"
        |
        |main() -> int
        |    val s = make(true)
        |    if s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 12. Value-struct cleanup — string fields decremented on scope exit
  // ====================================================================

  "value-struct local with string field freed on scope exit" in {
    // 16K heap, ~24B per allocation (8 hdr + 6 data + padding) — without per-field
    // decrement, ~600 iterations would exhaust the heap. 100 iterations is plenty
    // to prove the leak is gone within 100k cycle budget.
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var i = 0
        |    while i < 100
        |        var h = Holder("abc" + "def")
        |        if h.s != "abcdef" then return 1
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "value-struct reassignment frees old string field" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var h = Holder("abc" + "def")
        |    var i = 0
        |    while i < 50
        |        h = Holder("xyz" + "qrs")
        |        i += 1
        |    if h.s == "xyzqrs" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "value-struct returned from function — string field survives" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |build() -> Holder
        |    var h = Holder("part" + "_one")
        |    h
        |
        |main() -> int
        |    var i = 0
        |    while i < 50
        |        var h = build()
        |        if h.s != "part_one" then return 1
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "value-struct with two string fields freed on exit" in {
    runWithAlloc(
      """struct Pair
        |    a: string
        |    b: string
        |
        |main() -> int
        |    var i = 0
        |    while i < 20
        |        var p = Pair("first" + "_a", "second" + "_b")
        |        if p.a != "first_a" || p.b != "second_b" then return 1
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "nested value-struct with string field freed on exit" in {
    runWithAlloc(
      """struct Inner
        |    s: string
        |
        |struct Outer
        |    inner: Inner
        |    label: string
        |
        |main() -> int
        |    var i = 0
        |    while i < 30
        |        var o = Outer(Inner("deep" + "_str"), "top" + "_str")
        |        if o.inner.s != "deep_str" || o.label != "top_str" then return 1
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "value-struct holding borrowed string field — caller's local survives" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var msg = "abc" + "def"
        |    var i = 0
        |    while i < 50
        |        var h = Holder(msg)
        |        if h.s != "abcdef" then return 1
        |        i += 1
        |    if msg == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "early return with value-struct local — fields freed" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |check(b: bool) -> int
        |    var h = Holder("abc" + "def")
        |    if !b then return 1
        |    if len(h.s) == 6 then 0 else 2
        |
        |main() -> int = check(true)
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 13. Value-struct param ABI — true pass-by-value semantics
  // ====================================================================

  "value-struct param: field read works" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |get_len(h: Holder) -> int = len(h.s)
        |
        |main() -> int
        |    var h = Holder("abc" + "def")
        |    val n = get_len(h)
        |    if n == 6 && h.s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "value-struct param: callee mutation does not leak to caller" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |mutate(h: Holder) -> int
        |    h.s = "mutated"
        |    len(h.s)
        |
        |main() -> int
        |    var h = Holder("orig" + "inal")
        |    val n = mutate(h)
        |    if h.s == "original" && n == 7 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "value-struct param passed in loop — no heap leak" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |inspect(h: Holder) -> int = len(h.s)
        |
        |main() -> int
        |    var h = Holder("abc" + "def")
        |    var i = 0
        |    while i < 30
        |        if inspect(h) != 6 then return 1
        |        i += 1
        |    if h.s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "value-struct returned from function taking same struct as param" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |passthrough(h: Holder) -> Holder = h
        |
        |main() -> int
        |    var h = Holder("abc" + "def")
        |    val r = passthrough(h)
        |    if r.s == "abcdef" && h.s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "two value-struct params (second is stack-passed)" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |concat_lens(a: Holder, b: Holder) -> int = len(a.s) + len(b.s)
        |
        |main() -> int
        |    var x = Holder("first" + "_a")
        |    var y = Holder("second" + "_b")
        |    val n = concat_lens(x, y)
        |    if n == 15 && x.s == "first_a" && y.s == "second_b" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "value-struct param chained through two function calls" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |inner(h: Holder) -> int = len(h.s)
        |outer(h: Holder) -> int = inner(h)
        |
        |main() -> int
        |    var h = Holder("abc" + "def")
        |    val n = outer(h)
        |    if n == 6 && h.s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 14. Indirect calls (function pointers / closures) with aggregate args
  // ====================================================================

  "function pointer with string arg" in {
    runWithAlloc(
      """get_len(s: string) -> int = len(s)
        |
        |main() -> int
        |    val f = get_len
        |    val s = "abc" + "def"
        |    val n = f(s)
        |    if n == 6 && s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "function pointer with value-struct arg" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |get_len(h: Holder) -> int = len(h.s)
        |
        |main() -> int
        |    val f = get_len
        |    var h = Holder("abc" + "def")
        |    val n = f(h)
        |    if n == 6 && h.s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "function pointer returning value-struct" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |make() -> Holder = Holder("abc" + "def")
        |
        |main() -> int
        |    val f = make
        |    val h = f()
        |    if h.s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "function pointer mutation via value-struct param does not leak to caller" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |mutate(h: Holder) -> int
        |    h.s = "mutated"
        |    len(h.s)
        |
        |main() -> int
        |    val f = mutate
        |    var h = Holder("orig" + "inal")
        |    val n = f(h)
        |    if h.s == "original" && n == 7 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 15. Interface dispatch with aggregate args
  // ====================================================================

  "interface method with string arg" in {
    runWithAlloc(
      """interface Sizer
        |    size(s: string) -> int
        |
        |struct Counter
        |    n: int
        |
        |Counter.size(s: string) -> int = len(s) + self.n
        |
        |main() -> int
        |    var c = Counter(10)
        |    val sz: Sizer = c
        |    val s = "abc" + "def"
        |    val n = sz.size(s)
        |    if n == 16 && s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "interface method with value-struct arg" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |interface Inspector
        |    check(h: Holder) -> int
        |
        |struct Probe
        |    n: int
        |
        |Probe.check(h: Holder) -> int = len(h.s)
        |
        |main() -> int
        |    var p = Probe(0)
        |    val ins: Inspector = p
        |    var h = Holder("abc" + "def")
        |    val n = ins.check(h)
        |    if n == 6 && h.s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "interface method mutation via value-struct param does not leak to caller" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |interface Mutator
        |    mutate(h: Holder) -> int
        |
        |struct Doer
        |    n: int
        |
        |Doer.mutate(h: Holder) -> int
        |    h.s = "mutated"
        |    len(h.s)
        |
        |main() -> int
        |    var d = Doer(0)
        |    val m: Mutator = d
        |    var h = Holder("orig" + "inal")
        |    val n = m.mutate(h)
        |    if h.s == "original" && n == 7 then 0 else 1
        |""".stripMargin) shouldBe 0
  }
}
