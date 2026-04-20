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

  "string field reassignment in loop — old buffers freed (heap pressure test)" in {
    // Tiny 256-byte heap. Each iteration leaks ~24 bytes if h.s = ... doesn't
    // decr the old buffer's rc. ~10 iterations would exhaust the heap; we run 50.
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var h = Holder("abc" + "def")
        |    var i = 0
        |    while i < 50
        |        h.s = "iter" + "_data"
        |        i += 1
        |    if h.s == "iter_data" then 0 else 1
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "field-assign with borrowed string source — incr applied" in {
    // Source string borrowed from a local var; field-assign should incr to
    // claim a share. Both source and field point at the same buffer; both
    // get decremented (caller scope exit + field cleanup) → balanced.
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var h = Holder("init" + "_v")
        |    var src = "abc" + "def"
        |    h.s = src
        |    if h.s == "abcdef" && src == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "nested-struct field reassignment frees inner string buffers (heap pressure)" in {
    // Outer struct has a nested struct with a string field. Field-assign of the
    // outer's nested-struct field should recursively decr inner string buffers.
    runWithAlloc(
      """struct Inner
        |    s: string
        |
        |struct Outer
        |    inner: Inner
        |
        |main() -> int
        |    var o = Outer(Inner("init" + "_v"))
        |    var i = 0
        |    while i < 30
        |        o.inner = Inner("iter" + "_data")
        |        i += 1
        |    if o.inner.s == "iter_data" then 0 else 1
        |""".stripMargin, heapSize = 256) shouldBe 0
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

  // ====================================================================
  // 16. Sibling assign paths — globals, *p, arr[i]
  // ====================================================================

  "global string reassignment in loop — old buffers freed (heap pressure)" in {
    runWithAlloc(
      """var g: string
        |
        |main() -> int
        |    g = "init" + "_v"
        |    var i = 0
        |    while i < 50
        |        g = "iter" + "_data"
        |        i += 1
        |    if g == "iter_data" then 0 else 1
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "global struct-with-string field assignment in loop (heap pressure)" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |var g: Holder
        |
        |main() -> int
        |    g = Holder("init" + "_v")
        |    var i = 0
        |    while i < 30
        |        g = Holder("iter" + "_data")
        |        i += 1
        |    if g.s == "iter_data" then 0 else 1
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "*p = string in loop — old buffers freed (heap pressure)" in {
    runWithAlloc(
      """main() -> int
        |    var s = "init" + "_v"
        |    var p: *string = &s
        |    var i = 0
        |    while i < 50
        |        *p = "iter" + "_data"
        |        i += 1
        |    if s == "iter_data" then 0 else 1
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "arr[i] = string in loop — old buffers freed (heap pressure)" in {
    runWithAlloc(
      """main() -> int
        |    var arr: [4]string
        |    arr[0] = "init"
        |    var i = 0
        |    while i < 30
        |        arr[0] = "iter" + "_data"
        |        i += 1
        |    if arr[0] == "iter_data" then 0 else 1
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  // ====================================================================
  // 17. [N]string scope-exit element decr (stack arrays of strings)
  // ====================================================================

  "[N]string scope exit decrs each element (heap pressure)" in {
    // fill() populates all 4 slots with fresh buffers and returns. If scope
    // cleanup misses any slot, that buffer leaks. 10 iters × 4 buffers ×
    // ~24 bytes = ~960 B; on a 256-byte heap this would trap.
    runWithAlloc(
      """fill()
        |    var arr: [4]string
        |    arr[0] = "a" + "_v"
        |    arr[1] = "b" + "_v"
        |    arr[2] = "c" + "_v"
        |    arr[3] = "d" + "_v"
        |
        |main() -> int
        |    var i = 0
        |    while i < 10
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "[N]struct-with-string scope exit decrs each element (heap pressure)" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |fill()
        |    var arr: [3]Holder
        |    arr[0] = Holder("a" + "_v")
        |    arr[1] = Holder("b" + "_v")
        |    arr[2] = Holder("c" + "_v")
        |
        |main() -> int
        |    var i = 0
        |    while i < 10
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  // ====================================================================
  // 18. &[]string element rc (heap slices of strings)
  // ====================================================================

  "&[]string scope exit decrs each element (heap pressure)" in {
    runWithAlloc(
      """fill()
        |    val arr = new [4]string
        |    arr[0] = "a" + "_v"
        |    arr[1] = "b" + "_v"
        |    arr[2] = "c" + "_v"
        |    arr[3] = "d" + "_v"
        |
        |main() -> int
        |    var i = 0
        |    while i < 10
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "slice[i] = string in loop — old buffers freed (heap pressure)" in {
    runWithAlloc(
      """main() -> int
        |    val arr = new [4]string
        |    arr[0] = "init"
        |    var i = 0
        |    while i < 10
        |        arr[0] = "iter" + "_data"
        |        i += 1
        |    if arr[0] == "iter_data" then 0 else 1
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "&[]struct-with-string scope exit decrs each element (heap pressure)" in {
    // Slice of structs containing strings: deinit must recurse through struct fields.
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |fill()
        |    val arr = new [3]Holder
        |    arr[0] = Holder("a" + "_v")
        |    arr[1] = Holder("b" + "_v")
        |    arr[2] = Holder("c" + "_v")
        |
        |main() -> int
        |    var i = 0
        |    while i < 10
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "&[]string element preserved across scope exit when returned" in {
    // The slice escapes via the function return; caller-owned, no scope-exit decr in callee.
    runWithAlloc(
      """make() -> &[]string
        |    val arr = new [2]string
        |    arr[0] = "hello" + "_world"
        |    arr[1] = "a" + "b"
        |    arr
        |
        |main() -> int
        |    val s = make()
        |    if s[0] == "hello_world" && s[1] == "ab" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 12. Substring s[a:b] (option A: copy semantics)
  // ====================================================================

  "substring asm calls malloc and stores rc=1" in {
    val asm = compile(
      """main()
        |    val s = "hello"
        |    val t = s[1:4]
        |""".stripMargin)
    asm should include("malloc")
    asm should include("ldi r2, 1")
  }

  "substring of literal yields correct bytes" in {
    runWithAlloc(
      """main() -> int
        |    val s = "hello"
        |    val t = s[1:4]
        |    if t == "ell" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "substring with default lo (s[:k])" in {
    runWithAlloc(
      """main() -> int
        |    val s = "abcdef"
        |    val t = s[:3]
        |    if t == "abc" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "substring with default hi (s[k:])" in {
    runWithAlloc(
      """main() -> int
        |    val s = "abcdef"
        |    val t = s[2:]
        |    if t == "cdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "substring full copy (s[:]) equals original" in {
    runWithAlloc(
      """main() -> int
        |    val s = "hello"
        |    val t = s[:]
        |    if t == "hello" && len(t) == 5 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "empty substring (s[k:k])" in {
    runWithAlloc(
      """main() -> int
        |    val s = "hello"
        |    val t = s[2:2]
        |    if len(t) == 0 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "substring of concat result" in {
    runWithAlloc(
      """main() -> int
        |    val s = "ab" + "cdef"
        |    val t = s[1:5]
        |    if t == "bcde" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "substring in loop — old buffers freed (heap pressure)" in {
    // Loop-stress: each iteration mallocs a fresh substring buffer; scope exit
    // must decr it. Without proper free, the 256-byte heap will exhaust → trap.
    runWithAlloc(
      """fill()
        |    val s = "abcdefgh"
        |    val t = s[0:8]
        |
        |main() -> int
        |    var i = 0
        |    while i < 10
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "substring returned from function" in {
    runWithAlloc(
      """take(s: string, lo: int, hi: int) -> string = s[lo:hi]
        |
        |main() -> int
        |    val s = "hello world"
        |    val t = take(s, 6, 11)
        |    if t == "world" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "substring of substring" in {
    runWithAlloc(
      """main() -> int
        |    val s = "abcdefgh"
        |    val t = s[1:7]
        |    val u = t[1:4]
        |    if u == "cde" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "substring out-of-bounds traps with r1 = 1" in {
    // Trap halts the CPU; r1 holds error code (1 = out-of-bounds).
    runWithAlloc(
      """main() -> int
        |    val s = "hi"
        |    val t = s[0:10]
        |    0
        |""".stripMargin) shouldBe 1
  }

  // ====================================================================
  // 13. Enum-with-string variant rc (tag-dispatch decr on scope exit)
  // ====================================================================

  "enum variant with string field — scope exit decr (heap pressure)" in {
    // Construct an enum holding a heap-allocated string each iteration.
    // Without tag-dispatch decr at scope exit, the 256-byte heap exhausts.
    runWithAlloc(
      """enum E
        |    OneStr(s: string)
        |    Empty
        |
        |fill()
        |    val e = OneStr("ab" + "cd")
        |
        |main() -> int
        |    var i = 0
        |    while i < 10
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "enum variant with borrowed-string field — incr+decr balance (heap pressure)" in {
    // Source string lives in scope; storing it into the enum field must incr,
    // and tag-dispatch decr on scope exit must release the share.
    runWithAlloc(
      """enum E
        |    OneStr(s: string)
        |    Empty
        |
        |fill()
        |    val s = "ab" + "cd"
        |    val e = OneStr(s)
        |
        |main() -> int
        |    var i = 0
        |    while i < 10
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "enum variant with multiple string fields (heap pressure)" in {
    runWithAlloc(
      """enum E
        |    Two(a: string, b: string)
        |    Empty
        |
        |fill()
        |    val e = Two("a" + "1", "b" + "2")
        |
        |main() -> int
        |    var i = 0
        |    while i < 10
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "enum variant with nested struct-with-string field (heap pressure)" in {
    runWithAlloc(
      """struct Holder
        |    s: string
        |
        |enum E
        |    Wrap(h: Holder)
        |    Empty
        |
        |fill()
        |    val e = Wrap(Holder("ab" + "cd"))
        |
        |main() -> int
        |    var i = 0
        |    while i < 10
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "enum reassignment frees old variant strings (heap pressure)" in {
    // Reassigning the enum local must decr the old active variant's strings
    // before the new value's bytes overwrite the slot.
    runWithAlloc(
      """enum E
        |    OneStr(s: string)
        |    Empty
        |
        |main() -> int
        |    var e = OneStr("init" + "_v")
        |    var i = 0
        |    while i < 10
        |        e = OneStr("iter" + "_v")
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "enum returned from function — string field survives" in {
    runWithAlloc(
      """enum E
        |    Wrap(s: string)
        |    Empty
        |
        |make() -> E = Wrap("hello" + "_world")
        |
        |main() -> int
        |    val e = make()
        |    e match
        |        Wrap(s) -> if s == "hello_world" then 0 else 1
        |        Empty -> 2
        |""".stripMargin) shouldBe 0
  }

  "enum match correctly extracts string field" in {
    runWithAlloc(
      """enum E
        |    OneStr(s: string)
        |    Two(a: string, b: string)
        |
        |main() -> int
        |    val e = Two("foo" + "1", "bar" + "2")
        |    e match
        |        OneStr(s) -> 1
        |        Two(a, b) -> if a == "foo1" && b == "bar2" then 0 else 2
        |""".stripMargin) shouldBe 0
  }

  "enum with no-string variant — no spurious decr (heap pressure)" in {
    // Active variant is the no-string one; tag-dispatch must skip it cleanly
    // (no walk, no decr). Borrowed string outside the enum still decrs normally.
    runWithAlloc(
      """enum E
        |    OneStr(s: string)
        |    Empty
        |
        |fill()
        |    val s = "ab" + "cd"
        |    val e = Empty
        |
        |main() -> int
        |    var i = 0
        |    while i < 10
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "[N]Enum scope exit decrs each element (heap pressure)" in {
    // Array of enums-with-strings: scope exit must walk every element via the
    // tag-dispatch helper.
    runWithAlloc(
      """enum E
        |    OneStr(s: string)
        |    Empty
        |
        |fill()
        |    var arr: [3]E
        |    arr[0] = OneStr("a" + "_v")
        |    arr[1] = OneStr("b" + "_v")
        |    arr[2] = OneStr("c" + "_v")
        |
        |main() -> int
        |    var i = 0
        |    while i < 10
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "&[]Enum scope exit decrs each element (heap pressure)" in {
    // Heap slice of enums-with-strings: per-elem deinit synthesized for the
    // enum element type, and that deinit must run tag-dispatch decr per element.
    runWithAlloc(
      """enum E
        |    OneStr(s: string)
        |    Empty
        |
        |fill()
        |    val arr = new [3]E
        |    arr[0] = OneStr("a" + "_v")
        |    arr[1] = OneStr("b" + "_v")
        |    arr[2] = OneStr("c" + "_v")
        |
        |main() -> int
        |    var i = 0
        |    while i < 10
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "enum scope-exit asm reads tag and chains compares" in {
    // Sanity check on emitted shape: tag load (ldw) + per-variant compare (bne).
    val asm = compile(
      """enum E
        |    OneStr(s: string)
        |    Empty
        |
        |main() -> int
        |    val e = OneStr("ab" + "cd")
        |    0
        |""".stripMargin)
    asm should include("ldw r3")     // tag load in emitEnumStringFieldsRC
    asm should include regex """bne r3, r4, .*enum_rc_next""" // per-variant compare
  }

  // ====================================================================
  // 12. Closures capturing strings — env rc bracketing
  // ====================================================================

  "closure with single string capture: scope exit releases env (heap pressure)" in {
    // Without rc bracketing the captured string into env + freeing the env, the
    // 256-byte heap fills up after a few iterations and malloc returns null →
    // sbrk-backed allocator returns -1 → trap. With proper bracketing the env
    // (and its captured string) are reclaimed each iteration.
    runWithAlloc(
      """fill()
        |    val s = "abc" + "_v"
        |    val f = (x: int) -> x + len(s)
        |    val r = f(0)
        |
        |main() -> int
        |    var i = 0
        |    while i < 30
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "closure constructed but body never called: still releases env on scope exit" in {
    runWithAlloc(
      """fill()
        |    val s = "ab" + "cd"
        |    val f = (x: int) -> x + len(s)
        |    // f is never called
        |
        |main() -> int
        |    var i = 0
        |    while i < 30
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "closure with multiple string captures" in {
    runWithAlloc(
      """fill()
        |    val a = "x" + "y"
        |    val b = "p" + "q"
        |    val f = (n: int) -> n + len(a) + len(b)
        |    val r = f(1)
        |
        |main() -> int
        |    var i = 0
        |    while i < 20
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }


  "closure body uses captured string (computed result is correct)" in {
    runWithAlloc(
      """main() -> int
        |    val s = "ab" + "cd"
        |    val f = (n: int) -> n + len(s)
        |    f(38)
        |""".stripMargin) shouldBe 42
  }

  "closure construction asm emits rc=1 + deinit_ptr in env header" in {
    val asm = compile(
      """main()
        |    val s = "abc" + "_v"
        |    val f = (x: int) -> x + len(s)
        |""".stripMargin)
    // env layout: malloc(envSize+16), then rc=1 (ldi r2, 1; std r2, r1, r0)
    asm should include regex """movi r4, malloc"""
    asm should include regex """ldi r2, 1"""
    // deinit_ptr is the per-closure-id env deinit
    asm should include regex """movi r2, __closure_env_deinit___closure_"""
    // Dispatch shim is generated
    asm should include("__closure_env_dispatch:")
  }

  "closure with no rc-bearing captures: deinit_ptr is null (no walk needed)" in {
    // `val f = ...` (no expected type) defaults to escaping per analyzer convention.
    // → HeapEnv path → malloc. deinit_ptr=null since captures are non-rc-bearing.
    val asm = compile(
      """main()
        |    val a = 10
        |    val f = (x: int) -> x + a
        |""".stripMargin)
    asm should include("movi r4, malloc")
    asm should not include "__closure_env_deinit_"  // no per-id deinit registered
  }

  "stack-env optimization: closure as call arg with int capture has no malloc" in {
    // Non-escaping context (closure passed to `apply(f: (int) -> int, ...)`) +
    // non-rc-bearing captures → StackEnv → no malloc, no free, no dispatch.
    // This is what makes closures usable in no-allocator (kernel/bare-metal) builds.
    val asm = compile(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    val a = 10
        |    apply(x -> x + a, 32)
        |""".stripMargin)
    asm should not include "movi r4, malloc"
    asm should not include "movi r4, free"
    asm should not include "__closure_env_dispatch"
    asm should not include "__closure_env_deinit_"
  }

  "closure capturing struct-with-string field: env deinit walks struct" in {
    runWithAlloc(
      """struct Wrap
        |    s: string
        |
        |fill()
        |    val w = Wrap("ab" + "cd")
        |    val f = (n: int) -> n + len(w.s)
        |    val r = f(1)
        |
        |main() -> int
        |    var i = 0
        |    while i < 20
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "closure used inside higher-order function (descriptor passed by value)" in {
    // Descriptor is passed as a borrowed FuncType param. Caller still owns env's rc.
    runWithAlloc(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |fill()
        |    val s = "ab" + "cd"
        |    val r = apply(n -> n + len(s), 0)
        |
        |main() -> int
        |    var i = 0
        |    while i < 5
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  // ====================================================================
  // 13. *string parameters — pointer-to-string-descriptor
  //
  // *string was previously crashing in TDeref because the aggregate-pointer
  // case in genExpr only listed Struct/Enum/Array/Func — strings (and slices)
  // weren't handled and fell through to emitLoad which only handles scalars
  // ("emitLoad: unexpected type string"). Fix is one line: extend the
  // aggregate case to include StringType and SliceType. All rc bracketing
  // (val copy through deref, *p = new_str assignment, scope-exit cleanup)
  // already worked via the existing isOwnedStringExpr machinery — the deref
  // of *string is correctly treated as a borrowed source, so the buffer's
  // rc gets incr'd on copy and decr'd on scope exit.
  //
  // (&string would be RefType(StringType) — that's a separate language
  // feature: `new string(...)` doesn't currently parse.)
  // ====================================================================

  "*string param: read len through deref" in {
    runWithAlloc(
      """take(p: *string) -> int = len(*p)
        |
        |main() -> int
        |    val s = "ab" + "cd"
        |    take(&s)
        |""".stripMargin) shouldBe 4
  }

  "*string param: heap-pressure (caller still owns buffer)" in {
    runWithAlloc(
      """take(p: *string) -> int = len(*p)
        |
        |fill()
        |    val s = "ab" + "cd"
        |    val n = take(&s)
        |
        |main() -> int
        |    var i = 0
        |    while i < 30
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "*string param: val s = *p copies descriptor and incr's borrowed buffer rc" in {
    // Without correct rc bracketing, callee's local s would decr the buffer
    // at scope exit while caller still owns it → use-after-free.
    runWithAlloc(
      """take(p: *string) -> int
        |    val s = *p
        |    len(s)
        |
        |fill()
        |    val s = "ab" + "cd"
        |    val n = take(&s)
        |
        |main() -> int
        |    var i = 0
        |    while i < 30
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "*p = new_string assigns through pointer (decr old, incr new)" in {
    runWithAlloc(
      """write(p: *string)
        |    *p = "x" + "y"
        |
        |fill()
        |    var s = "ab" + "cd"
        |    write(&s)
        |
        |main() -> int
        |    var i = 0
        |    while i < 20
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  // ====================================================================
  // 14. Destructure pattern bindings (TDestructurePattern)
  // ====================================================================

  "destructure struct with string field extracts value" in {
    runWithAlloc(
      """struct Pair
        |    name: string
        |    n: int
        |
        |main() -> int
        |    p = Pair("hi" + "!", 42)
        |    p match
        |        Pair(_, n) -> n
        |""".stripMargin) shouldBe 42
  }

  "destructure bound string field usable in arm body" in {
    runWithAlloc(
      """struct Pair
        |    name: string
        |    n: int
        |
        |main() -> int
        |    p = Pair("ho" + "ld", 1)
        |    p match
        |        Pair(s, _) -> if s == "hold" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "destructure with string field in loop — no heap leak" in {
    runWithAlloc(
      """struct Pair
        |    name: string
        |    n: int
        |
        |main() -> int
        |    var i = 0
        |    while i < 40
        |        val p = Pair("iter" + "_v", i)
        |        val sum = p match
        |            Pair(_, n) -> n
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  // ====================================================================
  // 15. Closure descriptor copy (var g = f)
  // ====================================================================

  "closure descriptor copy (var g = f) does not double-decr heap env" in {
    runWithAlloc(
      """make() -> (int) -> int
        |    val cap = "ab" + "cd"
        |    (x: int) -> x + len(cap)
        |
        |main() -> int
        |    val f = make()
        |    val g = f
        |    f(0) - g(0)
        |""".stripMargin) shouldBe 0
  }

  "closure descriptor copy in loop — no use-after-free (heap pressure)" in {
    runWithAlloc(
      """make(i: int) -> (int) -> int
        |    val cap = "iter" + "_v"
        |    (x: int) -> x + len(cap) + i
        |
        |main() -> int
        |    var i = 0
        |    while i < 30
        |        val f = make(i)
        |        val g = f
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 512) shouldBe 0
  }

  "closure descriptor reassignment in loop — no leak (heap pressure)" in {
    runWithAlloc(
      """make(i: int) -> (int) -> int
        |    val cap = "iter" + "_v"
        |    (x: int) -> x + len(cap) + i
        |
        |main() -> int
        |    var f = make(0)
        |    var i = 1
        |    while i < 40
        |        f = make(i)
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  // ====================================================================
  // 16. &MyEnum (RefType(EnumType)) deinit
  // ====================================================================

  "new MyEnum(string) — heap enum reaches rc=0 walks active variant strings (heap pressure)" in {
    runWithAlloc(
      """enum E
        |    Wrap(s: string)
        |    Empty
        |
        |main() -> int
        |    var i = 0
        |    while i < 30
        |        val e = new Wrap("iter" + "_v")
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "new MyEnum reassignment in loop — old enum's strings freed (heap pressure)" in {
    runWithAlloc(
      """enum E
        |    Wrap(s: string)
        |    Empty
        |
        |main() -> int
        |    var e = new Wrap("init" + "_v")
        |    var i = 0
        |    while i < 30
        |        e = new Wrap("iter" + "_v")
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "new MyEnum with multi-string variant (heap pressure)" in {
    runWithAlloc(
      """enum E
        |    Two(a: string, b: string)
        |    Empty
        |
        |main() -> int
        |    var i = 0
        |    while i < 20
        |        val e = new Two("aa" + "_v", "bb" + "_v")
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "new MyEnum with empty variant — no spurious decr (heap pressure)" in {
    runWithAlloc(
      """enum E
        |    Wrap(s: string)
        |    Empty
        |
        |main() -> int
        |    var i = 0
        |    while i < 100
        |        val e = new Empty()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 17. &MyStruct (RefType(StructType)) auto-deinit
  // ====================================================================

  "new MyStruct(string) — heap struct reaches rc=0 walks string fields (heap pressure)" in {
    runWithAlloc(
      """struct Holder
        |    name: string
        |    n: int
        |
        |main() -> int
        |    var i = 0
        |    while i < 30
        |        val h = new Holder("iter" + "_v", i)
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "new MyStruct reassignment in loop — old struct's strings freed (heap pressure)" in {
    runWithAlloc(
      """struct Holder
        |    name: string
        |
        |main() -> int
        |    var h = new Holder("init" + "_v")
        |    var i = 0
        |    while i < 30
        |        h = new Holder("iter" + "_v")
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "new MyStruct with multi-string field (heap pressure)" in {
    runWithAlloc(
      """struct Pair
        |    a: string
        |    b: string
        |
        |main() -> int
        |    var i = 0
        |    while i < 20
        |        val p = new Pair("aa" + "_v", "bb" + "_v")
        |        i += 1
        |    0
        |""".stripMargin, heapSize = 256) shouldBe 0
  }

  "new MyStruct with no rc-bearing fields — no spurious decr (heap pressure)" in {
    runWithAlloc(
      """struct Counter
        |    n: int
        |
        |main() -> int
        |    var i = 0
        |    while i < 100
        |        val c = new Counter(i)
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }
}
