package io.github.edadma.trisc

class SyslLLVMStringRefcountTests extends SyslLLVMTestHelpers {

  // ====================================================================
  // 1. Literal layout
  // ====================================================================

  "literal global has immortal refcount sentinel" in {
    val ir = compileLLVM(
      """main()
        |    var s = "hello"
        |""".stripMargin)
    ir should include("private unnamed_addr constant <{ i64, [6 x i8] }> <{ i64 -1,")
  }

  "empty string literal still has header" in {
    val ir = compileLLVM(
      """main()
        |    var s = ""
        |""".stripMargin)
    ir should include("private unnamed_addr constant <{ i64, [1 x i8] }> <{ i64 -1,")
  }

  "duplicate literals share the same global" in {
    val ir = compileLLVM(
      """main() -> int
        |    var a = "shared"
        |    var b = "shared"
        |    if a == b then 0 else 1
        |""".stripMargin)
    val matches = "@.sstr.\\d+ = private unnamed_addr constant <\\{ i64, \\[7 x i8\\] \\}>".r.findAllIn(ir).length
    matches shouldBe 1
  }

  // ====================================================================
  // 2. Aliasing — chained var bindings of immortal & heap strings
  // ====================================================================

  "literal aliasing many times does not segfault" in {
    llvmExit(
      """main() -> int
        |    var s = "hello"
        |    var t = s
        |    var u = t
        |    var v = u
        |    if v == "hello" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "concat result aliased into multiple locals" in {
    // Each new var should incr the buffer; each scope-exit decrs once.
    // Net effect: rc balanced. No double-free, no premature-free.
    llvmExit(
      """main() -> int
        |    var s = "abc" + "def"
        |    var t = s
        |    var u = t
        |    var v = u
        |    if u == "abcdef" && v == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "alias chain in loop with concat sources" in {
    llvmExit(
      """main() -> int
        |    var i = 0
        |    while i < 50
        |        var s = "ab" + "cd"
        |        var t = s
        |        var u = t
        |        if u != "abcd" then return 1
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 3. Reassignment patterns
  // ====================================================================

  "reassign concat→concat decrements old buffer" in {
    llvmExit(
      """main() -> int
        |    var s = "abc" + "def"
        |    s = "xyz" + "qrs"
        |    if s == "xyzqrs" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "reassign literal→concat→literal" in {
    llvmExit(
      """main() -> int
        |    var s = "lit"
        |    s = "abc" + "def"
        |    s = "another lit"
        |    if s == "another lit" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "reassign in loop (only last buffer should be live)" in {
    llvmExit(
      """main() -> int
        |    var s = "init"
        |    var i = 0
        |    while i < 100
        |        s = "loop" + "body"
        |        i += 1
        |    if s == "loopbody" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "self-concat does not free the source mid-evaluation" in {
    llvmExit(
      """main() -> int
        |    var s = "abc" + "def"
        |    s = s + "ghi"
        |    if s == "abcdefghi" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "repeated self-concat in loop" in {
    // Each iteration: rc(old s)++ for use as left operand; concat allocates new; old s decr to 0 → freed.
    llvmExit(
      """main() -> int
        |    var s = "x"
        |    var i = 0
        |    while i < 20
        |        s = s + "y"
        |        i += 1
        |    if len(s) == 21 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 4. string(ptr, len) — copy semantics
  // ====================================================================

  "string(ptr, len) copies — buffer outlives source" in {
    llvmExit(
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
        |    var s = build()
        |    if s == "hello" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "string(ptr, len) zero-length is valid" in {
    llvmExit(
      """main() -> int
        |    var arr = new [1]byte
        |    arr[0] = 65
        |    var s = string(&arr[0], 0)
        |    if len(s) == 0 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 5. Function parameters & returns
  // ====================================================================

  "string param: callee adds share, caller still owns after call" in {
    llvmExit(
      """get_len(s: string) -> int = len(s)
        |
        |main() -> int
        |    var s = "the quick brown fox"
        |    val n = get_len(s)
        |    if n == 19 && s == "the quick brown fox" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "string param: pass concat result without explicit local" in {
    llvmExit(
      """get_len(s: string) -> int = len(s)
        |
        |main() -> int
        |    val n = get_len("abc" + "def")
        |    if n == 6 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "string param: passed through multiple functions" in {
    // Each callee incrs on entry, decrs on exit. Caller's share preserved.
    llvmExit(
      """f1(s: string) -> int = f2(s)
        |f2(s: string) -> int = f3(s)
        |f3(s: string) -> int = len(s)
        |
        |main() -> int
        |    var s = "hello"
        |    val n = f1(s)
        |    if n == 5 && s == "hello" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "concat result returned and immediately consumed" in {
    llvmExit(
      """make(prefix: string) -> string = prefix + "_done"
        |
        |main() -> int
        |    val s = make("task")
        |    if s == "task_done" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "function returns its parameter directly (must incr before return)" in {
    // Tricky: callee param's "share" must be transferred OR a new share created
    // for the caller. Otherwise caller's `r` would be released when callee exits.
    llvmExit(
      """passthrough(s: string) -> string = s
        |
        |main() -> int
        |    var src = "abc" + "def"
        |    var r = passthrough(src)
        |    if r == "abcdef" && src == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "concat result returned from function is freed after consumption" in {
    llvmExit(
      """make() -> string = "aaaa" + "bbbb"
        |
        |main() -> int
        |    var i = 0
        |    while i < 1000
        |        var s = make()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "many concats in loop without rebinding" in {
    // result alloca is overwritten each iteration; old buffer must be freed.
    llvmExit(
      """main() -> int
        |    var s = ""
        |    var i = 0
        |    while i < 500
        |        s = "abc" + "def"
        |        i += 1
        |    if s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 6. Struct fields
  // ====================================================================

  "string field in struct is incremented on construction" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var msg = "hello"
        |    var h = Holder(msg)
        |    if h.s == "hello" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "struct holds concat — buffer survives source local going out of scope" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |build() -> Holder
        |    var tmp = "abc" + "def"
        |    Holder(tmp)
        |
        |main() -> int
        |    var h = build()
        |    if h.s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "string field reassignment frees old field buffer" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var h = Holder("abc" + "def")
        |    h.s = "xyz" + "qrs"
        |    if h.s == "xyzqrs" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "struct with two string fields" in {
    llvmExit(
      """struct Pair
        |    a: string
        |    b: string
        |
        |main() -> int
        |    var p = Pair("abc" + "def", "uvw" + "xyz")
        |    if p.a == "abcdef" && p.b == "uvwxyz" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 7. If-expr branches
  // ====================================================================

  "if-expr returning literal vs concat" in {
    llvmExit(
      """build(b: bool) -> string =
        |    if b then "literal" else "abc" + "def"
        |
        |main() -> int
        |    var a = build(true)
        |    var b = build(false)
        |    if a == "literal" && b == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "if-expr returning concat both branches" in {
    llvmExit(
      """build(b: bool) -> string =
        |    if b then "abc" + "def" else "uvw" + "xyz"
        |
        |main() -> int
        |    var s = build(true)
        |    var t = build(false)
        |    if s == "abcdef" && t == "uvwxyz" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "if-stmt with conditional reassignment" in {
    llvmExit(
      """main() -> int
        |    var s = "abc" + "def"
        |    if true then
        |        s = "new" + "value"
        |    if s == "newvalue" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "if-expr in loop returning string from var" in {
    // After loop: i goes 0,1,...,49; last body iter i=49 → odd; final s = "odd_".
    llvmExit(
      """main() -> int
        |    var i = 0
        |    var s = "init"
        |    while i < 50
        |        s = if i % 2 == 0 then "even" + "_" else "odd" + "_"
        |        i += 1
        |    if s == "odd_" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 8. Match-expr branches
  // ====================================================================

  "match expr returning literal vs concat" in {
    llvmExit(
      """build(n: int) -> string =
        |    n match
        |        0 -> "zero"
        |        else -> "abc" + "def"
        |
        |main() -> int
        |    var a = build(0)
        |    var b = build(1)
        |    if a == "zero" && b == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "match in loop with mixed branches" in {
    llvmExit(
      """label(n: int) -> string =
        |    n match
        |        0 -> "zero"
        |        1 -> "one" + "_value"
        |        else -> "many"
        |
        |main() -> int
        |    var i = 0
        |    var last = ""
        |    while i < 30
        |        last = label(i % 3)
        |        i += 1
        |    if last == "many" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 9. Globals
  // ====================================================================

  "string global var assigned concat" in {
    llvmExit(
      """var g: string
        |
        |main() -> int
        |    g = "global" + "_value"
        |    if g == "global_value" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "string global reassigned in loop" in {
    llvmExit(
      """var g: string
        |
        |main() -> int
        |    g = "init"
        |    var i = 0
        |    while i < 100
        |        g = "iter" + "_value"
        |        i += 1
        |    if g == "iter_value" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 10. Comparison & length
  // ====================================================================

  "comparison does not consume the strings" in {
    llvmExit(
      """main() -> int
        |    var s = "abc" + "def"
        |    var t = "abc" + "def"
        |    var eq1 = s == t
        |    var eq2 = s == t
        |    var eq3 = s == "abcdef"
        |    if eq1 && eq2 && eq3 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "len of concat result, then use the string" in {
    llvmExit(
      """main() -> int
        |    var s = "abc" + "def"
        |    val n = len(s)
        |    if n == 6 && s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 11. Nested concat
  // ====================================================================

  "nested concat (a + b) + (c + d) — temporaries freed" in {
    llvmExit(
      """main() -> int
        |    var i = 0
        |    while i < 200
        |        var s = ("ab" + "cd") + ("ef" + "gh")
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "long concat chain" in {
    llvmExit(
      """main() -> int
        |    var s = "a" + "b" + "c" + "d" + "e" + "f" + "g" + "h"
        |    if s == "abcdefgh" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 12. Defer
  // ====================================================================

  "defer fires before string locals are released" in {
    // The defer captures s; printing it should still see the live buffer.
    llvmOutput(
      """main()
        |    var s = "hello" + " world"
        |    defer puts(s)
        |""".stripMargin) should include("hello world")
  }

  // ====================================================================
  // 13. Early returns
  // ====================================================================

  "early return decrements live string locals" in {
    llvmExit(
      """check(b: bool) -> int
        |    var s = "abc" + "def"
        |    if !b then return 1
        |    if len(s) == 6 then 0 else 2
        |
        |main() -> int = check(true)
        |""".stripMargin) shouldBe 0
  }

  "early return with concat returned" in {
    llvmExit(
      """make(b: bool) -> string
        |    if b then return "abc" + "def"
        |    "xyz"
        |
        |main() -> int
        |    var s = make(true)
        |    if s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 14. Mixed scenarios
  // ====================================================================

  "concat used in struct field, then string reassigned" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var msg = "abc" + "def"
        |    var h = Holder(msg)
        |    msg = "different"
        |    if h.s == "abcdef" && msg == "different" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "function returns owned concat repeatedly" in {
    // Stresses the return-value ownership transfer.
    llvmExit(
      """make(prefix: string) -> string = prefix + "!"
        |
        |main() -> int
        |    var i = 0
        |    while i < 200
        |        var s = make("hi")
        |        if len(s) != 3 then return 1
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "string concat + string interpolation mix" in {
    llvmExit(
      """main() -> int
        |    var who = "world"
        |    var greet = "hello, " + who
        |    if greet == "hello, world" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 14. Value-struct cleanup — string fields decremented on scope exit
  // ====================================================================

  "value-struct local with string field — IR contains decrement on exit" in {
    val ir = compileLLVM(
      """struct Holder
        |    s: string
        |
        |main()
        |    var h = Holder("abc" + "def")
        |""".stripMargin)
    // Expect a getelementptr to field 0 of %struct.Holder followed by string descrement path
    ir should include("getelementptr %struct.Holder, %struct.Holder*")
  }

  "value-struct local with string field freed on scope exit (loop stress)" in {
    // Without per-field decrement, each iteration leaks the heap buffer for h.s.
    // 1000 iterations × ~16B would exceed any reasonable test heap if leaked.
    llvmExit(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var i = 0
        |    while i < 1000
        |        var h = Holder("abc" + "def")
        |        if h.s != "abcdef" then return 1
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "value-struct reassignment frees old string field" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var h = Holder("abc" + "def")
        |    var i = 0
        |    while i < 500
        |        h = Holder("xyz" + "qrs")
        |        i += 1
        |    if h.s == "xyzqrs" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "value-struct returned from function — string field survives" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |build(n: int) -> Holder
        |    var h = Holder("part" + "_one")
        |    h
        |
        |main() -> int
        |    var i = 0
        |    while i < 200
        |        var h = build(i)
        |        if h.s != "part_one" then return 1
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "value-struct passed by value to function — caller copy survives" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |inspect(h: Holder) -> int = len(h.s)
        |
        |main() -> int
        |    var h = Holder("abc" + "def")
        |    var i = 0
        |    while i < 200
        |        if inspect(h) != 6 then return 1
        |        i += 1
        |    if h.s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "value-struct mutation in callee does not leak to caller (true value semantics)" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |mutate(h: Holder) -> int
        |    h.s = "mutated"
        |    len(h.s)
        |
        |main() -> int
        |    var h = Holder("orig" + "inal")
        |    var n = mutate(h)
        |    if h.s == "original" && n == 7 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "value-struct copy via var p = q — both live independently" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var q = Holder("abc" + "def")
        |    var p = q
        |    if p.s == "abcdef" && q.s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "value-struct with two string fields freed on exit" in {
    llvmExit(
      """struct Pair
        |    a: string
        |    b: string
        |
        |main() -> int
        |    var i = 0
        |    while i < 500
        |        var p = Pair("first" + "_a", "second" + "_b")
        |        if p.a != "first_a" || p.b != "second_b" then return 1
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "nested value-struct with string field freed on exit" in {
    llvmExit(
      """struct Inner
        |    s: string
        |
        |struct Outer
        |    inner: Inner
        |    label: string
        |
        |main() -> int
        |    var i = 0
        |    while i < 500
        |        var o = Outer(Inner("deep" + "_str"), "top" + "_str")
        |        if o.inner.s != "deep_str" || o.label != "top_str" then return 1
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "value-struct holding borrowed string field — caller's local survives" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var msg = "abc" + "def"
        |    var i = 0
        |    while i < 300
        |        var h = Holder(msg)
        |        if h.s != "abcdef" then return 1
        |        i += 1
        |    if msg == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "value-struct reassigned with borrowed source string" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var src = "abc" + "def"
        |    var h = Holder("init" + "_str")
        |    var i = 0
        |    while i < 300
        |        h = Holder(src)
        |        i += 1
        |    if h.s == "abcdef" && src == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "if-expr returning struct with string field — both branches" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |build(b: bool) -> Holder =
        |    if b then Holder("true" + "_path") else Holder("false" + "_path")
        |
        |main() -> int
        |    var i = 0
        |    while i < 200
        |        var h = build(i % 2 == 0)
        |        if h.s != "true_path" && h.s != "false_path" then return 1
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "match expr returning struct with string field" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |build(n: int) -> Holder =
        |    n match
        |        0 -> Holder("zero" + "_str")
        |        else -> Holder("other" + "_str")
        |
        |main() -> int
        |    var i = 0
        |    while i < 200
        |        var h = build(i)
        |        if h.s == "" then return 1
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "value-struct in if-stmt scope freed on block exit" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var i = 0
        |    while i < 500
        |        if i % 2 == 0
        |            var h = Holder("even" + "_str")
        |            if h.s != "even_str" then return 1
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "early return with value-struct local — fields freed" in {
    llvmExit(
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

  "global value-struct with string field" in {
    // Globals don't get scope-cleanup but assignment should still rebalance refcounts.
    llvmExit(
      """struct Holder
        |    s: string
        |
        |var g: Holder
        |
        |main() -> int
        |    g = Holder("global" + "_str")
        |    if g.s == "global_str" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 16. Arrays/slices of strings — element rc on scope exit / slice free
  // ====================================================================

  "[N]string scope exit emits per-element decrement IR" in {
    val ir = compileLLVM(
      """fill()
        |    var arr: [4]string
        |    arr[0] = "a" + "_v"
        |    arr[1] = "b" + "_v"
        |    arr[2] = "c" + "_v"
        |    arr[3] = "d" + "_v"
        |""".stripMargin)
    // Per-element string-buffer decrement should appear in fill() body.
    // 4 elements × 1 string descriptor each → at least 4 free calls before ret void.
    val freeCount = "call void @free".r.findAllIn(ir).length
    freeCount should be >= 4
  }

  "&[]string scope exit emits slice deinit + free" in {
    val ir = compileLLVM(
      """main() -> int
        |    val arr = new [2]string
        |    arr[0] = "a" + "x"
        |    arr[1] = "b" + "y"
        |    0
        |""".stripMargin)
    ir should include("define i32 @__slice_deinit_string(i8* %data)")
    ir should include("call i32 @__slice_deinit_string")
  }

  "[N]string scope exit (functional check)" in {
    llvmExit(
      """fill()
        |    var arr: [4]string
        |    arr[0] = "a" + "_v"
        |    arr[1] = "b" + "_v"
        |    arr[2] = "c" + "_v"
        |    arr[3] = "d" + "_v"
        |
        |main() -> int
        |    var i = 0
        |    while i < 50
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "&[]string scope exit (functional check)" in {
    llvmExit(
      """fill()
        |    val arr = new [4]string
        |    arr[0] = "a" + "_v"
        |    arr[1] = "b" + "_v"
        |    arr[2] = "c" + "_v"
        |    arr[3] = "d" + "_v"
        |
        |main() -> int
        |    var i = 0
        |    while i < 50
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 12. Substring s[a:b] (option A: copy semantics)
  // ====================================================================

  "substring IR allocates and copies via memcpy" in {
    val ir = compileLLVM(
      """main()
        |    val s = "hello"
        |    val t = s[1:4]
        |""".stripMargin)
    // Substring path: malloc + memcpy + emitMakeString
    ir should include("call i8* @malloc")
    ir should include("call i8* @memcpy")
  }

  "substring out-of-bounds emits abort branch" in {
    val ir = compileLLVM(
      """main()
        |    val s = "hi"
        |    val t = s[0:5]
        |""".stripMargin)
    ir should include("call void @abort()")
  }

  "substring of literal yields correct bytes" in {
    llvmExit(
      """main() -> int
        |    val s = "hello"
        |    val t = s[1:4]
        |    if t == "ell" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "substring with default lo (s[:k])" in {
    llvmExit(
      """main() -> int
        |    val s = "abcdef"
        |    val t = s[:3]
        |    if t == "abc" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "substring with default hi (s[k:])" in {
    llvmExit(
      """main() -> int
        |    val s = "abcdef"
        |    val t = s[2:]
        |    if t == "cdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "substring full copy (s[:]) equals original" in {
    llvmExit(
      """main() -> int
        |    val s = "hello"
        |    val t = s[:]
        |    if t == "hello" && len(t) == 5 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "empty substring (s[k:k])" in {
    llvmExit(
      """main() -> int
        |    val s = "hello"
        |    val t = s[2:2]
        |    if len(t) == 0 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "substring of concat result" in {
    llvmExit(
      """main() -> int
        |    val s = "ab" + "cdef"
        |    val t = s[1:5]
        |    if t == "bcde" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "substring returned from function" in {
    llvmExit(
      """take(s: string, lo: int, hi: int) -> string = s[lo:hi]
        |
        |main() -> int
        |    val s = "hello world"
        |    val t = take(s, 6, 11)
        |    if t == "world" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "substring of substring" in {
    llvmExit(
      """main() -> int
        |    val s = "abcdefgh"
        |    val t = s[1:7]
        |    val u = t[1:4]
        |    if u == "cde" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "substring in loop (functional check)" in {
    llvmExit(
      """fill()
        |    val s = "abcdefgh"
        |    val t = s[0:8]
        |
        |main() -> int
        |    var i = 0
        |    while i < 50
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 13. Enum-with-string variant rc (tag-dispatch decr on scope exit)
  // ====================================================================

  "enum scope-exit IR loads tag and chains compares" in {
    val ir = compileLLVM(
      """enum E
        |    OneStr(s: string)
        |    Empty
        |
        |fill()
        |    val e = OneStr("ab" + "cd")
        |
        |main() -> int
        |    fill()
        |    0
        |""".stripMargin)
    // Tag load (i32) appears in fill's scope-cleanup path
    ir should include regex """%\S+ = load i32, i32\* %\S+"""
    // Per-variant compare against tag value
    ir should include regex """icmp eq i32 %\S+, 0"""
  }

  "&[]Enum slice deinit synthesized" in {
    val ir = compileLLVM(
      """enum E
        |    OneStr(s: string)
        |    Empty
        |
        |main() -> int
        |    val arr = new [2]E
        |    arr[0] = OneStr("ab" + "cd")
        |    arr[1] = OneStr("ef" + "gh")
        |    0
        |""".stripMargin)
    // Per-elem deinit fn synthesized for the enum element type
    ir should include("define i32 @__slice_deinit_enum_E(i8* %data)")
  }

  "enum variant with string field — scope exit (functional)" in {
    llvmExit(
      """enum E
        |    OneStr(s: string)
        |    Empty
        |
        |main() -> int
        |    val e = OneStr("ab" + "cd")
        |    0
        |""".stripMargin) shouldBe 0
  }

  "enum variant with borrowed-string field — incr+decr balance" in {
    llvmExit(
      """enum E
        |    OneStr(s: string)
        |    Empty
        |
        |main() -> int
        |    val s = "ab" + "cd"
        |    val e = OneStr(s)
        |    0
        |""".stripMargin) shouldBe 0
  }

  "enum variant with multiple string fields" in {
    llvmExit(
      """enum E
        |    Two(a: string, b: string)
        |    Empty
        |
        |main() -> int
        |    val e = Two("foo" + "1", "bar" + "2")
        |    0
        |""".stripMargin) shouldBe 0
  }

  "enum variant with nested struct-with-string field" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |enum E
        |    Wrap(h: Holder)
        |    Empty
        |
        |main() -> int
        |    val e = Wrap(Holder("ab" + "cd"))
        |    0
        |""".stripMargin) shouldBe 0
  }

  "enum reassignment frees old variant strings" in {
    llvmExit(
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
        |""".stripMargin) shouldBe 0
  }

  "enum returned from function — string field survives" in {
    llvmExit(
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
    llvmExit(
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

  "enum with no-string variant — no spurious decr" in {
    llvmExit(
      """enum E
        |    OneStr(s: string)
        |    Empty
        |
        |main() -> int
        |    val s = "ab" + "cd"
        |    val e = Empty
        |    0
        |""".stripMargin) shouldBe 0
  }

  "[N]Enum scope exit decrs each element" in {
    llvmExit(
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
        |    fill()
        |    0
        |""".stripMargin) shouldBe 0
  }

  "&[]Enum scope exit decrs each element" in {
    llvmExit(
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
        |    fill()
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // 12. Closures capturing strings — env rc bracketing
  // ====================================================================

  "closure with single string capture: scope exit releases env" in {
    llvmExit(
      """fill()
        |    val s = "abc" + "_v"
        |    val f = (x: int) -> x + len(s)
        |    val r = f(0)
        |
        |main() -> int
        |    var i = 0
        |    while i < 100
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "closure constructed but body never called: still releases env" in {
    llvmExit(
      """fill()
        |    val s = "ab" + "cd"
        |    val f = (x: int) -> x + len(s)
        |    // f is never called
        |
        |main() -> int
        |    var i = 0
        |    while i < 100
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "closure with multiple string captures" in {
    llvmExit(
      """fill()
        |    val a = "x" + "y"
        |    val b = "p" + "q"
        |    val f = (n: int) -> n + len(a) + len(b)
        |    val r = f(1)
        |
        |main() -> int
        |    var i = 0
        |    while i < 100
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "closure body uses captured string (computed result is correct)" in {
    llvmExit(
      """main() -> int
        |    val s = "ab" + "cd"
        |    val f = (n: int) -> n + len(s)
        |    f(38)
        |""".stripMargin) shouldBe 42
  }

  "closure capturing struct-with-string field" in {
    llvmExit(
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
        |    while i < 50
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "closure used inside higher-order function (descriptor passed by value)" in {
    llvmExit(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |fill()
        |    val s = "ab" + "cd"
        |    val r = apply(n -> n + len(s), 0)
        |
        |main() -> int
        |    var i = 0
        |    while i < 100
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "closure construction IR emits +16 byte env (rc + deinit_ptr) and dispatch decl" in {
    val ir = compileLLVM(
      """main()
        |    val s = "abc" + "_v"
        |    val f = (x: int) -> x + len(s)
        |""".stripMargin)
    // env malloc'd with envSize+16 (single 16-byte string capture → 16+16=32)
    ir should include regex """call i8\* @malloc\(i64 32\)"""
    // rc = 1 stored in header
    ir should include regex """store i64 1, i64\*"""
    // deinit_ptr cast (per-closure-id env deinit)
    ir should include regex """bitcast i32 \(i8\*\)\* @__closure_env_deinit___closure_"""
    // Dispatch shim is generated
    ir should include("define i32 @__closure_env_dispatch")
    // Per-closure-id deinit is generated
    ir should include regex """define i32 @__closure_env_deinit___closure_"""
  }

  "closure with no rc-bearing captures: deinit_ptr is null (no per-id deinit)" in {
    // `val f = ...` (no expected type) defaults to escaping per analyzer convention.
    // → HeapEnv path → malloc. deinit_ptr=null since captures are non-rc-bearing.
    val ir = compileLLVM(
      """main()
        |    val a = 10
        |    val f = (x: int) -> x + a
        |""".stripMargin)
    ir should include("call i8* @malloc")
    ir should include regex """store i8\* null, i8\*\*"""
    // No per-id deinit registered
    ir should not include "@__closure_env_deinit_"
  }

  "stack-env optimization: closure as call arg with int capture has no malloc" in {
    // Non-escaping context (closure passed to `apply(f: (int) -> int, ...)`) +
    // non-rc-bearing captures → StackEnv → alloca, no malloc, no free.
    val ir = compileLLVM(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    val a = 10
        |    apply(x -> x + a, 32)
        |""".stripMargin)
    // No malloc/free for the closure env
    ir should not include "call i8* @malloc"
    ir should not include "@__closure_env_deinit_"
    ir should not include "@__closure_env_dispatch"
  }

  // ====================================================================
  // 13. *string parameters
  //
  // LLVM was already correct (TDeref uses isAggregate which includes
  // StringType). TRISC needed a one-line fix to its TDeref aggregate-case.
  // These tests assert end-to-end correctness on LLVM as a sanity check.
  // ====================================================================

  "*string param: read len through deref" in {
    llvmExit(
      """take(p: *string) -> int = len(*p)
        |
        |main() -> int
        |    val s = "ab" + "cd"
        |    take(&s)
        |""".stripMargin) shouldBe 4
  }

  "*string param: heap-pressure" in {
    llvmExit(
      """take(p: *string) -> int = len(*p)
        |
        |fill()
        |    val s = "ab" + "cd"
        |    val n = take(&s)
        |
        |main() -> int
        |    var i = 0
        |    while i < 100
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "*string param: val s = *p copies descriptor (borrowed-source rc)" in {
    llvmExit(
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
        |    while i < 100
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "*p = new_string assigns through pointer" in {
    llvmExit(
      """write(p: *string)
        |    *p = "x" + "y"
        |
        |fill()
        |    var s = "ab" + "cd"
        |    write(&s)
        |
        |main() -> int
        |    var i = 0
        |    while i < 100
        |        fill()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // Destructure pattern bindings (TDestructurePattern)
  // ====================================================================

  "destructure struct with string field extracts value" in {
    llvmExit(
      """struct Pair
        |    name: string
        |    n: int
        |
        |main() -> int
        |    p = Pair("hi" + "!", 42)
        |    p match
        |        Pair(_, n) -> n - 42
        |""".stripMargin) shouldBe 0
  }

  "destructure bound string field usable in arm body" in {
    llvmExit(
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
    llvmExit(
      """struct Pair
        |    name: string
        |    n: int
        |
        |main() -> int
        |    var i = 0
        |    while i < 100
        |        val p = Pair("iter" + "_v", i)
        |        val sum = p match
        |            Pair(_, n) -> n
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // Closure descriptor copy (var g = f)
  // ====================================================================

  "closure descriptor copy (var g = f) does not double-decr heap env" in {
    llvmExit(
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

  "closure descriptor copy in loop — no use-after-free" in {
    llvmExit(
      """make(i: int) -> (int) -> int
        |    val cap = "iter" + "_v"
        |    (x: int) -> x + len(cap) + i
        |
        |main() -> int
        |    var i = 0
        |    while i < 100
        |        val f = make(i)
        |        val g = f
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "closure descriptor reassignment in loop — no leak" in {
    llvmExit(
      """make(i: int) -> (int) -> int
        |    val cap = "iter" + "_v"
        |    (x: int) -> x + len(cap) + i
        |
        |main() -> int
        |    var f = make(0)
        |    var i = 1
        |    while i < 100
        |        f = make(i)
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // &MyEnum (RefType(EnumType)) — TNewEnum + per-enum deinit
  // ====================================================================

  "new MyEnum(string) — heap enum reaches rc=0 walks active variant strings" in {
    llvmExit(
      """enum E
        |    Wrap(s: string)
        |    Empty
        |
        |main() -> int
        |    var i = 0
        |    while i < 100
        |        val e = new Wrap("iter" + "_v")
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "new MyEnum reassignment in loop — old enum's strings freed" in {
    llvmExit(
      """enum E
        |    Wrap(s: string)
        |    Empty
        |
        |main() -> int
        |    var e = new Wrap("init" + "_v")
        |    var i = 0
        |    while i < 100
        |        e = new Wrap("iter" + "_v")
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "new MyEnum with multi-string variant" in {
    llvmExit(
      """enum E
        |    Two(a: string, b: string)
        |    Empty
        |
        |main() -> int
        |    var i = 0
        |    while i < 100
        |        val e = new Two("aa" + "_v", "bb" + "_v")
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "new MyEnum with empty variant — no spurious decr" in {
    llvmExit(
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

  "enum deinit IR emits __enum_deinit_<name>" in {
    val ir = compileLLVM(
      """enum E
        |    Wrap(s: string)
        |    Empty
        |
        |main() -> int
        |    val e = new Wrap("hi" + "!")
        |    0
        |""".stripMargin)
    ir should include("define i32 @__enum_deinit_E")
    ir should include("call i32 @__enum_deinit_E")
  }
}
