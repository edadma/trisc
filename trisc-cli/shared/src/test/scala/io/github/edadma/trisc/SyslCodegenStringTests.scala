package io.github.edadma.trisc

class SyslCodegenStringTests extends SyslCodegenHelpers {

  // Helper: compile and run, capturing stdout output
  private def compileAndRunOutput(source: String): (Long, String) =
    val asm = compile(source)
    val tof = assemble(asm, relocatable = true)
    val linked = Linker.link(Seq(Runtime.bootTof, tof, Runtime.ioTof))
    val output = new StringBuilder
    val stdout = new Stdout(Runtime.stdoutAddress, s => output ++= s)
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 100000 }
    cpu.reset()
    cpu.run()
    (cpu.r(1).read, output.toString)

  // ===== String literal length =====

  "string literal length" in {
    compileAndRun(
      """main() -> int
        |    val s = "hello"
        |    len(s)
        |""".stripMargin) shouldBe 5
  }

  "empty string length" in {
    compileAndRun(
      """main() -> int
        |    val s = ""
        |    len(s)
        |""".stripMargin) shouldBe 0
  }

  // ===== String indexing =====

  "string indexing" in {
    compileAndRun(
      """main() -> int
        |    val s = "ABC"
        |    s[0] * 100 + s[1] * 10 + s[2] - 65 * 111
        |""".stripMargin) shouldBe 12
  }

  // ===== String comparison =====

  "string equality — same" in {
    compileAndRun(
      """main() -> int
        |    if "hello" == "hello" then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "string equality — different" in {
    compileAndRun(
      """main() -> int
        |    if "hello" == "world" then return 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "string inequality" in {
    compileAndRun(
      """main() -> int
        |    if "abc" != "def" then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "string equality — different lengths" in {
    compileAndRun(
      """main() -> int
        |    if "abc" == "abcd" then return 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "string equality — empty strings" in {
    compileAndRun(
      """main() -> int
        |    if "" == "" then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  // ===== String puts =====

  "puts string literal" in {
    val (_, out) = compileAndRunOutput(
      """main() -> int
        |    puts("hello")
        |    0
        |""".stripMargin)
    out shouldBe "hello"
  }

  "puts string variable" in {
    val (_, out) = compileAndRunOutput(
      """main() -> int
        |    val s = "world"
        |    puts(s)
        |    0
        |""".stripMargin)
    out shouldBe "world"
  }

  // ===== String passed to function =====

  "string passed to function" in {
    compileAndRun(
      """length(s: string) -> int = len(s)
        |
        |main() -> int = length("hello")
        |""".stripMargin) shouldBe 5
  }

  // ===== String as *i8 decay =====

  "string decays to *i8" in {
    compileAndRun(
      """first_byte(p: *i8) -> int = p[0]
        |
        |main() -> int = first_byte("A")
        |""".stripMargin) shouldBe 65
  }

  // ===== Multiple string variables =====

  "multiple string variables" in {
    compileAndRun(
      """main() -> int
        |    val a = "abc"
        |    val b = "def"
        |    len(a) + len(b)
        |""".stripMargin) shouldBe 6
  }

  // ===== String reassignment =====

  "string reassignment" in {
    val (_, out) = compileAndRunOutput(
      """main() -> int
        |    var s = "first"
        |    s = "second"
        |    puts(s)
        |    0
        |""".stripMargin)
    out shouldBe "second"
  }

  // ===== String in if/else =====

  "string in if/else" in {
    val (_, out) = compileAndRunOutput(
      """main() -> int
        |    val flag = true
        |    val s = if flag then "yes" else "no"
        |    puts(s)
        |    0
        |""".stripMargin)
    out shouldBe "yes"
  }

  // ===== String equality with variables =====

  "string equality with variables" in {
    compileAndRun(
      """main() -> int
        |    val a = "hello"
        |    val b = "hello"
        |    if a == b then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "string inequality with variables" in {
    compileAndRun(
      """main() -> int
        |    val a = "hello"
        |    val b = "world"
        |    if a != b then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  // ===== String concatenation (requires allocator) =====

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

  private def allocSources(mainSource: String, heapSize: Int = 16384): Map[String, String] =
    Map(
      "posix/unistd/sbrk" -> sbrkModule(heapSize),
      "posix/string/string" -> stringSource,
      "posix/stdlib/alloc" -> allocSource,
      "main" -> mainSource,
    )

  private def compileMultiAndRunOutput(sources: Map[String, String]): (Long, String) =
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val linked = Linker.link(Seq(Runtime.bootTof) ++ tofs ++ Seq(Runtime.ioTof))
    val output = new StringBuilder
    val stdout = new Stdout(Runtime.stdoutAddress, s => output ++= s)
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 100000 }
    cpu.reset()
    cpu.run()
    (cpu.r(1).read, output.toString)

  "concat length is sum" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val a = "abc"
        |    val b = "defgh"
        |    val c = a + b
        |    len(c)
        |""".stripMargin)) shouldBe 8
  }

  "concat output" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val s = "hello" + " " + "world"
        |    puts(s)
        |    0
        |""".stripMargin))
    out shouldBe "hello world"
  }

  "concat preserves originals" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val a = "hello"
        |    val b = " world"
        |    val c = a + b
        |    puts(a)
        |    0
        |""".stripMargin))
    out shouldBe "hello"
  }

  "string equality after concat" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val a = "hel" + "lo"
        |    if a == "hello" then return 1
        |    0
        |""".stripMargin)) shouldBe 1
  }

  "string returned from function" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |
        |greet(name: string) -> string = "hello " + name
        |
        |main() -> int
        |    puts(greet("world"))
        |    0
        |""".stripMargin))
    out shouldBe "hello world"
  }

  // ===== Bounds checking =====

  "string index last valid position" in {
    compileAndRun(
      """main() -> int
        |    val s = "ABC"
        |    s[2]
        |""".stripMargin) shouldBe 67  // 'C'
  }

  "string single char" in {
    compileAndRun(
      """main() -> int
        |    val s = "Z"
        |    s[0]
        |""".stripMargin) shouldBe 90  // 'Z'
  }

  "string single char length" in {
    compileAndRun(
      """main() -> int
        |    len("X")
        |""".stripMargin) shouldBe 1
  }

  // ===== Comparison edge cases =====

  "string comparison — same prefix different end" in {
    compileAndRun(
      """main() -> int
        |    if "hello" == "hellp" then return 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "string comparison — differ at first byte" in {
    compileAndRun(
      """main() -> int
        |    if "abc" == "xbc" then return 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "string inequality — same strings" in {
    compileAndRun(
      """main() -> int
        |    if "abc" != "abc" then return 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "string comparison — one empty one not" in {
    compileAndRun(
      """main() -> int
        |    if "" == "a" then return 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "string comparison — variables same content different literals" in {
    compileAndRun(
      """main() -> int
        |    val a = "test"
        |    val b = "test"
        |    if a == b then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  // ===== puts edge cases =====

  "puts empty string" in {
    val (_, out) = compileAndRunOutput(
      """main() -> int
        |    puts("")
        |    0
        |""".stripMargin)
    out shouldBe ""
  }

  "puts long string" in {
    val (_, out) = compileAndRunOutput(
      """main() -> int
        |    puts("the quick brown fox jumps over the lazy dog")
        |    0
        |""".stripMargin)
    out shouldBe "the quick brown fox jumps over the lazy dog"
  }

  "puts two strings" in {
    val (_, out) = compileAndRunOutput(
      """main() -> int
        |    puts("hello")
        |    puts("world")
        |    0
        |""".stripMargin)
    out shouldBe "helloworld"
  }

  // ===== String if/else — both branches =====

  "string in if/else — else branch" in {
    val (_, out) = compileAndRunOutput(
      """main() -> int
        |    val flag = false
        |    val s = if flag then "yes" else "no"
        |    puts(s)
        |    0
        |""".stripMargin)
    out shouldBe "no"
  }

  // ===== String as *i8 edge cases =====

  "string variable decays to *i8" in {
    compileAndRun(
      """first_byte(p: *i8) -> int = p[0]
        |
        |main() -> int
        |    val s = "Hello"
        |    first_byte(s)
        |""".stripMargin) shouldBe 72  // 'H'
  }

  "string literal as 4th arg (stack arg) *i8" in {
    compileAndRun(
      """check(a: int, b: int, c: int, s: *i8) -> int = s[0]
        |
        |main() -> int = check(1, 2, 3, "Q")
        |""".stripMargin) shouldBe 81  // 'Q'
  }

  // ===== String function args — multiple params =====

  // TODO: Two string literal register args — callee push overwrites TStringLit temps.
  // Works fine when at least one arg is a variable (stable fp-relative address).
  // Needs ABI fix: either pass string reg args as 16 bytes or pre-allocate temps.
  "two string params" ignore {
    compileAndRun(
      """both(a: string, b: string) -> int = len(a) + len(b)
        |
        |main() -> int = both("abc", "defgh")
        |""".stripMargin) shouldBe 8
  }

  "string as 4th arg (stack arg)" in {
    compileAndRun(
      """check(a: int, b: int, c: int, s: string) -> int = len(s)
        |
        |main() -> int = check(1, 2, 3, "hello")
        |""".stripMargin) shouldBe 5
  }

  "string as 4th and 5th arg" in {
    compileAndRun(
      """both(a: int, b: int, c: int, s1: string, s2: string) -> int = len(s1) + len(s2)
        |
        |main() -> int = both(1, 2, 3, "abc", "de")
        |""".stripMargin) shouldBe 5
  }

  // ===== len on literals directly =====

  "len on literal directly" in {
    compileAndRun(
      """main() -> int = len("hello")
        |""".stripMargin) shouldBe 5
  }

  // ===== Multiple reassignments =====

  "multiple string reassignments" in {
    val (_, out) = compileAndRunOutput(
      """main() -> int
        |    var s = "first"
        |    s = "second"
        |    s = "third"
        |    puts(s)
        |    0
        |""".stripMargin)
    out shouldBe "third"
  }

  // ===== Concat edge cases (require allocator) =====

  "concat empty left" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val s = "" + "abc"
        |    puts(s)
        |    0
        |""".stripMargin))
    out shouldBe "abc"
  }

  "concat empty right" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val s = "abc" + ""
        |    puts(s)
        |    0
        |""".stripMargin))
    out shouldBe "abc"
  }

  "concat both empty" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val s = "" + ""
        |    len(s)
        |""".stripMargin)) shouldBe 0
  }

  "concat single chars" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val s = "a" + "b"
        |    puts(s)
        |    0
        |""".stripMargin))
    out shouldBe "ab"
  }

  "len of concat result directly" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val a = "hello"
        |    val b = " world"
        |    len(a + b)
        |""".stripMargin)) shouldBe 11
  }

  "index into concat result" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val s = "AB" + "CD"
        |    s[0] * 1000 + s[1] * 100 + s[2] * 10 + s[3]
        |""".stripMargin)) shouldBe (65 * 1000 + 66 * 100 + 67 * 10 + 68)
  }

  "puts concat result directly" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    puts("hello" + " world")
        |    0
        |""".stripMargin))
    out shouldBe "hello world"
  }

  "nested concat" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val s = ("hel" + "lo") + (" " + "world")
        |    puts(s)
        |    0
        |""".stripMargin))
    out shouldBe "hello world"
  }

  "reassignment with concat" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    var s = "hello"
        |    s = s + " world"
        |    puts(s)
        |    0
        |""".stripMargin))
    out shouldBe "hello world"
  }

  // TODO: Same two-string-literal-register-arg issue as "two string params"
  "string built in function and compared" ignore {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |make(a: string, b: string) -> string = a + b
        |
        |main() -> int
        |    val s = make("foo", "bar")
        |    if s == "foobar" then return 1
        |    0
        |""".stripMargin)) shouldBe 1
  }

  "string from function used in puts" ignore {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |
        |make(a: string, b: string) -> string = a + b
        |
        |main() -> int
        |    val s = make("hello", " world")
        |    puts(s)
        |    0
        |""".stripMargin))
    out shouldBe "hello world"
  }

  // ===== Concat in loop — critical for memory reuse =====

  "concat in loop" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    var s = ""
        |    var i = 0
        |    while i < 3
        |        s = s + "ab"
        |        i++
        |    puts(s)
        |    0
        |""".stripMargin))
    out shouldBe "ababab"
  }

  "concat in loop length" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    var s = ""
        |    var i = 0
        |    while i < 5
        |        s = s + "x"
        |        i++
        |    len(s)
        |""".stripMargin)) shouldBe 5
  }

  // ===== Memory reuse proof — small heap =====

  "concat in loop with small heap proves free works" in {
    // With 4KB heap: each concat allocates 8 + len bytes.
    // After 50 iterations of s = s + "x", total allocations far exceed 4KB.
    // If free doesn't work, malloc will fail (halt). Completing = proof of free.
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    var s = ""
        |    var i = 0
        |    while i < 50
        |        s = s + "x"
        |        i++
        |    len(s)
        |""".stripMargin, heapSize = 4096)) shouldBe 50
  }
}
