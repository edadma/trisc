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
}
