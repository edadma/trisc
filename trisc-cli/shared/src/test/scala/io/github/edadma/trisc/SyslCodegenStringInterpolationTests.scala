package io.github.edadma.trisc

class SyslCodegenStringInterpolationTests extends SyslCodegenHelpers {

  // Allocator infrastructure (same pattern as SyslCodegenStringTests)
  private val allocSource = scala.io.Source.fromFile("posix/stdlib/alloc.sysl").mkString
  private val stringSource = scala.io.Source.fromFile("posix/string/string.sysl").mkString
  private val ctypeSource = scala.io.Source.fromFile("posix/ctype/ctype.sysl").mkString

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

  private def allocSources(mainSource: String): Map[String, String] =
    Map(
      "posix/unistd/sbrk" -> sbrkModule(),
      "posix/string/string" -> stringSource,
      "posix/ctype/ctype" -> ctypeSource,
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
    val cpu = new CPU(mem) { limit = 200000 }
    cpu.reset()
    cpu.run()
    (cpu.r(1).read, output.toString)

  // ===== str() builtin =====

  "str of positive int" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    puts(str(42))\n    0\n"))
    out shouldBe "42"
  }

  "str of zero" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    puts(str(0))\n    0\n"))
    out shouldBe "0"
  }

  "str of negative" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    puts(str(-123))\n    0\n"))
    out shouldBe "-123"
  }

  "str of one" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    puts(str(1))\n    0\n"))
    out shouldBe "1"
  }

  "str of large number" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    puts(str(123456))\n    0\n"))
    out shouldBe "123456"
  }

  // ===== String length from str() =====

  "str result length" in {
    compileMultiAndRun(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    s: string = str(42)\n    len(s)\n")) shouldBe 2
  }

  "str negative result length" in {
    compileMultiAndRun(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    s: string = str(-5)\n    len(s)\n")) shouldBe 2
  }

  // ===== String interpolation with $name =====

  "interpolate integer" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    x = 42\n    puts(s\"$x\")\n    0\n"))
    out shouldBe "42"
  }

  "interpolate in middle" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    x = 5\n    puts(s\"val=$x!\")\n    0\n"))
    out shouldBe "val=5!"
  }

  "multiple interpolations" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    a = 10\n    b = 20\n    puts(s\"$a+$b\")\n    0\n"))
    out shouldBe "10+20"
  }

  "interpolate at start" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    x = 7\n    puts(s\"$x ok\")\n    0\n"))
    out shouldBe "7 ok"
  }

  "interpolate at end" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    x = 99\n    puts(s\"v=$x\")\n    0\n"))
    out shouldBe "v=99"
  }

  // ===== ${expr} interpolation =====

  "interpolate expression" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    x = 3\n    puts(s\"${x + 1}\")\n    0\n"))
    out shouldBe "4"
  }

  "interpolate negative" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    x = -5\n    puts(s\"x=$x\")\n    0\n"))
    out shouldBe "x=-5"
  }

  "interpolate zero" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    x = 0\n    puts(s\"$x\")\n    0\n"))
    out shouldBe "0"
  }

  "interpolated string length" in {
    compileMultiAndRun(allocSources(
      "import posix.stdlib.*\nmain() -> int\n    x = 42\n    s: string = s\"v=$x\"\n    len(s)\n")) shouldBe 4
  }

  // ===== Float interpolation (6-digit fixed precision) =====

  "str of positive float" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |main() -> int
        |    val x = 3.14
        |    puts(str(x))
        |    0
        |""".stripMargin))
    out shouldBe "3.140000"
  }

  "str of negative float" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |main() -> int
        |    val x = 0.0 - 0.5
        |    puts(str(x))
        |    0
        |""".stripMargin))
    out shouldBe "-0.500000"
  }

  "str of zero float" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |main() -> int
        |    val x = 0.0
        |    puts(str(x))
        |    0
        |""".stripMargin))
    out shouldBe "0.000000"
  }

  "str of integer-valued float" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |main() -> int
        |    val x = 42.0
        |    puts(str(x))
        |    0
        |""".stripMargin))
    out shouldBe "42.000000"
  }

  "interpolate float variable" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |main() -> int
        |    val x = 1.5
        |    puts(s"x = $x")
        |    0
        |""".stripMargin))
    out shouldBe "x = 1.500000"
  }

  "interpolate float expression" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |main() -> int
        |    val a = 2.5
        |    val b = 1.5
        |    puts(s"sum=${a + b}")
        |    0
        |""".stripMargin))
    out shouldBe "sum=4.000000"
  }

  "interpolate int and float together" in {
    val (_, out) = compileMultiAndRunOutput(allocSources(
      """import posix.stdlib.*
        |main() -> int
        |    val i = 10
        |    val f = 0.25
        |    puts(s"i=$i f=$f")
        |    0
        |""".stripMargin))
    out shouldBe "i=10 f=0.250000"
  }
}
