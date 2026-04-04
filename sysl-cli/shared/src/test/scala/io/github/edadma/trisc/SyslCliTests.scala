package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslCliTests extends AnyFreeSpec with Matchers {

  // --- Argument parsing ---

  "parse bare file defaults to compile" in {
    val Some(config) = SyslCli.parse(Seq("foo.sysl")): @unchecked
    config.command shouldBe a[CompileCommand]
    config.command.asInstanceOf[CompileCommand].inputs shouldBe Seq("foo.sysl")
  }

  "parse multiple bare files" in {
    val Some(config) = SyslCli.parse(Seq("a.sysl", "b.sysl")): @unchecked
    config.command.asInstanceOf[CompileCommand].inputs shouldBe Seq("a.sysl", "b.sysl")
  }

  "parse compile command with emit" in {
    val Some(config) = SyslCli.parse(Seq("compile", "--emit", "tof", "foo.sysl")): @unchecked
    val cmd = config.command.asInstanceOf[CompileCommand]
    cmd.emit shouldBe "tof"
    cmd.inputs shouldBe Seq("foo.sysl")
  }

  "parse compile command with output" in {
    val Some(config) = SyslCli.parse(Seq("compile", "-o", "out.asm", "foo.sysl")): @unchecked
    val cmd = config.command.asInstanceOf[CompileCommand]
    cmd.output shouldBe Some("out.asm")
  }

  "parse bare --emit without compile keyword" in {
    val Some(config) = SyslCli.parse(Seq("--emit", "llvm", "foo.sysl")): @unchecked
    val cmd = config.command.asInstanceOf[CompileCommand]
    cmd.emit shouldBe "llvm"
  }

  "parse bare -o without compile keyword" in {
    val Some(config) = SyslCli.parse(Seq("-o", "out.tof", "--emit", "tof", "foo.sysl")): @unchecked
    val cmd = config.command.asInstanceOf[CompileCommand]
    cmd.output shouldBe Some("out.tof")
    cmd.emit shouldBe "tof"
  }

  "parse run command single file" in {
    val Some(config) = SyslCli.parse(Seq("run", "foo.sysl")): @unchecked
    val cmd = config.command.asInstanceOf[RunCommand]
    cmd.inputs shouldBe Seq("foo.sysl")
  }

  "parse run command multiple files" in {
    val Some(config) = SyslCli.parse(Seq("run", "a.sysl", "b.sysl")): @unchecked
    val cmd = config.command.asInstanceOf[RunCommand]
    cmd.inputs shouldBe Seq("a.sysl", "b.sysl")
  }

  "parse run command directory" in {
    val Some(config) = SyslCli.parse(Seq("run", "examples/multifile")): @unchecked
    val cmd = config.command.asInstanceOf[RunCommand]
    cmd.inputs shouldBe Seq("examples/multifile")
  }

  "compile defaults to asm emit" in {
    val Some(config) = SyslCli.parse(Seq("foo.sysl")): @unchecked
    config.command.asInstanceOf[CompileCommand].emit shouldBe "asm"
  }

  "compile defaults to no output" in {
    val Some(config) = SyslCli.parse(Seq("foo.sysl")): @unchecked
    config.command.asInstanceOf[CompileCommand].output shouldBe None
  }

  // --- Doc command parsing ---

  "parse doc command single file" in {
    val Some(config) = SyslCli.parse(Seq("doc", "foo.lsysl")): @unchecked
    val cmd = config.command.asInstanceOf[DocCommand]
    cmd.inputs shouldBe Seq("foo.lsysl")
    cmd.output shouldBe None
  }

  "parse doc command with output" in {
    val Some(config) = SyslCli.parse(Seq("doc", "-o", "out.html", "foo.lsysl")): @unchecked
    val cmd = config.command.asInstanceOf[DocCommand]
    cmd.output shouldBe Some("out.html")
    cmd.inputs shouldBe Seq("foo.lsysl")
  }

  "parse doc command multiple files" in {
    val Some(config) = SyslCli.parse(Seq("doc", "a.lsysl", "b.lsysl")): @unchecked
    val cmd = config.command.asInstanceOf[DocCommand]
    cmd.inputs shouldBe Seq("a.lsysl", "b.lsysl")
  }

  "parse doc command with output directory" in {
    val Some(config) = SyslCli.parse(Seq("doc", "-o", "outdir", "a.lsysl", "b.lsysl")): @unchecked
    val cmd = config.command.asInstanceOf[DocCommand]
    cmd.output shouldBe Some("outdir")
  }

  "doc command requires input files" in {
    SyslCli.parse(Seq("doc")) shouldBe None
  }

  // --- Doc rendering ---

  "renderHTML produces highlighted code" in {
    val html = LiterateRenderer.renderHTML(
      """Some prose.
        |
        |    main() -> int
        |        42
        |""".stripMargin)
    html should include("<pre>")
    html should include("style=\"color:")
  }

  "renderPage produces full HTML document" in {
    val html = LiterateRenderer.renderPage(
      """# Hello
        |
        |    main() -> int
        |        0
        |""".stripMargin, "hello")
    html should include("<!DOCTYPE html>")
    html should include("<title>hello</title>")
    html should include("katex")
    html should include("<pre>")
  }

  "renderPage with back link includes nav" in {
    val html = LiterateRenderer.renderPage("Some text.\n\n    x = 1\n", "test", Some("index.html"))
    html should include("""<a href="index.html">""")
  }

  "renderIndex produces file list" in {
    val html = LiterateRenderer.renderIndex("mymod", Seq(("demo", "demo.html"), ("math", "math.html")))
    html should include("<!DOCTYPE html>")
    html should include("<title>mymod</title>")
    html should include("""<a href="demo.html">demo</a>""")
    html should include("""<a href="math.html">math</a>""")
  }

  // --- Interpreter execution (single file) ---

  private def interpret(source: String): (Long, String) =
    val output = new StringBuilder
    val parser = new SyslParser
    val Right(ast) = parser.parseProgram(source): @unchecked
    val analyzer = new SyslAnalyzer
    val typed = analyzer.analyze(ast)
    val interpreter = new SyslInterpreter(s => output ++= s)
    val result = interpreter.run(typed)
    (result, output.toString)

  "interpret hello" in {
    val (result, output) = interpret(
      """main() -> int
        |    putchar(72)
        |    putchar(105)
        |    0
        |""".stripMargin)
    result shouldBe 0
    output shouldBe "Hi"
  }

  "interpret factorial" in {
    val (result, _) = interpret(
      """factorial(n: int) -> int
        |    if n <= 1 then 1
        |    else n * factorial(n - 1)
        |main() -> int = factorial(10)
        |""".stripMargin)
    result shouldBe 3628800
  }

  "interpret fibonacci" in {
    val (result, _) = interpret(
      """fib(n: int) -> int
        |    var a: int = 0
        |    var b: int = 1
        |    for i = 0; i < n; i++
        |        var t: int = b
        |        b = a + b
        |        a = t
        |    a
        |main() -> int = fib(10)
        |""".stripMargin)
    result shouldBe 55
  }

  "interpret gcd" in {
    val (result, _) = interpret(
      """gcd(a: int, b: int) -> int
        |    while b != 0
        |        var t: int = b
        |        b = a % b
        |        a = t
        |    a
        |main() -> int = gcd(48, 18)
        |""".stripMargin)
    result shouldBe 6
  }

  "interpret function pointers" in {
    val (result, _) = interpret(
      """twice(x: int) -> int = x * 2
        |apply(f: func(int) -> int, x: int) -> int = f(x)
        |main() -> int = apply(twice, 21)
        |""".stripMargin)
    result shouldBe 42
  }

  "interpret arrays" in {
    val (result, _) = interpret(
      """main() -> int
        |    arr: [5]int
        |    for i = 0; i < 5; i++
        |        arr[i] = (i + 1) * 10
        |    sum = 0
        |    for i = 0; i < 5; i++
        |        sum += arr[i]
        |    sum
        |""".stripMargin)
    result shouldBe 150
  }

  "interpret pointers" in {
    val (result, _) = interpret(
      """swap(a: *int, b: *int) -> int
        |    var t: int = *a
        |    *a = *b
        |    *b = t
        |    0
        |main() -> int
        |    var x: int = 10
        |    var y: int = 20
        |    swap(&x, &y)
        |    x
        |""".stripMargin)
    result shouldBe 20
  }

  // --- Multi-file interpretation ---

  private def interpretMulti(sources: Map[String, String]): (Long, String) =
    val output = new StringBuilder
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interpreter = new SyslInterpreter(s => output ++= s)
    val value = interpreter.run(merged)
    (value, output.toString)

  "interpret multi-file program" in {
    val (result, _) = interpretMulti(Map(
      "math" ->
        """abs_val(x: int) -> int
          |    if x < 0 then -x else x
          |""".stripMargin,
      "main" ->
        """import math.*
          |main() -> int = abs_val(-42)
          |""".stripMargin,
    ))
    result shouldBe 42
  }

  "interpret multi-file with multiple imports" in {
    val (result, _) = interpretMulti(Map(
      "math" ->
        """add_one(x: int) -> int = x + 1
          |""".stripMargin,
      "util" ->
        """import math.*
          |add_two(x: int) -> int = add_one(add_one(x))
          |""".stripMargin,
      "main" ->
        """import util.*
          |main() -> int = add_two(40)
          |""".stripMargin,
    ))
    result shouldBe 42
  }

  "interpret multi-file with output" in {
    val (result, output) = interpretMulti(Map(
      "io" ->
        """greet() -> int
          |    putchar(72)
          |    putchar(105)
          |    0
          |""".stripMargin,
      "main" ->
        """import io.*
          |main() -> int
          |    greet()
          |    0
          |""".stripMargin,
    ))
    result shouldBe 0
    output shouldBe "Hi"
  }

  // --- Compile output ---

  "compile to asm produces valid assembly" in {
    val parser = new SyslParser
    val Right(ast) = parser.parseProgram(
      """main() -> int = 42
        |""".stripMargin): @unchecked
    val analyzer = new SyslAnalyzer
    val typed = analyzer.analyze(ast)
    val codegen = new SyslTriscCodegen
    val asm = codegen.generate(typed)
    asm should include("main")
    asm should include("jalr r0, r6")
  }

  "compile to tof produces linkable output" in {
    val parser = new SyslParser
    val Right(ast) = parser.parseProgram(
      """main() -> int = 42
        |""".stripMargin): @unchecked
    val analyzer = new SyslAnalyzer
    val typed = analyzer.analyze(ast)
    val codegen = new SyslTriscCodegen
    val asm = codegen.generate(typed)
    val tof = assemble(asm, relocatable = true)
    val linked = Linker.link(Seq(tof))
    linked.entryAddress shouldBe defined
  }

  "compile to llvm produces valid IR" in {
    val parser = new SyslParser
    val Right(ast) = parser.parseProgram(
      """main() -> int = 42
        |""".stripMargin): @unchecked
    val analyzer = new SyslAnalyzer
    val typed = analyzer.analyze(ast)
    val codegen = new SyslLLVMCodegen
    val ir = codegen.generate(typed)
    ir should include("define")
    ir should include("@main")
    ir should include("ret")
  }
}
