package io.github.edadma.trisc

import scopt.OParser

sealed trait SyslCommand
case class CompileCommand(
    inputs: Seq[String] = Seq.empty,
    output: Option[String] = None,
    emit: String = "asm", // asm, tof, llvm
    target: String = "host", // host, x86_64-elf, x86_64-linux, aarch64-elf, aarch64-linux
    noContracts: Boolean = false,
) extends SyslCommand
case class RunCommand(
    inputs: Seq[String] = Seq.empty,
    programArgs: Seq[String] = Seq.empty,
    noContracts: Boolean = false,
) extends SyslCommand
case class DocCommand(
    inputs: Seq[String] = Seq.empty,
    output: Option[String] = None,
) extends SyslCommand
case class TestCommand(
    inputs: Seq[String] = Seq.empty,
    filter: Option[String] = None,
    backend: String = "interpreter",
    failFast: Boolean = false,
    verbose: Boolean = false,
) extends SyslCommand

case class SyslConfig(
    command: SyslCommand = CompileCommand(),
)

object SyslCli:
  private val builder = OParser.builder[SyslConfig]

  private val parser =
    import builder.*
    OParser.sequence(
      programName("sysl"),
      head("sysl", "0.1.0"),
      // Default: compile
      cmd("compile")
        .text("Compile Sysl source files (default if no command given)")
        .action((_, c) => c.copy(command = CompileCommand()))
        .children(
          opt[String]('o', "output")
            .text("Output file or directory")
            .action((v, c) =>
              c.copy(command = c.command match
                case cc: CompileCommand => cc.copy(output = Some(v))
                case other              => other
              )
            ),
          opt[String]("emit")
            .text("Output format: asm (default), tof, llvm")
            .validate(v =>
              if Seq("asm", "tof", "llvm").contains(v) then success
              else failure(s"Unknown emit format: $v (expected asm, tof, llvm)")
            )
            .action((v, c) =>
              c.copy(command = c.command match
                case cc: CompileCommand => cc.copy(emit = v)
                case other              => other
              )
            ),
          opt[String]("target")
            .text("Target: host (default), x86_64-elf, x86_64-linux, aarch64-elf, aarch64-linux")
            .action((v, c) =>
              c.copy(command = c.command match
                case cc: CompileCommand => cc.copy(target = v)
                case other              => other
              )
            ),
          opt[Unit]("no-contracts")
            .text("Strip runtime contract checks (require/ensure/invariant/variant/type-predicates/type-attrs). Ada pragma Assertion_Policy(Disable).")
            .action((_, c) =>
              c.copy(command = c.command match
                case cc: CompileCommand => cc.copy(noContracts = true)
                case other              => other
              )
            ),
          arg[String]("<source>...")
            .unbounded()
            .text("Sysl source files or a directory")
            .action((v, c) =>
              c.copy(command = c.command match
                case cc: CompileCommand => cc.copy(inputs = cc.inputs :+ v)
                case other              => other
              )
            ),
        ),
      // run: interpret via tree-walker
      cmd("run")
        .text("Interpret a Sysl program (file, files, or directory)")
        .action((_, c) => c.copy(command = RunCommand()))
        .children(
          opt[Unit]("no-contracts")
            .text("Strip runtime contract checks (require/ensure/invariant/variant/type-predicates/type-attrs).")
            .action((_, c) =>
              c.copy(command = c.command match
                case rc: RunCommand => rc.copy(noContracts = true)
                case other          => other
              )
            ),
          arg[String]("<source>...")
            .unbounded()
            .text("Sysl source files or a directory")
            .action((v, c) =>
              c.copy(command = c.command match
                case rc: RunCommand => rc.copy(inputs = rc.inputs :+ v)
                case other          => other
              )
            ),
        ),
      // doc: generate styled HTML page from literate source
      cmd("doc")
        .text("Generate a styled HTML page from a .lsysl file")
        .action((_, c) => c.copy(command = DocCommand()))
        .children(
          opt[String]('o', "output")
            .text("Output file or directory")
            .action((v, c) =>
              c.copy(command = c.command match
                case dc: DocCommand => dc.copy(output = Some(v))
                case other          => other
              )
            ),
          arg[String]("<source>...")
            .unbounded()
            .text("Literate source files (.lsysl) or a directory")
            .action((v, c) =>
              c.copy(command = c.command match
                case dc: DocCommand => dc.copy(inputs = dc.inputs :+ v)
                case other          => other
              )
            ),
        ),
      // test: run #test-annotated functions
      cmd("test")
        .text("Discover and run #test functions")
        .action((_, c) => c.copy(command = TestCommand()))
        .children(
          opt[String]("filter")
            .text("Only run tests whose name contains this substring")
            .action((v, c) =>
              c.copy(command = c.command match
                case tc: TestCommand => tc.copy(filter = Some(v))
                case other           => other
              )
            ),
          opt[String]("backend")
            .text("Backend: interpreter (default) | llvm-host | svm-host | trisc | all")
            .validate(v =>
              if Seq("interpreter", "llvm-host", "svm-host", "trisc", "all").contains(v) then success
              else failure(s"Unknown backend: $v (expected interpreter, llvm-host, svm-host, trisc, all)")
            )
            .action((v, c) =>
              c.copy(command = c.command match
                case tc: TestCommand => tc.copy(backend = v)
                case other           => other
              )
            ),
          opt[Unit]("fail-fast")
            .text("Stop at first failing test")
            .action((_, c) =>
              c.copy(command = c.command match
                case tc: TestCommand => tc.copy(failFast = true)
                case other           => other
              )
            ),
          opt[Unit]("verbose")
            .text("Verbose output")
            .action((_, c) =>
              c.copy(command = c.command match
                case tc: TestCommand => tc.copy(verbose = true)
                case other           => other
              )
            ),
          arg[String]("<source>...")
            .unbounded()
            .text("Sysl source files or a directory")
            .action((v, c) =>
              c.copy(command = c.command match
                case tc: TestCommand => tc.copy(inputs = tc.inputs :+ v)
                case other           => other
              )
            ),
        ),
      // Allow bare options/args (no subcommand) to default to compile
      opt[String]('o', "output")
        .hidden()
        .action((v, c) =>
          c.copy(command = c.command match
            case cc: CompileCommand => cc.copy(output = Some(v))
            case other              => other
          )
        ),
      opt[String]("emit")
        .hidden()
        .action((v, c) =>
          c.copy(command = c.command match
            case cc: CompileCommand => cc.copy(emit = v)
            case other              => other
          )
        ),
      arg[String]("<source>...")
        .unbounded()
        .optional()
        .hidden()
        .action((v, c) =>
          c.copy(command = c.command match
            case cc: CompileCommand => cc.copy(inputs = cc.inputs :+ v)
            case other              => other
          )
        ),
      checkConfig(c =>
        c.command match
          case CompileCommand(inputs, _, _, _, _) if inputs.isEmpty =>
            failure("No input files specified")
          case RunCommand(inputs, _, _) if inputs.isEmpty =>
            failure("No input files specified for run")
          case DocCommand(inputs, _) if inputs.isEmpty =>
            failure("No input files specified for doc")
          case TestCommand(inputs, _, _, _, _) if inputs.isEmpty =>
            failure("No input files specified for test")
          case _ => success
      ),
    )

  def parse(args: Seq[String]): Option[SyslConfig] =
    // Split at "--": everything before goes to scopt, everything after becomes program args
    val dashIdx = args.indexOf("--")
    val (cliArgs, progArgs) = if dashIdx >= 0 then
      (args.take(dashIdx), args.drop(dashIdx + 1))
    else (args, Seq.empty)

    OParser.parse(parser, cliArgs, SyslConfig()).map { config =>
      if progArgs.nonEmpty then
        config.copy(command = config.command match
          case rc: RunCommand => rc.copy(programArgs = progArgs)
          case other          => other
        )
      else config
    }

  private case class CliError(msg: String) extends RuntimeException(msg)

  private def fail(msg: String): Nothing =
    System.err.println(msg)
    throw CliError(msg)

  def execute(config: SyslConfig): Unit =
    try
      config.command match
        case cmd: CompileCommand => executeCompile(cmd)
        case cmd: RunCommand     => executeRun(cmd)
        case cmd: DocCommand     => executeDoc(cmd)
        case cmd: TestCommand    => executeTest(cmd)
    catch case CliError(_) => () // already printed

  private def executeCompile(cmd: CompileCommand): Unit =
    val sources = resolveSources(cmd.inputs)
    val baseDirs = cmd.inputs.filter(p => io.exists(p) && io.isDirectory(p)).toList match
      case Nil => List(".")
      case dirs => dirs

    val config = if cmd.noContracts then Map("contracts" -> "off") else Map.empty[String, String]
    val driver = new SyslDriver(Some(io), baseDirs, config = config, tangler = Some(raw => LiterateRenderer.tangle(new LiterateParser().parse(raw))))
    val result = driver.compile(sources)

    // Write .smeta files for package modules
    for (path, meta) <- result.packageMetas do
      val parts = path.split("/")
      for base <- baseDirs do
        val dirPath = io.joinPath(base, path)
        if io.exists(dirPath) && io.isDirectory(dirPath) then
          io.writeFile(io.joinPath(dirPath, ".smeta"), meta.toSmeta)

    cmd.emit match
      case "asm" =>
        val codegen = new SyslTriscCodegen
        for unit <- result.units do
          val asm = codegen.generate(stripTestDecls(unit.typed))
          val outFile = outputPath(cmd.output, unit.name, ".asm", result.units.size)
          io.writeFile(outFile, asm)
          System.err.println(s"  ${unit.name} -> $outFile")

      case "tof" =>
        val codegen = new SyslTriscCodegen
        val tofs = for unit <- result.units yield
          val asm = codegen.generate(stripTestDecls(unit.typed))
          assemble(asm, relocatable = true)
        val linked = Linker.link(tofs, relocatable = true)
        val outFile = cmd.output.getOrElse("out.tof")
        io.writeFile(outFile, linked.serialize)
        System.err.println(s"  -> $outFile")

      case "llvm" =>
        val codegen = new SyslLLVMCodegen(cmd.target)
        val merged = stripTestDecls(TProgram(result.units.flatMap(_.typed.decls)))
        val ir = codegen.generate(merged)
        val outFile = cmd.output.getOrElse(result.units.head.name + ".ll")
        io.writeFile(outFile, ir)
        System.err.println(s"  -> $outFile")

      case _ => System.err.println(s"Unknown emit format: ${cmd.emit}")

  private def executeRun(cmd: RunCommand): Unit =
    val initialSources = resolveSources(cmd.inputs)
    val argv = cmd.programArgs.toArray

    val baseDirs = cmd.inputs.filter(p => io.exists(p) && io.isDirectory(p)).toList match
      case Nil => List(".")
      case dirs => dirs
    val sources = resolveTransitiveSources(initialSources, baseDirs)
    val config = if cmd.noContracts then Map("contracts" -> "off") else Map.empty[String, String]
    val driver = new SyslDriver(Some(io), baseDirs, config = config, tangler = Some(raw => LiterateRenderer.tangle(new LiterateParser().parse(raw))))
    val result = driver.compile(sources)
    val stdlibImports = driver.collectStdlibImports(result.units)
    val merged = stripTestDecls(TProgram(result.units.flatMap(_.typed.decls)))
    val interpreter = new SyslInterpreter()
    wireStdlib(interpreter, stdlibImports, argv)
    val value = interpreter.run(merged)
    if value != 0 then println(value)

  private def isTestFn(f: TFunDecl): Boolean =
    f.attributes.exists(_.name == "test")

  private def stripTestDecls(p: TProgram): TProgram =
    TProgram(p.decls.filter {
      case f: TFunDecl => !isTestFn(f)
      case _           => true
    })

  private case class DiscoveredTest(
      unitName: String,
      fn: TFunDecl,
      displayName: String,
      shouldPanic: Boolean,
      expectedMsg: Option[String],
      line: Option[Int],
  )

  private def attrString(a: AttrArg): Option[String] = a match
    case AttrPositional(AttrLitString(v)) => Some(v)
    case _                                => None

  private def attrIdent(a: AttrArg): Option[String] = a match
    case AttrPositional(AttrLitIdent(v)) => Some(v)
    case _                               => None

  private def attrNamedString(a: AttrArg, key: String): Option[String] = a match
    case AttrNamed(k, AttrLitString(v)) if k == key => Some(v)
    case _                                          => None

  /** Strip module prefix from a mangled name for display. */
  private def shortFnName(name: String): String =
    name.indexOf("__") match
      case -1 => name
      case i  => name.substring(i + 2)

  private def discoverTest(unitName: String, fn: TFunDecl): Option[DiscoveredTest] =
    fn.attributes.find(_.name == "test").map { attr =>
      val displayName = attr.args.flatMap(attrString).headOption.getOrElse(shortFnName(fn.name))
      val shouldPanicFlag = attr.args.exists(attrIdent(_).contains("should_panic"))
      val expectedMsg = attr.args.flatMap(a => attrNamedString(a, "should_panic")).headOption
      val sp = shouldPanicFlag || expectedMsg.isDefined
      val line = if attr.pos == scala.util.parsing.input.NoPosition then None else Some(attr.pos.line)
      DiscoveredTest(unitName, fn, displayName, sp, expectedMsg, line)
    }

  private sealed trait TestOutcome
  private case object Pass extends TestOutcome
  private case class Fail(msg: String, output: String = "") extends TestOutcome

  /** Compile a unit's scoped TProgram (tests included) to a native binary with a
    * test dispatcher shim. Returns the binary path on success, None on failure.
    * Binary takes a test function name as argv[1] and calls it; exits 0 on
    * clean return, non-zero when the test panics (via abort()).
    */
  private def compileUnitToLLVMBinary(program: TProgram, unitName: String): Either[String, String] =
    val testFns = program.decls.collect { case f: TFunDecl if isTestFn(f) => f.name }
    if testFns.isEmpty then return Left("no test functions in unit")

    val codegen = new SyslLLVMCodegen("host")
    val ir = try codegen.generate(program)
             catch case e: Throwable => return Left(s"IR codegen failed: ${e.getMessage}")

    val workDir = java.nio.file.Files.createTempDirectory("sysl-llvm-test-")
    val unitKey = unitName.replace("/", "_").replace(".", "_")
    val irPath = workDir.resolve(s"$unitKey.ll")
    java.nio.file.Files.writeString(irPath, ir)

    val shim = new StringBuilder
    shim ++= "#include <stdio.h>\n#include <string.h>\n"
    for fn <- testFns do shim ++= s"extern void $fn(void);\n"
    shim ++= "int main(int argc, char** argv) {\n"
    shim ++= "  if (argc < 2) { fprintf(stderr, \"missing test name\\n\"); return 1; }\n"
    shim ++= "  const char *name = argv[1];\n"
    for fn <- testFns do
      shim ++= s"""  if (!strcmp(name, "$fn")) { $fn(); return 0; }\n"""
    shim ++= "  fprintf(stderr, \"unknown test: %s\\n\", name);\n"
    shim ++= "  return 2;\n}\n"
    val cPath = workDir.resolve(s"$unitKey.c")
    java.nio.file.Files.writeString(cPath, shim.toString)

    val binPath = workDir.resolve(unitKey)
    val buildLog = new StringBuilder
    val logger = scala.sys.process.ProcessLogger(
      line => buildLog.append(line).append('\n'),
      line => buildLog.append(line).append('\n')
    )
    val exit = scala.sys.process.Process(
      Seq("clang", "-o", binPath.toString, irPath.toString, cPath.toString, "-w")
    ).!(logger)
    if exit != 0 then Left(s"clang failed (exit $exit): ${buildLog.toString.trim}")
    else Right(binPath.toString)

  private def runOneLLVM(
      program: TProgram,
      t: DiscoveredTest,
      binCache: scala.collection.mutable.Map[String, Either[String, String]],
  ): TestOutcome =
    val binResult = binCache.getOrElseUpdate(t.unitName, compileUnitToLLVMBinary(program, t.unitName))
    binResult match
      case Left(err) => Fail(s"unit compile failed: $err")
      case Right(bin) =>
        val outBuf = new StringBuilder
        val errBuf = new StringBuilder
        val logger = scala.sys.process.ProcessLogger(
          line => outBuf.append(line).append('\n'),
          line => errBuf.append(line).append('\n'),
        )
        val exit = scala.sys.process.Process(Seq(bin, t.fn.name)).!(logger)
        val output = outBuf.toString
        val errOut = errBuf.toString
        if exit == 0 then
          if t.shouldPanic then Fail("expected panic, got normal return", output) else Pass
        else
          val panicMsg = errOut.linesIterator.find(_.startsWith("panic: ")).map(_.stripPrefix("panic: "))
            .orElse(errOut.linesIterator.find(_.startsWith("assertion failed: ")).map(_.stripPrefix("assertion failed: ")))
            .getOrElse(errOut.trim)
          if t.shouldPanic then
            t.expectedMsg match
              case Some(substr) if !panicMsg.contains(substr) =>
                Fail(s"panic message did not contain '$substr' (got: $panicMsg)", output)
              case _ => Pass
          else Fail(s"panic: $panicMsg (exit $exit)", output)

  /** Compile + run a single test on the SVM bytecode interpreter.
    *
    * Strategy: codegen the test's scoped program to SVM asm, append a tiny
    * `main` wrapper that calls the target test function and pushes a unique
    * sentinel value before halting. Assemble, link with the SVM runtime
    * (boot + io), and run under a fresh SVM instance. If the post-run
    * top-of-stack equals the sentinel, the test returned normally; any other
    * TOS indicates the VM halted via `halt` (contract failure, assert, etc.).
    */
  private def runOneSVM(program: TProgram, t: DiscoveredTest): TestOutcome =
    val outputBuf = new StringBuilder
    val asm =
      try (new SyslSVMCodegen).generate(program)
      catch case e: Throwable =>
        if System.getenv("SVM_TRACE") != null then e.printStackTrace()
        return Fail(s"SVM codegen failed: ${e.getClass.getSimpleName}: ${e.getMessage}")
    if System.getenv("SVM_DUMP_ASM") != null then
      java.nio.file.Files.writeString(java.nio.file.Paths.get(s"/tmp/svm_${t.unitName.replace("/", "_")}.s"), asm)
    val sentinel = 0x5AFE_FADE_5AFE_FADEL
    val wrapperAsm =
      s"""|extern ${t.fn.name}
          |global main, func
          |entry main
          |
          |segment code
          |main:
          |  call ${t.fn.name}
          |  push_i64 $sentinel
          |  halt
          |""".stripMargin
    val programTof =
      try svmAssemble(asm, relocatable = true)
      catch case e: Throwable => return Fail(s"SVM assembly failed: ${e.getMessage}", outputBuf.toString)
    val wrapperTof =
      try svmAssemble(wrapperAsm, relocatable = true)
      catch case e: Throwable => return Fail(s"SVM wrapper assembly failed: ${e.getMessage}", outputBuf.toString)
    val linked =
      try Linker.link(Seq(SVMRuntime.bootTof, programTof, wrapperTof, SVMRuntime.ioTof))
      catch case e: Throwable => return Fail(s"SVM link failed: ${e.getMessage}", outputBuf.toString)
    val stdout = new Stdout(SVMRuntime.stdoutAddress, s => outputBuf ++= s)
    val ram = new RAM(0, SVMRuntime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    try linked.load(mem)
    catch case e: Throwable => return Fail(s"SVM load failed: ${e.getMessage}", outputBuf.toString)
    val svm = new SVM(mem) { limit = 50_000_000 }
    try
      svm.reset()
      svm.run()
    catch case e: Throwable =>
      return Fail(s"SVM runtime error: ${e.getClass.getSimpleName}: ${e.getMessage}", outputBuf.toString)
    val captured = outputBuf.toString
    if svm.result == sentinel then
      if t.shouldPanic then Fail("expected panic, got normal return", captured) else Pass
    else
      if t.shouldPanic then
        t.expectedMsg match
          case Some(substr) if !captured.contains(substr) =>
            Fail(s"panic message did not contain '$substr'", captured)
          case _ => Pass
      else Fail(s"panic (halt): svm.result=0x${svm.result.toHexString}", captured)

  private def runOneInterpreter(program: TProgram, stdlibImports: Set[String], t: DiscoveredTest): TestOutcome =
    val outputBuf = new StringBuilder
    val interp = new SyslInterpreter(s => outputBuf ++= s)
    wireStdlib(interp, stdlibImports)
    try interp.load(program)
    catch case e: Throwable => return Fail(s"test init failed: ${e.getMessage}")
    val captured = outputBuf.toString
    try
      interp.runNamed(t.fn.name)
      if t.shouldPanic then Fail("expected panic, got normal return", captured) else Pass
    catch
      case e: RuntimeException if e.getClass.getSimpleName == "RuntimeError" =>
        val msg = Option(e.getMessage).getOrElse("")
        if !t.shouldPanic then Fail(s"panic: $msg", captured)
        else t.expectedMsg match
          case Some(substr) if !msg.contains(substr) =>
            Fail(s"panic message did not contain '$substr' (got: $msg)", captured)
          case _ => Pass
      case e: Throwable => Fail(s"unexpected error: ${e.getClass.getSimpleName}: ${e.getMessage}", captured)

  private def executeTest(cmd: TestCommand): Unit =
    if cmd.backend == "trisc" || cmd.backend == "all" then
      System.err.println(s"error: backend '${cmd.backend}' not yet implemented (use 'interpreter', 'llvm-host', or 'svm-host')")
      throw CliError("unsupported backend")

    // Always use project root as base so module paths resolve correctly.
    // e.g. std/regex/regex.lsysl → key "std/regex/regex" → module "std.regex"
    // This works regardless of input depth (std/, std/regex/, std/regex/regex.lsysl).
    val baseDirs = List(".")
    val initialSources: Map[String, String] =
      cmd.inputs.flatMap { p =>
        if !io.exists(p) then fail(s"error: file not found: $p")
        if io.isDirectory(p) then
          collectSyslFiles(p).map(f => resolveSource(f, ""))
        else
          List(resolveSource(p, ""))
      }.toMap
    val sources = resolveTransitiveSources(initialSources, baseDirs)
    val driver = new SyslDriver(Some(io), baseDirs, tangler = Some(raw => LiterateRenderer.tangle(new LiterateParser().parse(raw))))
    val result = driver.compile(sources, keepTests = true)
    val stdlibImports = driver.collectStdlibImports(result.units)

    // Build per-test scoped programs so that each test sees only the
    // functions from its own unit + transitively-imported units.
    //
    // Without this, merging all units into one TProgram lets functions
    // from unrelated modules (e.g. std.bytes.trim_space) shadow the
    // caller's intended function (std.strings.trim_space), since
    // the interpreter keeps a flat function-name -> TFunDecl map.
    val unitImports: Map[String, Set[String]] =
      result.units.map(u =>
        u.name -> u.typed.decls.collect { case TImportDecl(p) => p }.toSet
      ).toMap
    val unitsByModule: Map[String, List[CompilationUnit]] =
      result.units.groupBy(_.modulePath.getOrElse(""))
    val unitByName: Map[String, CompilationUnit] =
      result.units.map(u => u.name -> u).toMap
    def reachableUnits(startUnitName: String): Set[String] =
      val visited = scala.collection.mutable.Set[String](startUnitName)
      val queue = scala.collection.mutable.Queue[String](startUnitName)
      while queue.nonEmpty do
        val u = queue.dequeue()
        val myModule = unitByName.get(u).flatMap(_.modulePath).getOrElse("")
        // Same-module siblings
        for sib <- unitsByModule.getOrElse(myModule, Nil) if !visited(sib.name) do
          visited += sib.name
          queue += sib.name
        // Imported modules (only those that are in-tree; stdlib is wired separately)
        for imp <- unitImports.getOrElse(u, Set.empty) do
          for impUnit <- unitsByModule.getOrElse(imp, Nil) if !visited(impUnit.name) do
            visited += impUnit.name
            queue += impUnit.name
      visited.toSet
    val scopedPrograms = scala.collection.mutable.Map[String, TProgram]()
    def programFor(unitName: String): TProgram =
      scopedPrograms.getOrElseUpdate(unitName, {
        val reach = reachableUnits(unitName)
        TProgram(result.units.filter(u => reach(u.name)).flatMap(_.typed.decls))
      })

    // Discover tests, tagging each with its unit
    val discovered = result.units.flatMap { unit =>
      unit.typed.decls.collect { case f: TFunDecl => f }
        .flatMap(discoverTest(unit.name, _))
    }

    val filtered = cmd.filter match
      case None => discovered
      case Some(pat) => discovered.filter(t =>
        shortFnName(t.fn.name).contains(pat) || t.displayName.contains(pat) || t.fn.name.contains(pat))

    println(s"running ${filtered.size} tests (backend: ${cmd.backend})")

    var passed = 0
    var failed = 0
    val totalStart = System.nanoTime()
    var currentUnit = ""
    var stop = false

    // Cache compiled binaries per unit so tests in the same file share one
    // clang invocation. Stored as Either so a failed unit compile is
    // reported once and then propagated as a per-test Fail.
    val llvmBinCache = scala.collection.mutable.Map[String, Either[String, String]]()

    for t <- filtered if !stop do
      if t.unitName != currentUnit then
        currentUnit = t.unitName
        println(currentUnit)
      val start = System.nanoTime()
      val outcome = cmd.backend match
        case "llvm-host" => runOneLLVM(programFor(t.unitName), t, llvmBinCache)
        case "svm-host"  => runOneSVM(programFor(t.unitName), t)
        case _           => runOneInterpreter(programFor(t.unitName), stdlibImports, t)
      val elapsedMs = (System.nanoTime() - start) / 1e6
      outcome match
        case Pass =>
          passed += 1
          println(f"  ✓ ${t.displayName}%-28s ($elapsedMs%.1fms)")
        case Fail(msg, output) =>
          failed += 1
          println(f"  ✗ ${t.displayName}%-28s ($elapsedMs%.1fms)")
          println(s"      $msg")
          t.line.foreach(l => println(s"      at ${t.unitName}:$l"))
          if output.nonEmpty then
            for line <- output.split("\n") do
              println(s"      | $line")
          if cmd.failFast then stop = true

    val totalMs = (System.nanoTime() - totalStart) / 1e6
    val skipped = discovered.size - filtered.size
    println(f"\n$passed passed, $failed failed, $skipped skipped — $totalMs%.1fms")
    if failed > 0 then throw CliError(s"$failed test(s) failed")

  private def executeDoc(cmd: DocCommand): Unit =
    val isModule = cmd.inputs.size == 1 && io.isDirectory(cmd.inputs.head)
    val sources = resolveLiterateSources(cmd.inputs)
    val backLink = if isModule then Some("index.html") else None

    for (name, source) <- sources do
      val html = LiterateRenderer.renderPage(source, name, backLink)
      val outFile = outputPath(cmd.output, name, ".html", sources.size)
      io.writeFile(outFile, html)
      System.err.println(s"  $name -> $outFile")

    if isModule then
      val moduleName = io.fileName(cmd.inputs.head)
      val sorted = sources.keys.toSeq.sorted.map(n => (n, n + ".html"))
      val index = LiterateRenderer.renderIndex(moduleName, sorted)
      val outDir = cmd.output.getOrElse(".")
      if !io.exists(outDir) then io.mkdirs(outDir)
      val indexFile = io.joinPath(outDir, "index.html")
      io.writeFile(indexFile, index)
      System.err.println(s"  index -> $indexFile")

  private def resolveLiterateSources(inputs: Seq[String]): Map[String, String] =
    if inputs.size == 1 && io.isDirectory(inputs.head) then
      val files = io.listFiles(inputs.head).filter(f => io.fileName(f).endsWith(".lsysl"))
      if files.isEmpty then
        fail(s"error: no .lsysl files in directory: ${inputs.head}")
      files.map(f => (io.fileName(f).stripSuffix(".lsysl"), io.readFile(f))).toMap
    else
      inputs.map { path =>
        if !io.exists(path) then
          fail(s"error: file not found: $path")
        if !io.fileName(path).endsWith(".lsysl") then
          fail(s"error: not a literate source file: $path")
        (io.fileName(path).stripSuffix(".lsysl"), io.readFile(path))
      }.toMap

  private def wireStdlib(interpreter: SyslInterpreter, imports: Set[String], argv: Array[String] = Array.empty): Unit =
    if imports.nonEmpty then
      val ctx = new SyslStdlib.StdlibContext(argv = argv)
      for mod <- imports do
        // Extern functions are NOT module-mangled (ABI-level names)
        interpreter.registerBuiltins(SyslStdlib.builtins(mod, ctx))
      // Val constants ARE module-mangled (e.g., O_RDONLY → std_io__O_RDONLY)
      if imports.contains("std/io") then
        for (name, value) <- SyslStdlib.ioConstants do
          interpreter.registerGlobal(s"std_io__$name", value)

  private def io: FileOps = FileOps.instance

  private def isSyslSource(name: String): Boolean =
    name.endsWith(".sysl") || name.endsWith(".lsysl")

  /** Resolve a source file, returning (relative-path-without-extension, source-code). */
  private def resolveSource(path: String, baseDir: String): (String, String) =
    val name = io.fileName(path)
    val raw = io.readFile(path)
    // Compute relative path from base directory
    val relPath = if path.startsWith(baseDir) then
      val rel = path.drop(baseDir.length).dropWhile(c => c == '/' || c == '\\')
      if rel.nonEmpty then rel else name
    else name
    val key = if relPath.endsWith(".lsysl") then relPath.stripSuffix(".lsysl")
    else relPath.stripSuffix(".sysl")
    val source = if name.endsWith(".lsysl") then
      val doc = new LiterateParser().parse(raw)
      LiterateRenderer.tangle(doc)
    else raw
    (key, source)

  private def resolveSources(inputs: Seq[String]): Map[String, String] =
    if inputs.size == 1 && io.isDirectory(inputs.head) then
      val baseDir = inputs.head + (if inputs.head.endsWith("/") then "" else "/")
      val files = collectSyslFiles(inputs.head)
      if files.isEmpty then
        fail(s"error: no .sysl or .lsysl files in directory: ${inputs.head}")
      files.map(f => resolveSource(f, baseDir)).toMap
    else
      inputs.map { path =>
        if !io.exists(path) then
          fail(s"error: file not found: $path")
        resolveSource(path, "")
      }.toMap

  /** Expand a source map by resolving transitive in-tree imports from the file system.
    * Parses each source to extract imports, looks for matching .sysl/.lsysl files on disk,
    * and repeats until no new sources are discovered. Skips JVM builtin modules.
    */
  private def resolveTransitiveSources(initial: Map[String, String], baseDirs: List[String]): Map[String, String] =
    val sources = scala.collection.mutable.LinkedHashMap[String, String]() ++= initial
    val processed = scala.collection.mutable.Set[String]()
    val parser = new SyslParser
    val dirs = if baseDirs.isEmpty then List(".") else baseDirs

    // Extract import module paths from a source string
    def extractImportPaths(source: String): Seq[String] =
      parser.parseProgram(source) match
        case Right(ast) => ast.decls.collect { case imp: ImportDeclAST => imp.modulePath }
        case Left(_) => Seq.empty

    // Try to resolve a module path to source files on disk
    def resolveModuleSources(modPath: String): Seq[(String, String)] =
      dirs.iterator.flatMap { base =>
        val basePrefix = if base == "." then "./" else if base.endsWith("/") then base else base + "/"
        val dirPath = io.joinPath(base, modPath)
        val syslPath = s"${io.joinPath(base, modPath)}.sysl"
        val lsyslPath = s"${io.joinPath(base, modPath)}.lsysl"

        if io.exists(syslPath) then
          Seq(resolveSource(syslPath, basePrefix))
        else if io.exists(lsyslPath) then
          Seq(resolveSource(lsyslPath, basePrefix))
        else if io.exists(dirPath) && io.isDirectory(dirPath) then
          collectSyslFiles(dirPath).map(f => resolveSource(f, basePrefix))
        else
          Seq.empty
      }.toSeq

    var changed = true
    while changed do
      changed = false
      val currentKeys = sources.keys.toList
      for key <- currentKeys if !processed(key) do
        processed += key
        val importPaths = extractImportPaths(sources(key))
        for modPath <- importPaths do
          if !SyslStdlib.builtinModules.contains(modPath) then
            val resolved = resolveModuleSources(modPath)
            for (rKey, rSource) <- resolved do
              if !sources.contains(rKey) then
                sources(rKey) = rSource
                changed = true

    sources.toMap

  /** Recursively collect all .sysl/.lsysl files under a directory. */
  private def collectSyslFiles(dir: String): Seq[String] =
    io.listFiles(dir).flatMap { f =>
      if io.isDirectory(f) then collectSyslFiles(f)
      else if isSyslSource(io.fileName(f)) then Seq(f)
      else Seq.empty
    }

  private def outputPath(output: Option[String], name: String, ext: String, unitCount: Int): String =
    output match
      case Some(out) if unitCount == 1 => out
      case Some(out) =>
        if !io.exists(out) then io.mkdirs(out)
        io.joinPath(out, name + ext)
      case None => name + ext
