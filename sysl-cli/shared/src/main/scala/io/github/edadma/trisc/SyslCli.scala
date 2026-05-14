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
case class ProveCommand(
    inputs: Seq[String] = Seq.empty,
    output: Option[String] = None,
) extends SyslCommand
case class FetchCommand(
    root: String = ".",
) extends SyslCommand
case class UpdateCommand(
    root: String = ".",
) extends SyslCommand
case class TreeCommand(
    root: String = ".",
) extends SyslCommand

/** How the CLI should treat `sysl.lock` for this invocation.
 *  - `Default`: lock is written/refreshed as needed; resolver may hit the network for git refs.
 *  - `Locked`: lock must already match what the resolver would produce; otherwise error. Lock is not rewritten.
 *  - `Frozen`: stricter `Locked` — also forbids any network IO. Every git URL must already be pinned by the lock and present in the cache. */
enum LockMode:
  case Default, Locked, Frozen

case class SyslConfig(
    command: SyslCommand = CompileCommand(),
    lockMode: LockMode = LockMode.Default,
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
            .text("Target: host (default), x86_64-elf, x86_64-linux, aarch64-elf, aarch64-linux, riscv64-elf, riscv32-elf")
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
            .text("Backend: interpreter (default) | llvm-host | svm-host | trisc | riscv64 | riscv32 | wasm32 | all")
            .validate(v =>
              if Seq("interpreter", "llvm-host", "svm-host", "trisc", "riscv64", "riscv32", "wasm32", "all").contains(v) then success
              else failure(s"Unknown backend: $v (expected interpreter, llvm-host, svm-host, trisc, riscv64, riscv32, wasm32, all)")
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
      // fetch: resolve the dep graph and write/refresh sysl.lock
      cmd("fetch")
        .text("Resolve [dependencies] and write sysl.lock without compiling")
        .action((_, c) => c.copy(command = FetchCommand()))
        .children(
          arg[String]("[root]")
            .optional()
            .text("Project or workspace root (default: cwd)")
            .action((v, c) =>
              c.copy(command = c.command match
                case fc: FetchCommand => fc.copy(root = v)
                case other            => other
              )
            ),
        ),
      // update: re-resolve the dep graph and overwrite sysl.lock
      cmd("update")
        .text("Re-resolve [dependencies] and rewrite sysl.lock from scratch")
        .action((_, c) => c.copy(command = UpdateCommand()))
        .children(
          arg[String]("[root]")
            .optional()
            .text("Project or workspace root (default: cwd)")
            .action((v, c) =>
              c.copy(command = c.command match
                case uc: UpdateCommand => uc.copy(root = v)
                case other             => other
              )
            ),
        ),
      // tree: print the resolved dep graph as an indented tree
      cmd("tree")
        .text("Print the resolved dependency graph as an indented tree")
        .action((_, c) => c.copy(command = TreeCommand()))
        .children(
          arg[String]("[root]")
            .optional()
            .text("Project or workspace root (default: cwd)")
            .action((v, c) =>
              c.copy(command = c.command match
                case tc: TreeCommand => tc.copy(root = v)
                case other           => other
              )
            ),
        ),
      // prove: emit equivalent WhyML for offline discharge with Why3
      cmd("prove")
        .text("Translate Sysl source to WhyML (input language for the Why3 verifier)")
        .action((_, c) => c.copy(command = ProveCommand()))
        .children(
          opt[String]('o', "output")
            .text("Output file (default: stdout)")
            .action((v, c) =>
              c.copy(command = c.command match
                case pc: ProveCommand => pc.copy(output = Some(v))
                case other            => other
              )
            ),
          arg[String]("<source>...")
            .unbounded()
            .text("Sysl source files")
            .action((v, c) =>
              c.copy(command = c.command match
                case pc: ProveCommand => pc.copy(inputs = pc.inputs :+ v)
                case other            => other
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
      // --locked / --frozen apply to every command that resolves dependencies.
      // Made top-level (rather than per-command) so the flag works the same
      // way regardless of subcommand. `--frozen` implies `--locked` plus
      // "no network IO" — git refs must already be pinned in `sysl.lock`.
      opt[Unit]("locked")
        .text("Require sysl.lock to already match what the resolver would produce; do not modify it")
        .action((_, c) =>
          c.copy(lockMode = if c.lockMode == LockMode.Frozen then c.lockMode else LockMode.Locked)
        ),
      opt[Unit]("frozen")
        .text("Like --locked, but also forbid network access (git deps must be pinned and cached)")
        .action((_, c) => c.copy(lockMode = LockMode.Frozen)),
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
          case ProveCommand(inputs, _) if inputs.isEmpty =>
            failure("No input files specified for prove")
          case _: UpdateCommand if c.lockMode != LockMode.Default =>
            failure("--locked / --frozen are incompatible with `update` (which exists to mutate the lock)")
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
      val lm = config.lockMode
      config.command match
        case cmd: CompileCommand => executeCompile(cmd, lm)
        case cmd: RunCommand     => executeRun(cmd, lm)
        case cmd: DocCommand     => executeDoc(cmd)
        case cmd: TestCommand    => executeTest(cmd, lm)
        case cmd: ProveCommand   => executeProve(cmd)
        case cmd: FetchCommand   => executeFetch(cmd, lm)
        case cmd: UpdateCommand  => executeUpdate(cmd)
        case cmd: TreeCommand    => executeTree(cmd)
    catch case CliError(_) => () // already printed

  private def executeCompile(cmd: CompileCommand, lockMode: LockMode): Unit =
    val sources = resolveSources(cmd.inputs)
    val resolved = resolveDeps(cmd.inputs, ignoreLockPins = false, lockMode) match
      case Right(r) => r
      case Left(msg) => fail(s"error: $msg")
    applyLockMode(resolved, lockMode)
    val inputDirs = cmd.inputs.filter(p => io.exists(p) && io.isDirectory(p)).toList
    val baseDirs = (inputDirs ++ resolved.searchRoots).distinct match
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

  private def executeRun(cmd: RunCommand, lockMode: LockMode): Unit =
    val initialSources = resolveSources(cmd.inputs)
    val argv = cmd.programArgs.toArray

    val resolved = resolveDeps(cmd.inputs, ignoreLockPins = false, lockMode) match
      case Right(r) => r
      case Left(msg) => fail(s"error: $msg")
    applyLockMode(resolved, lockMode)
    val inputDirs = cmd.inputs.filter(p => io.exists(p) && io.isDirectory(p)).toList
    val baseDirs = (inputDirs ++ resolved.searchRoots ++ List(".")).distinct
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
      if System.getenv("SVM_TRACE") != null then
        e.printStackTrace()
        val ip = svm.ip
        val allSyms = for seg <- linked.segments; sym <- seg.symbols yield (sym.name, seg.org + sym.offset)
        val sorted = allSyms.sortBy(_._2)
        val before = sorted.filter { case (_, addr) => addr <= ip }.takeRight(3)
        val after = sorted.filter { case (_, addr) => addr > ip }.take(3)
        System.err.println(s"SVM_TRACE: context near 0x${ip.toHexString}:")
        for (n, a) <- before ++ after do System.err.println(f"  0x$a%x $n")
      return Fail(s"SVM runtime error: ${e.getClass.getSimpleName}: ${e.getMessage} at IP=0x${svm.ip.toHexString}", outputBuf.toString)
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

  /** Compile + run a single test on the TRISC emulator.
    *
    * Strategy: codegen the test's scoped program to TRISC asm, append a boot
    * wrapper with a vector table that calls the target test function and
    * halts. The wrapper's trap and fault ISRs each record a distinct sentinel
    * byte at a known memory location before halting, because TRISC `trap N`
    * doesn't halt the CPU — it transfers control to the matching vector slot,
    * which would otherwise look identical to a clean return.
    *
    * Memory layout:
    *   0x00000..0x0009F  vector table (20 slots × 8 bytes)
    *   0x000A0..0x1FEF7  code + data + stack (grows down from 0x1FEF8)
    *   0x1FF00           panic flag byte (0=clean, sysl-code=panic, 222=fault)
    *   0x20000           STDOUT MMIO (1 byte)
    *
    * Outcome is read from `cpu.state` and the panic flag byte:
    *   - state == Halt, flag == 0     → clean return → Pass (or Fail if shouldPanic)
    *   - state == Halt, flag in 1..5  → sysl panic → Pass if shouldPanic, else Fail
    *   - state == Halt, flag == 222   → CPU fault routed through fault ISR → Fail
    *   - state == Run                 → cycle-limit reached → Fail (timeout)
    *   - any other state              → CPU fault before fault ISR ran → Fail
    *
    * Limitations: TRISC `assert`/`panic` lower to inline `trap 1` with no
    * message printing, so the `should_panic = "msg"` substring match cannot
    * be verified — any trap satisfies a shouldPanic test. Tests that exercise
    * heap allocation (`new`, dynamic strings, `s"..."` interpolation) require
    * malloc/free externs which this minimal runtime does not yet provide;
    * those tests will fail at link time. Both gaps are tracked as follow-ups.
    *
    * `TRISC_DUMP_ASM=1` writes the generated asm to /tmp for inspection.
    * `TRISC_TRACE=1` prints a one-line per-test summary and lets the CPU emit
    * its diagnostic stderr (otherwise quiet).
    */
  private def runOneTRISC(program: TProgram, t: DiscoveredTest): TestOutcome =
    val outputBuf = new StringBuilder
    val asm =
      try (new SyslTriscCodegen).generate(program)
      catch case e: Throwable =>
        if System.getenv("TRISC_TRACE") != null then e.printStackTrace()
        return Fail(s"TRISC codegen failed: ${e.getClass.getSimpleName}: ${e.getMessage}")
    if System.getenv("TRISC_DUMP_ASM") != null then
      java.nio.file.Files.writeString(
        java.nio.file.Paths.get(s"/tmp/trisc_${t.unitName.replace("/", "_")}.s"), asm)
    // Memory layout (1MB RAM):
    //   0x000000..0x00009F  vector table (20 × 8 bytes)
    //   0x0000A0..          wrapper code, then user program code + rodata +
    //                       data + const segments, all packed sequentially
    //                       by the linker. Allowed to grow up to HEAP_START.
    //   0x080000..0x0FFE00  bump-allocator heap (~512 KB)
    //   0x0FFEF8            initial SP (stack grows down into 0x0FFE00..0x0FFEF8)
    //   0x0FFF00            panic flag (1 byte)
    //   0x0FFF08            heap brk pointer (8 bytes; init to 0 by RAM zero,
    //                       lazily set to HEAP_START on first malloc)
    //   0x0FFF10            saved-PC slot (8 bytes, written by fault_isr; the
    //                       supervisor exception frame puts saved PC at [r7])
    //   0x0FFF20..0x0FFF4F  saved r1..r6 (6 × 8 bytes, written by fault_isr
    //                       before its body clobbers them; r6=LR is most
    //                       useful for "jalr to garbage" CPU faults)
    //   0x100000            STDOUT (1 byte, write-only device)
    //
    // Earlier layout pinned SP and the metadata slots at ~0x1FF00, with the
    // heap above. That broke for any program whose code+rodata exceeded
    // ~128 KB (e.g. std/strings) — the linker placed rodata over the
    // metadata region, silently corrupting BRK_PTR. Pushing the metadata
    // up to high RAM (just below STDOUT) and bumping HEAP_START gives
    // generous program space. HEAP_START was 0x80000 (= 512 KB code,
    // 512 KB heap) but std/flag's per-suite combined binary plus the
    // method-on-temp / slice-index reclaims pushed it just past 0x80000;
    // 0xC0000 gives ~768 KB of program space and ~256 KB of heap, which
    // is plenty for every std/ test.
    val stdoutAddr = 0x100000L
    val ramSize = 0x100000L
    val panicFlagAddr = 0xFFF00L
    val brkPtrAddr = 0xFFF08L
    val faultPcAddr = 0xFFF10L
    val faultRegsAddr = 0xFFF20L  // r1..r6, 8 bytes each
    val heapStart = 0xC0000L
    val heapEnd = 0xFFE00L
    val initialSP = 0xFFEF8L
    val faultIsrSlots = (1 to 7).map(_ => "  dl fault_isr").mkString("\n")
    val trapIsrSlots = (1 to 8).map(_ => "  dl panic_isr").mkString("\n")
    val tailFaultSlots = (1 to 3).map(_ => "  dl fault_isr").mkString("\n")
    val wrapperAsm =
      s"""|STDOUT = $stdoutAddr
          |BRK_PTR = $brkPtrAddr
          |HEAP_START = $heapStart
          |HEAP_END = $heapEnd
          |FAULT_PC = $faultPcAddr
          |FAULT_REGS = $faultRegsAddr
          |
          |segment vectors
          |  dl $initialSP
          |  dl boot
          |$faultIsrSlots
          |$trapIsrSlots
          |$tailFaultSlots
          |
          |segment code
          |
          |extern ${t.fn.name}
          |
          |global boot, func
          |entry boot
          |
          |boot
          |  movi r4, ${t.fn.name}
          |  jalr r6, r4
          |  halt
          |
          |global putchar, func
          |putchar
          |  movi r2, STDOUT
          |  stb r1, r2, r0
          |  jalr r0, r6
          |
          |; write_str(fd: int, s: string) -> int
          |; The TRISC test runner has a single STDOUT device; fd is ignored.
          |; ABI: r1 = fd (ignored), [r7+0] = s.ptr, [r7+8] = s.len. Returns
          |; the number of bytes written (= s.len) in r1. Uses r1..r4 and the
          |; return-stack slot pushed by pshd; preserves r5/r6/r7 per ABI and
          |; does not mutate the caller's stack args.
          |global write_str, func
          |write_str
          |  ldd r2, r7, r0        ; r2 = s.ptr
          |  addi r3, r7, 8
          |  ldd r3, r3, r0        ; r3 = s.len
          |  pshd r3               ; save original len for return
          |  movi r4, STDOUT
          |.write_str_loop
          |  beq r3, r0, .write_str_done
          |  ldb r1, r2, r0        ; r1 = *p
          |  stb r1, r4, r0        ; STDOUT = byte
          |  addi r2, r2, 1        ; p++
          |  addi r3, r3, -1       ; len--
          |  bra .write_str_loop
          |.write_str_done
          |  popd r1               ; r1 = original len (return value)
          |  jalr r0, r6
          |
          |; exit(code: int) — halt the VM. r1 = code (placed in panic flag for
          |; debugger visibility but not interpreted by the test runner). The
          |; runner treats `halt` as success unless the panic flag has a sysl
          |; sentinel (1..5, 99); arbitrary `exit` codes from std.process are
          |; therefore stored to keep the slot consistent but won't be confused
          |; with a panic by the harness.
          |global exit, func
          |exit
          |  halt
          |
          |; std.io descriptors. The test runner has only one device (stdout);
          |; reads/writes to STDIN/STDERR FDs aren't routed anywhere special,
          |; but the symbols must exist for std.log et al. to link.
          |segment data
          |  align 8
          |global std_io__STDIN, data, 8
          |std_io__STDIN:  dl 0
          |  align 8
          |global std_io__STDOUT, data, 8
          |std_io__STDOUT: dl 1
          |  align 8
          |global std_io__STDERR, data, 8
          |std_io__STDERR: dl 2
          |segment code
          |
          |; panic_isr: invoked by trap N (sysl panics). r1 holds the sysl
          |; error code (1=oob, 2=null, 3=abort, 4=assert/panic, 5=div0,
          |; 99=brk-corruption sentinel).
          |; Records r1 to the panic flag AND saves the faulting PC and r1..r4
          |; into the same FAULT_REGS slots used by fault_isr, so the runner
          |; can dump them on demand for trap-99 (brk-corrupt) diagnostics.
          |global panic_isr, func
          |panic_isr
          |  ; Save r1..r4 — clobbered below.
          |  movi r6, FAULT_REGS
          |  std r1, r6, r0
          |  addi r6, r6, 8
          |  std r2, r6, r0
          |  addi r6, r6, 8
          |  std r3, r6, r0
          |  addi r6, r6, 8
          |  std r4, r6, r0
          |  ; Save the faulting PC (same exception frame layout as fault_isr).
          |  ldd r3, r7, r0
          |  movi r2, FAULT_PC
          |  std r3, r2, r0
          |  ; Record the panic code.
          |  movi r2, $panicFlagAddr
          |  stb r1, r2, r0
          |  halt
          |
          |; fault_isr: invoked by hardware faults (instruction-access, etc.).
          |; Records a distinct sentinel + the saved PC so the runner can
          |; distinguish a CPU fault from a sysl panic AND report the actual
          |; faulting instruction. On exception entry the CPU pushes PSR then
          |; PC onto the supervisor stack (see CPU.enterException), so when
          |; fault_isr starts r7 points at the saved PC.
          |global fault_isr, func
          |fault_isr
          |  ; Save all caller registers r1..r6 to FAULT_REGS for diagnostics.
          |  ; r6 is the link register — for "we jalr'd to garbage" CPU faults
          |  ; it's the most useful clue, so we preserve its original value by
          |  ; pushing it on the supervisor stack BEFORE using r6 as scratch.
          |  pshd r6
          |  movi r6, FAULT_REGS
          |  std r1, r6, r0
          |  addi r6, r6, 8
          |  std r2, r6, r0
          |  addi r6, r6, 8
          |  std r3, r6, r0
          |  addi r6, r6, 8
          |  std r4, r6, r0
          |  addi r6, r6, 8
          |  std r5, r6, r0
          |  ; r6 slot: read original r6 back from the supervisor stack (we
          |  ; pushed it first, so it sits at [r7+0]) and store to FAULT_REGS+40.
          |  addi r6, r6, 8
          |  ldd r4, r7, r0
          |  std r4, r6, r0
          |  addi r7, r7, 8       ; pop the saved-r6 slot
          |  ; Save the faulting PC. On exception entry the CPU pushes
          |  ; PSR then PC onto the supervisor stack, and the new r7 points
          |  ; at the saved PC (see CPU.enterException).
          |  ldd r3, r7, r0
          |  movi r2, FAULT_PC
          |  std r3, r2, r0
          |  ; Sentinel.
          |  movi r1, 222
          |  movi r2, $panicFlagAddr
          |  stb r1, r2, r0
          |  halt
          |
          |; malloc(size: i64) -> *byte
          |; Bump allocator. Aligns size up to 8, advances brk, returns the
          |; old brk. Returns 0 on heap exhaustion. Never reclaims (free is
          |; a no-op) — fine for a one-shot test runner: each test gets a
          |; fresh CPU+memory.
          |;
          |; ABI: only clobbers r1..r4 — r5 is the caller's frame pointer,
          |; r6 the link register, r7 the stack pointer. Touching r5 used
          |; to corrupt the caller's stack-relative addressing.
          |global malloc, func
          |malloc
          |  addi r1, r1, 7         ; size += 7
          |  addi r2, r0, -8        ; r2 = -8 = ~7  (movi rejects negative)
          |  and r1, r1, r2         ; r1 = aligned size
          |  movi r2, BRK_PTR
          |  ldd r3, r2, r0         ; r3 = current brk
          |  beq r3, r0, .first     ; first call: lazy init below
          |  ; Validate brk in [HEAP_START, HEAP_END]. Unsigned compare catches
          |  ; both negative (= huge unsigned) and positive-but-bogus values.
          |  movi r4, HEAP_START
          |  bgu r4, r3, .corrupt   ; HEAP_START > brk → bogus
          |  movi r4, HEAP_END
          |  bgu r3, r4, .corrupt   ; brk > HEAP_END → bogus (incl. negatives)
          |  bra .have
          |.first
          |  movi r3, HEAP_START
          |.have
          |  add r1, r3, r1         ; r1 = new brk (consumes the size in r1)
          |  movi r4, HEAP_END
          |  bgu r1, r4, .oom       ; if new brk > HEAP_END, OOM
          |  std r1, r2, r0         ; brk = new brk
          |  mov r1, r3             ; return old brk
          |  jalr r0, r6
          |.oom
          |  movi r1, 0
          |  jalr r0, r6
          |.corrupt
          |  ; BRK_PTR was clobbered by a previous call. Trap with sentinel 99
          |  ; so the runner reports `trap (code=99)` instead of the usual OOB.
          |  movi r1, 99
          |  trap 1
          |
          |; free(p: *byte) — no-op (bump allocator)
          |global free, func
          |free
          |  jalr r0, r6
          |""".stripMargin
    val programTof =
      try assemble(asm, relocatable = true)
      catch case e: Throwable => return Fail(s"TRISC assembly failed: ${e.getMessage}", outputBuf.toString)
    val wrapperTof =
      try assemble(wrapperAsm, relocatable = true)
      catch case e: Throwable => return Fail(s"TRISC wrapper assembly failed: ${e.getMessage}", outputBuf.toString)
    // Two-pass link: first link the user program TOF (relocatable), then merge
    // with the wrapper at base 0. This mirrors OSKitTestHelpers.runWithBoot and
    // ensures the wrapper's `vectors` segment lands at address 0 even if the
    // user program also declares vectors-shaped data.
    val linkedProgram =
      try Linker.link(Seq(programTof), relocatable = true)
      catch case e: Throwable => return Fail(s"TRISC user-link failed: ${e.getMessage}", outputBuf.toString)
    val linked =
      try Linker.link(Seq(wrapperTof, linkedProgram))
      catch case e: Throwable => return Fail(s"TRISC link failed: ${e.getMessage}", outputBuf.toString)
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = stdoutAddr
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = outputBuf += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    // Optional write-watchpoints (set TRISC_WATCH=<hex_addr>:<width>,...).
    // Common targets: TRISC_WATCH=0xFFF08:8 watches BRK_PTR. The watchpoint
    // skips writes during initial load (before CPU exists) and prints the
    // CPU's current PC to stderr on every match. Several ranges may be set
    // by separating with commas. Wraps RAM with an override only when set.
    var watchCpu: Option[CPU] = None
    val watchRanges: List[(Long, Long)] =
      Option(System.getenv("TRISC_WATCH")).map { spec =>
        spec.split(",").toList.flatMap { r =>
          r.split(":") match
            case Array(addrStr, widthStr) =>
              val addr = java.lang.Long.parseLong(addrStr.stripPrefix("0x"), 16)
              val width = widthStr.toLong
              Some((addr, addr + width))
            case _ => None
        }
      }.getOrElse(Nil)
    val ram: Addressable =
      if watchRanges.nonEmpty then
        new RAM(0, ramSize) {
          private def maybeReport(addr: Long, data: Long, width: String): Unit =
            if watchRanges.exists((lo, hi) => addr >= lo && addr < hi) then
              watchCpu.foreach { c =>
                System.err.println(f"[WATCH] $width 0x$data%x to 0x$addr%x at PC=0x${c.pc.toHexString}")
              }
          override def writeByte(addr: Long, data: Long): Unit =
            maybeReport(addr, data, "stb"); super.writeByte(addr, data)
          override def writeShort(addr: Long, data: Long): Unit =
            maybeReport(addr, data, "sts"); super.writeShort(addr, data)
          override def writeInt(addr: Long, data: Long): Unit =
            maybeReport(addr, data, "stw"); super.writeInt(addr, data)
          override def writeLong(addr: Long, data: Long): Unit =
            maybeReport(addr, data, "std"); super.writeLong(addr, data)
        }
      else new RAM(0, ramSize)
    val mem = new Memory("Memory", ram, stdout)
    try linked.load(mem)
    catch case e: Throwable => return Fail(s"TRISC load failed: ${e.getMessage}", outputBuf.toString)
    if System.getenv("TRISC_DUMP_DISASM") != null then
      // Build PC -> symbol map from the linked TOF.
      val syms: Map[Long, String] =
        (for seg <- linked.segments; sym <- seg.symbols
         yield (seg.org + sym.offset) -> sym.name).toMap
      val buf = new StringBuilder
      val tmpCpu = new CPU(mem)
      for seg <- linked.segments do
        val end = seg.org + seg.chunks.map {
          case TOF.DataChunk(d)  => d.length.toLong
          case TOF.ResChunk(s)   => s
          case TOF.CommentChunk(_) => 0L
        }.sum
        buf ++= s"# segment ${seg.name} org=0x${seg.org.toHexString} end=0x${end.toHexString}\n"
        var addr = seg.org
        while addr < end do
          syms.get(addr).foreach(n => buf ++= s"$n:\n")
          val w = mem.readShortUnsigned(addr)
          tmpCpu.pc = addr + 2
          val text = Decode(w).disassemble(tmpCpu)
          buf ++= f"  0x$addr%04x  $text%n"
          addr += 2
      java.nio.file.Files.writeString(
        java.nio.file.Paths.get(s"/tmp/trisc_${t.unitName.replace("/", "_")}.dis"), buf.toString)
    val cpu = new CPU(mem) { limit = 50_000_000; quiet = System.getenv("TRISC_TRACE") == null }
    watchCpu = Some(cpu)
    try
      cpu.reset()
      cpu.run()
    catch case e: Throwable =>
      return Fail(s"TRISC runtime error: ${e.getClass.getSimpleName}: ${e.getMessage} at PC=0x${cpu.pc.toHexString}", outputBuf.toString)
    val captured = outputBuf.toString
    val flag = mem.readByte(panicFlagAddr) & 0xFF
    if System.getenv("TRISC_TRACE") != null then
      System.err.println(s"TRISC_TRACE: ${t.fn.name} state=${cpu.state} pc=0x${cpu.pc.toHexString} r1=${cpu.r(1).read} flag=$flag")
    def readFaultRegs(): String =
      try
        val r1v = mem.readLong(faultRegsAddr)
        val r2v = mem.readLong(faultRegsAddr + 8)
        val r3v = mem.readLong(faultRegsAddr + 16)
        val r4v = mem.readLong(faultRegsAddr + 24)
        val r5v = mem.readLong(faultRegsAddr + 32)
        val r6v = mem.readLong(faultRegsAddr + 40)
        f"\n      regs: r1=0x$r1v%x r2=0x$r2v%x r3=0x$r3v%x r4=0x$r4v%x r5=0x$r5v%x r6=0x$r6v%x"
      catch case _: Throwable => ""
    def disasmWindow(faultPc: Long): String =
      if faultPc < 0 || faultPc + 4 >= ramSize then ""
      else
        val rng = if System.getenv("TRISC_TRACE") != null then -32 to 8 by 2 else -4 to 4 by 2
        val window = rng.flatMap { d =>
          val p = faultPc + d
          if p < 0 || p + 1 >= ramSize then None
          else
            try
              val w = mem.readShortUnsigned(p)
              val mark = if d == 0 then " <-- FAULT" else ""
              Some(f"      0x$p%04x: ${Decode(w).disassemble(cpu)}$mark")
            catch case _: Throwable => None
        }
        if window.isEmpty then "" else "\n" + window.mkString("\n")
    cpu.state match
      case State.Halt =>
        flag match
          case 0 =>
            if t.shouldPanic then Fail("expected panic, got normal return", captured) else Pass
          case 222 =>
            val savedPc = mem.readLong(faultPcAddr)
            // CPU advances pc *before* executing the instruction, so the saved
            // PC on the exception frame is one past the faulting instruction.
            val faultPc = savedPc - 2
            val regs = readFaultRegs()
            val ctx = disasmWindow(faultPc)
            Fail(s"CPU fault at PC=0x${faultPc.toHexString} (saved PC=0x${savedPc.toHexString})$regs$ctx", captured)
          case code =>
            val codeName = code match
              case 1 => "out-of-bounds"
              case 2 => "null-deref"
              case 3 => "abort"
              case 4 => "assert/panic"
              case 5 => "divide-by-zero"
              case 99 => "brk-corrupt (test-runner sentinel)"
              case _ => s"trap (code=$code)"
            // For trap-99 (brk-corruption diagnostics) and TRISC_TRACE always
            // append the saved-PC + reg dump so we can see *which* trap fired
            // in the user code (panic_isr now saves the same exception frame
            // info that fault_isr does).
            val showDiag = code == 99 || System.getenv("TRISC_TRACE") != null
            val diag =
              if !showDiag then ""
              else
                val savedPc = try mem.readLong(faultPcAddr) catch case _: Throwable => 0L
                val faultPc = savedPc - 2
                s"\n      at PC=0x${faultPc.toHexString} (saved=0x${savedPc.toHexString})${readFaultRegs()}${disasmWindow(faultPc)}"
            if t.shouldPanic then Pass
            else Fail(s"panic ($codeName)$diag", captured)
      case State.Run =>
        Fail(s"TRISC test timed out (cycle limit reached) at PC=0x${cpu.pc.toHexString}", captured)
      case other =>
        Fail(s"unexpected CPU state: $other at PC=0x${cpu.pc.toHexString}", captured)

  /** Pre-built freestanding RV runtime objects for one xlen. Caches the paths
    * to start.o, sbi.o, libc.o, llvm_intrinsics.o (rv32 only) and the linker
    * script, all under one tmp dir owned for the lifetime of this CLI run. */
  private case class RiscVRuntime(
      xlen: Int,
      objs: Seq[String],   // absolute paths to .o files, in link order
      linkScript: String,  // absolute path to link.ld
      workDir: String,     // tmp dir for build artifacts (reused for elfs)
  )

  /** Defaults match the chunk-2 toolchain pin on macOS+Homebrew. Each is
    * overridable via env var so a Linux host or non-standard install can run
    * the same tests without code changes. */
  private val rvClang: String = Option(System.getenv("SYSL_RV_CLANG"))
    .getOrElse("/opt/homebrew/opt/llvm/bin/clang")
  private val rvRuntimeDir: String = Option(System.getenv("SYSL_RV_RUNTIME"))
    .getOrElse("sysl/runtime/rv")
  private def rvQemu(xlen: Int): String =
    val envKey = if xlen == 64 then "SYSL_RV_QEMU64" else "SYSL_RV_QEMU32"
    Option(System.getenv(envKey)).getOrElse(s"/opt/homebrew/bin/qemu-system-riscv$xlen")

  /** Build the freestanding RV runtime objects once per (session, xlen). The
    * compile is small (~0.5s/file), but std/ has hundreds of tests — caching
    * eliminates a per-test 2s setup cost. Returns the linker script's path
    * and a sequence of .o paths in deterministic link order. */
  private def buildRiscVRuntime(xlen: Int): Either[String, RiscVRuntime] =
    val (target, march, mabi, linkLd) = xlen match
      case 64 => ("riscv64-unknown-elf", "rv64gc", "lp64d", "link64.ld")
      case 32 => ("riscv32-unknown-elf", "rv32gc", "ilp32d", "link32.ld")
      case _  => return Left(s"unsupported xlen $xlen (only 32/64)")
    val runtimeDir = java.nio.file.Paths.get(rvRuntimeDir).toAbsolutePath
    if !java.nio.file.Files.exists(runtimeDir) then
      return Left(s"runtime dir $runtimeDir not found (set SYSL_RV_RUNTIME or run from repo root)")
    val sources = Seq("start.S", "sbi.c", "libc.c") ++
      (if xlen == 32 then Seq("llvm_intrinsics.c") else Seq.empty)
    val linkScriptPath = runtimeDir.resolve(linkLd).toString
    if !java.nio.file.Files.exists(java.nio.file.Paths.get(linkScriptPath)) then
      return Left(s"missing linker script $linkScriptPath")
    val workDir = java.nio.file.Files.createTempDirectory(s"sysl-rv${xlen}-rt-")
    val baseClangArgs = Seq(
      rvClang,
      "-target", target,
      s"-march=$march", s"-mabi=$mabi",
      "-mcmodel=medany",
      "-ffreestanding", "-nostdlib", "-static",
      "-c", "-O2",
    )
    val objs = scala.collection.mutable.ArrayBuffer.empty[String]
    // Compile each runtime source. The inner `?` short-circuits on the first
    // failure; we avoid non-local return-from-loop, which Scala 3 deprecates.
    def compileOne(src: String): Either[String, String] =
      val srcPath = runtimeDir.resolve(src).toString
      if !java.nio.file.Files.exists(java.nio.file.Paths.get(srcPath)) then
        Left(s"missing runtime source $srcPath")
      else
        val objPath = workDir.resolve(src.replace(".c", ".o").replace(".S", ".o")).toString
        val log = new StringBuilder
        val plog = scala.sys.process.ProcessLogger(
          line => log.append(line).append('\n'),
          line => log.append(line).append('\n'),
        )
        try
          val exit = scala.sys.process.Process(baseClangArgs ++ Seq(srcPath, "-o", objPath)).!(plog)
          if exit != 0 then Left(s"clang failed on $src (exit $exit): ${log.toString.trim}")
          else Right(objPath)
        catch case e: Throwable => Left(s"clang invoke failed on $src: ${e.getMessage}")
    val firstFailure = sources.iterator
      .map(src => (src, compileOne(src)))
      .find(_._2.isLeft)
    firstFailure match
      case Some((_, Left(err))) => Left(err)
      case _ =>
        for src <- sources do
          objs += workDir.resolve(src.replace(".c", ".o").replace(".S", ".o")).toString
        Right(RiscVRuntime(xlen, objs.toSeq, linkScriptPath, workDir.toString))

  /** Compile a unit's scoped TProgram to an LLVM IR file targeted for the
    * given xlen, then `clang -c` it into a `program.o`. The .o is cached per
    * (xlen, unitName); the per-test cost is then just shim + link + qemu. */
  private def compileUnitToRiscVProgramObj(xlen: Int, program: TProgram, unitName: String): Either[String, String] =
    val (target, march, mabi) = xlen match
      case 64 => ("riscv64-unknown-elf", "rv64gc", "lp64d")
      case 32 => ("riscv32-unknown-elf", "rv32gc", "ilp32d")
      case _  => return Left(s"unsupported xlen $xlen")
    val codegen = new SyslLLVMCodegen(s"riscv$xlen-elf")
    val ir = try codegen.generate(program)
             catch case e: Throwable => return Left(s"IR codegen failed: ${e.getMessage}")
    val workDir = java.nio.file.Files.createTempDirectory(s"sysl-rv${xlen}-unit-")
    val unitKey = unitName.replace("/", "_").replace(".", "_")
    val irPath = workDir.resolve(s"$unitKey.ll")
    java.nio.file.Files.writeString(irPath, ir)
    val objPath = workDir.resolve(s"$unitKey.o").toString
    val log = new StringBuilder
    val plog = scala.sys.process.ProcessLogger(
      line => log.append(line).append('\n'),
      line => log.append(line).append('\n'),
    )
    val args = Seq(
      rvClang,
      "-target", target,
      s"-march=$march", s"-mabi=$mabi",
      "-mcmodel=medany",
      "-ffreestanding", "-nostdlib", "-static",
      "-c", "-O2", "-w",
      irPath.toString, "-o", objPath,
    )
    val exit =
      try scala.sys.process.Process(args).!(plog)
      catch case e: Throwable => return Left(s"clang invoke failed: ${e.getMessage}")
    if exit != 0 then Left(s"clang failed (exit $exit): ${log.toString.trim}")
    else Right(objPath)

  /** Compile + run one test on the RV LLVM backend under qemu-system-riscv*.
    *
    * Strategy: cache a pre-built freestanding RV runtime (start/sbi/libc, and
    * llvm_intrinsics on rv32) plus the per-unit `program.o`. Per test we only
    * write a tiny C shim that calls the named test function, link, and boot
    * the ELF under qemu with stdio serial. The shim's `main` returns 0 if the
    * test returned cleanly — `_start` then calls SBI SystemReset(0,0).
    *
    * The pass/fail signal: sysl panic/assert/range_fail all `write(2, ...)` a
    * marked prefix to stdout (the RV runtime routes every fd to the SBI
    * console, so stderr lands in our capture) before calling `abort()`. We
    * detect any of those prefixes to conclude a panic regardless of the qemu
    * exit code — SBI SystemReset's "reason" field is not reliably surfaced
    * across qemu versions, but the captured serial output is.
    *
    * QEMU's `-machine virt -bios default` boots OpenSBI which prints a banner
    * before jumping to our kernel; we strip everything up to and including
    * the OpenSBI footer line so the captured "user" output matches what other
    * backends report.
    *
    * `RV_DUMP_IR=1` saves the IR + ELF in /tmp/sysl_rv{xlen}_<unit>/ for
    * inspection. `RV_TRACE=1` prints a one-line per-test summary plus the raw
    * qemu output (banner included) to stderr.
    */
  private def runOneRiscV(
      xlen: Int,
      program: TProgram,
      t: DiscoveredTest,
      objCache: scala.collection.mutable.Map[(Int, String), Either[String, String]],
      runtimeCache: scala.collection.mutable.Map[Int, Either[String, RiscVRuntime]],
  ): TestOutcome =
    val runtime = runtimeCache.getOrElseUpdate(xlen, buildRiscVRuntime(xlen)) match
      case Right(r)  => r
      case Left(err) => return Fail(s"RV runtime build failed: $err")
    val programObj = objCache.getOrElseUpdate((xlen, t.unitName),
      compileUnitToRiscVProgramObj(xlen, program, t.unitName)) match
      case Right(p)  => p
      case Left(err) => return Fail(s"RV unit compile failed: $err")

    // Per-test shim: call the target test fn, return 0. Linker drags only the
    // exported test fn out of program.o thanks to -nostdlib + entry resolution.
    val workDir = java.nio.file.Paths.get(runtime.workDir)
    val shim =
      s"""|extern void ${t.fn.name}(void);
          |int main(void) {
          |    ${t.fn.name}();
          |    return 0;
          |}
          |""".stripMargin
    val shimC = workDir.resolve(s"shim_${t.fn.name}.c").toString
    java.nio.file.Files.writeString(java.nio.file.Paths.get(shimC), shim)
    val elf = workDir.resolve(s"test_${t.fn.name}.elf").toString
    val (target, march, mabi) = xlen match
      case 64 => ("riscv64-unknown-elf", "rv64gc", "lp64d")
      case _  => ("riscv32-unknown-elf", "rv32gc", "ilp32d")
    val linkArgs = Seq(
      rvClang,
      "-target", target,
      s"-march=$march", s"-mabi=$mabi",
      "-mcmodel=medany",
      "-ffreestanding", "-nostdlib", "-static",
      "-fuse-ld=lld",
      "-T", runtime.linkScript,
      "-O2", "-w",
    ) ++ runtime.objs ++ Seq(programObj, shimC, "-o", elf)
    val linkLog = new StringBuilder
    val linkLogger = scala.sys.process.ProcessLogger(
      line => linkLog.append(line).append('\n'),
      line => linkLog.append(line).append('\n'),
    )
    val linkExit =
      try scala.sys.process.Process(linkArgs).!(linkLogger)
      catch case e: Throwable => return Fail(s"RV link invoke failed: ${e.getMessage}")
    if linkExit != 0 then
      return Fail(s"RV link failed (exit $linkExit): ${linkLog.toString.trim}")

    if System.getenv("RV_DUMP_IR") != null then
      val keepDir = java.nio.file.Paths.get(s"/tmp/sysl_rv${xlen}_${t.unitName.replace("/", "_")}")
      java.nio.file.Files.createDirectories(keepDir)
      java.nio.file.Files.copy(java.nio.file.Paths.get(programObj),
        keepDir.resolve(s"${t.unitName.replace("/", "_")}.o"),
        java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      java.nio.file.Files.copy(java.nio.file.Paths.get(elf),
        keepDir.resolve(s"test_${t.fn.name}.elf"),
        java.nio.file.StandardCopyOption.REPLACE_EXISTING)

    // Boot under qemu. `-bios default` runs OpenSBI which then enters our ELF
    // in S-mode. `-no-reboot` makes SystemReset.SHUTDOWN actually exit. The
    // timeout cap prevents a runaway test from hanging the suite — std/ tests
    // routinely finish in <100ms wall, so 15s is a generous ceiling.
    val outBuf = new StringBuilder
    val errBuf = new StringBuilder
    val qemuLogger = scala.sys.process.ProcessLogger(
      line => outBuf.append(line).append('\n'),
      line => errBuf.append(line).append('\n'),
    )
    val qemuArgs = Seq(
      rvQemu(xlen),
      "-machine", "virt",
      "-bios", "default",
      "-kernel", elf,
      "-nographic",
      "-serial", "mon:stdio",
      "-no-reboot",
      "-display", "none",
    )
    val qemuProc =
      try scala.sys.process.Process(qemuArgs).run(qemuLogger)
      catch case e: Throwable => return Fail(s"RV qemu spawn failed: ${e.getMessage}")
    val qemuExit: Int =
      val deadline = System.currentTimeMillis() + 15000L
      var done = false
      var code = -1
      while !done && System.currentTimeMillis() < deadline do
        if !qemuProc.isAlive() then
          code = qemuProc.exitValue()
          done = true
        else Thread.sleep(20)
      if !done then
        qemuProc.destroy()
        Thread.sleep(100)
        if qemuProc.isAlive() then qemuProc.destroy()
        return Fail(s"RV qemu timed out after 15s", outBuf.toString)
      code

    val rawStdout = outBuf.toString
    // Strip OpenSBI banner: everything up to and including the last banner line
    // (a row of `=` separators ends each banner section, with the final block
    // being the boot HART summary). Match the empirically observed footer.
    val stripped = stripOpenSbiBanner(rawStdout)
    if System.getenv("RV_TRACE") != null then
      System.err.println(s"RV_TRACE: ${t.fn.name} xlen=$xlen exit=$qemuExit")
      System.err.println(s"RV_TRACE: raw stdout:\n$rawStdout")
      System.err.println(s"RV_TRACE: stripped:\n$stripped")

    val panicMsg = panicMarker(stripped)
    if panicMsg.isDefined then
      if t.shouldPanic then
        t.expectedMsg match
          case Some(substr) if !panicMsg.get.contains(substr) =>
            Fail(s"panic message did not contain '$substr' (got: ${panicMsg.get})", stripped)
          case _ => Pass
      else Fail(s"panic: ${panicMsg.get}", stripped)
    else if qemuExit == 0 then
      if t.shouldPanic then Fail("expected panic, got normal return", stripped) else Pass
    else
      // No panic prefix found but qemu exited nonzero — typically a CPU fault
      // (illegal instruction, page fault) that bypassed our panic ISR. Report
      // both the exit code and any qemu stderr so the user has something to
      // grep on.
      val errTail = errBuf.toString.trim
      val errPart = if errTail.isEmpty then "" else s" (qemu stderr: $errTail)"
      Fail(s"RV qemu exited $qemuExit with no panic marker$errPart", stripped)

  // ------------------------------------------------------------------
  // wasm32-wasi backend — mirror of the RV runner with the qemu/SBI bits
  // swapped for wasmtime/WASI. Reuses the chunk-1 codegen (target
  // "wasm32") and the chunk-2 freestanding runtime under
  // sysl/runtime/wasm/. No linker script; no banner-stripping (wasmtime
  // stdout is clean). The shape — runtime build + per-unit obj cache +
  // per-test shim/link/run — is intentionally identical to the RV path
  // so the same expectations (panicMarker, exit-code rules) carry over.
  // ------------------------------------------------------------------

  /** Pre-built freestanding wasm32 runtime objects. Caches the paths to
    * imports.o, libc.o, llvm_intrinsics.o under one tmp dir owned for
    * the lifetime of this CLI run. No linker script (wasm-ld lays out
    * linear memory itself), no xlen (wasm32 is the only target). */
  private case class WasmRuntime(
      objs: Seq[String],   // absolute paths to .o files, in link order
      workDir: String,     // tmp dir for build artifacts (reused for wasms)
  )

  /** Defaults match the chunk-2 toolchain pin on macOS+Homebrew. Each is
    * overridable via env var so a Linux host or non-standard install can
    * run the same tests without code changes. */
  private val wasmClang: String = Option(System.getenv("SYSL_WASM_CLANG"))
    .getOrElse("/opt/homebrew/opt/llvm/bin/clang")
  private val wasmRuntimeDir: String = Option(System.getenv("SYSL_WASM_RUNTIME"))
    .getOrElse("sysl/runtime/wasm")
  private val wasmtimePath: String = Option(System.getenv("SYSL_WASMTIME"))
    .getOrElse("/opt/homebrew/bin/wasmtime")

  /** Build the freestanding wasm runtime objects once per session. Each
    * compile is ~150ms; std/ has hundreds of tests so caching is worth
    * it even though wasm is faster than RV. clang+wasm-ld pick the right
    * driver automatically from `--target=wasm32-unknown-wasi`. */
  private def buildWasmRuntime(): Either[String, WasmRuntime] =
    val runtimeDir = java.nio.file.Paths.get(wasmRuntimeDir).toAbsolutePath
    if !java.nio.file.Files.exists(runtimeDir) then
      return Left(s"runtime dir $runtimeDir not found (set SYSL_WASM_RUNTIME or run from repo root)")
    val sources = Seq("imports.c", "libc.c", "llvm_intrinsics.c")
    val workDir = java.nio.file.Files.createTempDirectory("sysl-wasm-rt-")
    val baseClangArgs = Seq(
      wasmClang,
      "--target=wasm32-unknown-wasi",
      "-ffreestanding", "-nostdlib",
      "-c", "-O2",
    )
    def compileOne(src: String): Either[String, String] =
      val srcPath = runtimeDir.resolve(src).toString
      if !java.nio.file.Files.exists(java.nio.file.Paths.get(srcPath)) then
        Left(s"missing runtime source $srcPath")
      else
        val objPath = workDir.resolve(src.replace(".c", ".o")).toString
        val log = new StringBuilder
        val plog = scala.sys.process.ProcessLogger(
          line => log.append(line).append('\n'),
          line => log.append(line).append('\n'),
        )
        try
          val exit = scala.sys.process.Process(baseClangArgs ++ Seq(srcPath, "-o", objPath)).!(plog)
          if exit != 0 then Left(s"clang failed on $src (exit $exit): ${log.toString.trim}")
          else Right(objPath)
        catch case e: Throwable => Left(s"clang invoke failed on $src: ${e.getMessage}")
    val firstFailure = sources.iterator
      .map(src => (src, compileOne(src)))
      .find(_._2.isLeft)
    firstFailure match
      case Some((_, Left(err))) => Left(err)
      case _ =>
        val objs = sources.map(src => workDir.resolve(src.replace(".c", ".o")).toString)
        Right(WasmRuntime(objs, workDir.toString))

  /** Compile a unit's scoped TProgram to wasm32 LLVM IR, then `clang -c`
    * it to a `program.o`. Cached per unit name; per-test cost is then
    * just shim + link + wasmtime. */
  private def compileUnitToWasmProgramObj(program: TProgram, unitName: String): Either[String, String] =
    val codegen = new SyslLLVMCodegen("wasm32")
    val ir = try codegen.generate(program)
             catch case e: Throwable => return Left(s"IR codegen failed: ${e.getMessage}")
    val workDir = java.nio.file.Files.createTempDirectory("sysl-wasm-unit-")
    val unitKey = unitName.replace("/", "_").replace(".", "_")
    val irPath = workDir.resolve(s"$unitKey.ll")
    java.nio.file.Files.writeString(irPath, ir)
    val objPath = workDir.resolve(s"$unitKey.o").toString
    val log = new StringBuilder
    val plog = scala.sys.process.ProcessLogger(
      line => log.append(line).append('\n'),
      line => log.append(line).append('\n'),
    )
    val args = Seq(
      wasmClang,
      "--target=wasm32-unknown-wasi",
      "-ffreestanding", "-nostdlib",
      "-c", "-O2", "-w",
      irPath.toString, "-o", objPath,
    )
    val exit =
      try scala.sys.process.Process(args).!(plog)
      catch case e: Throwable => return Left(s"clang invoke failed: ${e.getMessage}")
    if exit != 0 then Left(s"clang failed (exit $exit): ${log.toString.trim}")
    else Right(objPath)

  /** Compile + run one test on the wasm32 LLVM backend under wasmtime.
    *
    * Mirrors `runOneRiscV` step for step: cache the runtime objects and
    * the per-unit program obj, write a tiny C shim that calls the named
    * test function, link to a `.wasm`, run under wasmtime, capture
    * stdout+stderr (we merge — see the pitfalls note in the roadmap
    * memo), parse the exit code and panic marker.
    *
    * `WASM_DUMP_IR=1` saves the IR + wasm in
    * /tmp/sysl_wasm_<unit>/ for inspection. `WASM_TRACE=1` prints a
    * one-line per-test summary plus the raw wasmtime output to stderr.
    */
  private def runOneWasm(
      program: TProgram,
      t: DiscoveredTest,
      objCache: scala.collection.mutable.Map[String, Either[String, String]],
      runtimeCache: scala.collection.mutable.Map[Unit, Either[String, WasmRuntime]],
  ): TestOutcome =
    val runtime = runtimeCache.getOrElseUpdate((), buildWasmRuntime()) match
      case Right(r)  => r
      case Left(err) => return Fail(s"wasm runtime build failed: $err")
    val programObj = objCache.getOrElseUpdate(t.unitName,
      compileUnitToWasmProgramObj(program, t.unitName)) match
      case Right(p)  => p
      case Left(err) => return Fail(s"wasm unit compile failed: $err")

    val workDir = java.nio.file.Paths.get(runtime.workDir)
    val shim =
      s"""|extern void ${t.fn.name}(void);
          |int main(void) {
          |    ${t.fn.name}();
          |    return 0;
          |}
          |""".stripMargin
    val shimC = workDir.resolve(s"shim_${t.fn.name}.c").toString
    java.nio.file.Files.writeString(java.nio.file.Paths.get(shimC), shim)
    val wasmOut = workDir.resolve(s"test_${t.fn.name}.wasm").toString
    val linkArgs = Seq(
      wasmClang,
      "--target=wasm32-unknown-wasi",
      "-ffreestanding", "-nostdlib",
      "-Wl,--no-entry", "-Wl,--export=_start", "-Wl,--allow-undefined",
      "-O2", "-w",
    ) ++ runtime.objs ++ Seq(programObj, shimC, "-o", wasmOut)
    val linkLog = new StringBuilder
    val linkLogger = scala.sys.process.ProcessLogger(
      line => linkLog.append(line).append('\n'),
      line => linkLog.append(line).append('\n'),
    )
    val linkExit =
      try scala.sys.process.Process(linkArgs).!(linkLogger)
      catch case e: Throwable => return Fail(s"wasm link invoke failed: ${e.getMessage}")
    if linkExit != 0 then
      return Fail(s"wasm link failed (exit $linkExit): ${linkLog.toString.trim}")

    if System.getenv("WASM_DUMP_IR") != null then
      val keepDir = java.nio.file.Paths.get(s"/tmp/sysl_wasm_${t.unitName.replace("/", "_")}")
      java.nio.file.Files.createDirectories(keepDir)
      java.nio.file.Files.copy(java.nio.file.Paths.get(programObj),
        keepDir.resolve(s"${t.unitName.replace("/", "_")}.o"),
        java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      java.nio.file.Files.copy(java.nio.file.Paths.get(wasmOut),
        keepDir.resolve(s"test_${t.fn.name}.wasm"),
        java.nio.file.StandardCopyOption.REPLACE_EXISTING)

    // Run under wasmtime. Our libc routes write(2, ...) through the same
    // putchar path as fd=1 (see chunk-2 roadmap pitfalls), so panic text
    // can land on either stream depending on host buffering. Merge both
    // captures into one blob — `panicMarker` is a substring search so
    // order doesn't matter, and the rv runner does the same thing
    // (`stripped` already contains everything that arrived on serial).
    val outBuf = new StringBuilder
    val errBuf = new StringBuilder
    val wmLogger = scala.sys.process.ProcessLogger(
      line => outBuf.append(line).append('\n'),
      line => errBuf.append(line).append('\n'),
    )
    val wmArgs = Seq(wasmtimePath, "run", wasmOut)
    val wmProc =
      try scala.sys.process.Process(wmArgs).run(wmLogger)
      catch case e: Throwable => return Fail(s"wasmtime spawn failed: ${e.getMessage}")
    val wmExit: Int =
      val deadline = System.currentTimeMillis() + 15000L
      var done = false
      var code = -1
      while !done && System.currentTimeMillis() < deadline do
        if !wmProc.isAlive() then
          code = wmProc.exitValue()
          done = true
        else Thread.sleep(20)
      if !done then
        wmProc.destroy()
        Thread.sleep(100)
        if wmProc.isAlive() then wmProc.destroy()
        return Fail(s"wasmtime timed out after 15s", outBuf.toString + errBuf.toString)
      code

    val captured = outBuf.toString + errBuf.toString
    if System.getenv("WASM_TRACE") != null then
      System.err.println(s"WASM_TRACE: ${t.fn.name} exit=$wmExit")
      System.err.println(s"WASM_TRACE: stdout:\n${outBuf.toString}")
      System.err.println(s"WASM_TRACE: stderr:\n${errBuf.toString}")

    val panicMsg = panicMarker(captured)
    if panicMsg.isDefined then
      if t.shouldPanic then
        t.expectedMsg match
          case Some(substr) if !panicMsg.get.contains(substr) =>
            Fail(s"panic message did not contain '$substr' (got: ${panicMsg.get})", captured)
          case _ => Pass
      else Fail(s"panic: ${panicMsg.get}", captured)
    else if wmExit == 0 then
      if t.shouldPanic then Fail("expected panic, got normal return", captured) else Pass
    else
      // No panic prefix found but wasmtime exited nonzero — usually a
      // wasm trap (unreachable, OOB memory access) caught by the host.
      // Surface both the exit code and the captured streams so the user
      // can grep for the trap reason wasmtime printed.
      Fail(s"wasmtime exited $wmExit with no panic marker", captured)

  /** Strip the OpenSBI v1.x banner from a captured stdout. The banner ends
    * with a "Domain0 Next Boot HART" / footer block followed by a blank line
    * before the kernel's first write. We split on the last empty line that
    * follows an OpenSBI line — robust against version skew in line wording. */
  private def stripOpenSbiBanner(raw: String): String =
    val lines = raw.linesIterator.toVector
    // Find the highest index of any OpenSBI banner line; anything after it
    // (skipping its trailing blank) is kernel output.
    val sbiIdx = lines.lastIndexWhere(l =>
      l.startsWith("OpenSBI") || l.contains("OpenSBI") ||
      l.startsWith("Domain") || l.startsWith("Boot HART") ||
      l.startsWith("Platform Name") || l.startsWith("Firmware") ||
      l.matches("^\\s*_+\\s*$") || l.matches("^\\s*\\|.*\\|\\s*$"))
    if sbiIdx < 0 then raw
    else
      val tail = lines.drop(sbiIdx + 1).dropWhile(_.trim.isEmpty)
      tail.mkString("\n") + (if tail.nonEmpty then "\n" else "")

  /** Look for any of the panic-message prefixes emitted by the LLVM prelude
    * before its `abort()` call. Returns the extracted message body if any
    * marker is found, else None.
    *
    * **Why substring, not line-prefix:** on hosted backends panic goes to
    * stderr while user prints go to stdout, so the runner sees the panic
    * line cleanly. Under qemu-system-riscv* the SBI console is one stream,
    * so a test that prints args via `print(x)` (no newline) immediately
    * before panicking ends up with the panic message glued onto the previous
    * line — e.g. `12panic: mismatch: got 1, want 2`. The markers are
    * distinctive enough that substring search has no realistic false-positive
    * risk and matches both layouts. All three markers terminate with a
    * newline but `linesIterator` already strips that. */
  private def panicMarker(out: String): Option[String] =
    val markers = List("panic: ", "assertion failed: ", "range check failed: ")
    out.linesIterator
      .flatMap { l =>
        markers.iterator
          .map(m => (m, l.indexOf(m)))
          .collectFirst { case (m, i) if i >= 0 => l.substring(i + m.length) }
      }
      .nextOption()

  private def executeTest(cmd: TestCommand, lockMode: LockMode): Unit =
    if cmd.backend == "all" then
      System.err.println(s"error: backend 'all' not yet implemented (use 'interpreter', 'llvm-host', 'svm-host', 'trisc', 'riscv64', 'riscv32', or 'wasm32')")
      throw CliError("unsupported backend")

    val resolved = resolveDeps(cmd.inputs, ignoreLockPins = false, lockMode) match
      case Right(r) => r
      case Left(msg) => fail(s"error: $msg")
    // Workspace-level resolve already enumerates every member + transitive
    // path dep, so a single lock write at the workspace root captures the
    // whole graph (matches Cargo's "one Cargo.lock per workspace" rule).
    applyLockMode(resolved, lockMode)

    SyslResolver.workspaceMemberDirs(io, resolved) match
      case Some(members) =>
        // Workspace mode: each member is its own compilation. Re-resolve and
        // run tests per member so module keys don't collide between members
        // and each member sees only its own [dependencies].
        val root = resolved.projectRoot.get
        println(s"workspace at $root: testing ${members.size} member(s)")
        var totalPassed = 0
        var totalFailed = 0
        var totalSkipped = 0
        val workspaceStart = System.nanoTime()
        for member <- members do
          println(s"\n— member: $member")
          val perMember = resolveDeps(Seq(member), ignoreLockPins = false, lockMode) match
            case Right(r) => r
            case Left(msg) => fail(s"error in workspace member $member: $msg")
          val (p, f, s) = runProjectTests(cmd, Seq(member), perMember)
          totalPassed += p
          totalFailed += f
          totalSkipped += s
        val workspaceMs = (System.nanoTime() - workspaceStart) / 1e6
        println(f"\nworkspace total: $totalPassed passed, $totalFailed failed, $totalSkipped skipped — $workspaceMs%.1fms")
        if totalFailed > 0 then throw CliError(s"$totalFailed test(s) failed")
      case None =>
        val (_, failed, _) = runProjectTests(cmd, cmd.inputs, resolved)
        if failed > 0 then throw CliError(s"$failed test(s) failed")

  /** Run the test discovery + execution pipeline for a single project (or for
   *  legacy "no project root" inputs). Returns (passed, failed, skipped) so
   *  the workspace dispatcher can aggregate. Throws CliError only on hard
   *  errors (missing input file, resolver failure) — failing tests are
   *  reported via the return tuple so the caller decides how to surface
   *  workspace-wide totals.
   */
  private def runProjectTests(
      cmd: TestCommand,
      inputs: Seq[String],
      resolved: ResolvedDeps,
  ): (Int, Int, Int) =
    // baseDirs always include "." so that legacy invocations from inside the
    // trisc repo (sub-dir tests with no sysl.toml in scope) keep working.
    // When a project + path deps are in scope, we add each dep's project root
    // to baseDirs so import-based source discovery can find dep modules.
    val baseDirs = (List(".") ++ resolved.searchRoots).distinct
    val initialSources: Map[String, String] =
      inputs.flatMap { p =>
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

    // Scope: only run tests defined in the user-supplied paths. The driver
    // also pulls in transitively-imported modules so type-check + codegen
    // succeed, but their `#test` functions are NOT in the user's intent —
    // running them muddies failure attribution and inflates test counts
    // (e.g. parsyl getting std.bytes / std.utf8 / std.builder for free
    // just by importing them).
    val inScopeUnits = initialSources.keys.toSet
    val inScopeDiscovered = discovered.filter(t => inScopeUnits(t.unitName))

    val filtered = cmd.filter match
      case None => inScopeDiscovered
      case Some(pat) => inScopeDiscovered.filter(t =>
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
    // The RV path needs two caches: pre-built runtime object files (per xlen,
    // built once per session) and per-unit `program.o` files (the IR compiled
    // for the right target, re-linked per test against a tiny shim). Lazy so
    // the runtime build only fires when at least one RV test runs.
    val rvRuntimeCache = scala.collection.mutable.Map[Int, Either[String, RiscVRuntime]]()
    val rvProgramObjCache = scala.collection.mutable.Map[(Int, String), Either[String, String]]()
    // wasm32: single runtime (no xlen variant) and per-unit obj cache.
    val wasmRuntimeCache = scala.collection.mutable.Map[Unit, Either[String, WasmRuntime]]()
    val wasmProgramObjCache = scala.collection.mutable.Map[String, Either[String, String]]()

    for t <- filtered if !stop do
      if t.unitName != currentUnit then
        currentUnit = t.unitName
        println(currentUnit)
      val start = System.nanoTime()
      val outcome = cmd.backend match
        case "llvm-host" => runOneLLVM(programFor(t.unitName), t, llvmBinCache)
        case "svm-host"  => runOneSVM(programFor(t.unitName), t)
        case "trisc"     => runOneTRISC(programFor(t.unitName), t)
        case "riscv64"   => runOneRiscV(64, programFor(t.unitName), t, rvProgramObjCache, rvRuntimeCache)
        case "riscv32"   => runOneRiscV(32, programFor(t.unitName), t, rvProgramObjCache, rvRuntimeCache)
        case "wasm32"    => runOneWasm(programFor(t.unitName), t, wasmProgramObjCache, wasmRuntimeCache)
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
    // `skipped` reports tests excluded by `--filter`; the scope filter above
    // is silent (the user didn't ask for those tests in the first place).
    val skipped = inScopeDiscovered.size - filtered.size
    println(f"\n$passed passed, $failed failed, $skipped skipped — $totalMs%.1fms")
    (passed, failed, skipped)

  /** `sysl fetch [root]` — resolve the dep graph rooted at `root` (or cwd) and
   *  write `sysl.lock` next to the manifest if it would change. No code is
   *  compiled, no tests run. For git deps, honors any sha pins in an existing
   *  lock so a re-run with no manifest changes does no network IO. */
  private def executeFetch(cmd: FetchCommand, lockMode: LockMode): Unit =
    val resolved = resolveDeps(Seq(cmd.root), ignoreLockPins = false, lockMode) match
      case Right(r) => r
      case Left(msg) => fail(s"error: $msg")
    resolved.projectRoot match
      case None =>
        println(s"sysl: no sysl.toml in scope at ${cmd.root}; nothing to lock")
      case Some(root) => lockMode match
        case LockMode.Default =>
          SyslLock.writeIfChanged(io, resolved) match
            case Some(true)  => println(s"sysl: wrote ${SyslLock.LockFile} at $root")
            case Some(false) => println(s"sysl: ${SyslLock.LockFile} at $root is up to date")
            case None        => println(s"sysl: $root has no resolvable packages; nothing to lock")
        case _ =>
          // --locked / --frozen: byte-compare instead of writing. applyLockMode
          // emits the standard "out of date" diagnostic; if it returns we know
          // the lock matched.
          applyLockMode(resolved, lockMode)
          println(s"sysl: ${SyslLock.LockFile} at $root is up to date")

  /** `sysl update [root]` — re-resolve the dep graph from scratch and rewrite
   *  the lock unconditionally. Git deps re-resolve their refs (so a `branch
   *  = "main"` dep advances to the new HEAD); path deps just refresh the
   *  recorded directory. Reports per-package version transitions for any
   *  dep whose `<name> <version>` (or sha) actually changed. */
  private def executeUpdate(cmd: UpdateCommand): Unit =
    val priorLock = SyslResolver.findProjectRoot(io, Seq(cmd.root)) match
      case None => None
      case Some(root) => SyslLock.loadFrom(io, root) match
          case Right(opt) => opt
          // A version-too-new lock is a hard failure even for `update`: we can't
          // safely overwrite the user's newer file with our older format. Any
          // other parse failure is just "no diff baseline" — proceed but warn.
          case Left(e: SyslLock.LockLoadError.IncompatibleVersion) =>
            fail(s"error: ${e.message}")
          case Left(SyslLock.LockLoadError.Malformed(msg)) =>
            System.err.println(s"warning: discarding unparseable prior lock: $msg")
            None
    val resolved = resolveDeps(Seq(cmd.root), ignoreLockPins = true) match
      case Right(r) => r
      case Left(msg) => fail(s"error: $msg")
    resolved.projectRoot match
      case None =>
        println(s"sysl: no sysl.toml in scope at ${cmd.root}; nothing to update")
      case Some(root) =>
        val newLock = SyslLock.fromResolved(io, resolved)
        if newLock.packages.isEmpty then
          println(s"sysl: $root has no resolvable packages; nothing to update")
        else
          for line <- formatUpdateDiff(priorLock, newLock) do println(line)
          val path = io.joinPath(root, SyslLock.LockFile)
          io.writeFile(path, SyslLock.render(newLock))
          println(s"sysl: rewrote ${SyslLock.LockFile} at $root")

  /** `sysl tree [root]` — resolve and print the dep graph as an indented
   *  tree, like `cargo tree`. Single-package roots produce one tree;
   *  workspaces produce one tree per member. Cycles short-circuit with a
   *  `(*)` marker so a self-referential dep doesn't recurse forever. */
  private def executeTree(cmd: TreeCommand): Unit =
    val resolved = resolveDeps(Seq(cmd.root), ignoreLockPins = false) match
      case Right(r) => r
      case Left(msg) => fail(s"error: $msg")
    resolved.projectRoot match
      case None =>
        println(s"sysl: no sysl.toml in scope at ${cmd.root}; nothing to print")
      case Some(root) =>
        resolved.manifests.get(root) match
          case Some(WorkspaceManifest(ws)) =>
            for (member, idx) <- ws.members.zipWithIndex do
              if idx > 0 then println()
              val memberDir = SyslResolver.resolveDepPath(io, root, member)
              printTree(resolved, memberDir, Set.empty, "", isLast = true, isRoot = true)
          case Some(PackageManifest(_)) =>
            printTree(resolved, root, Set.empty, "", isLast = true, isRoot = true)
          case _ =>
            println(s"sysl: $root has no [package] or [workspace] table; nothing to print")

  /** Cargo-style box-drawing tree print. `prefix` carries the accumulated
   *  vertical bars from outer levels; `isLast` decides between `└──` and
   *  `├──` for this node; `visited` is the cycle guard (compares by dep
   *  directory because that's the unique identity post-resolution). */
  private def printTree(
      resolved: ResolvedDeps,
      dir: String,
      visited: Set[String],
      prefix: String,
      isLast: Boolean,
      isRoot: Boolean,
  ): Unit =
    val label = treeLabel(resolved, dir)
    val cycleMarker = if visited.contains(dir) then " (*)" else ""
    val branch =
      if isRoot then ""
      else if isLast then "└── "
      else "├── "
    println(s"$prefix$branch$label$cycleMarker")
    if visited.contains(dir) then return
    val nextVisited = visited + dir
    val children = childEdges(resolved, dir)
    val childPrefix =
      if isRoot then prefix
      else prefix + (if isLast then "    " else "│   ")
    for ((_, childDir), idx) <- children.zipWithIndex do
      val childIsLast = idx == children.length - 1
      printTree(resolved, childDir, nextVisited, childPrefix, childIsLast, isRoot = false)

  /** "<name> v<version> (<source>)" for one node. Falls back to the directory
   *  itself when no [package] is declared (synthetic / marker manifests). */
  private def treeLabel(resolved: ResolvedDeps, dir: String): String =
    resolved.manifests.get(dir).flatMap(_.pkg) match
      case Some(pkg) =>
        val sourceTag = resolved.gitSources.get(dir) match
          case Some(g) => s" (git+${g.url}#${g.sha.take(7)})"
          case None =>
            // No git source ⇒ either path dep or local. We show "(local)" for the
            // project root (or workspace member) and the absolute path otherwise.
            if isLocalDir(resolved, dir) then " (local)"
            else s" (${io.absolutePath(dir)})"
        s"${pkg.name} v${pkg.version}$sourceTag"
      case None => dir

  /** Edges out of `dir`, in manifest declaration order. Returns
   *  `(alias, depDir)`; the alias is what the parent's `[dependencies]` table
   *  declared, the dir is the resolved location. */
  private def childEdges(resolved: ResolvedDeps, dir: String): List[(String, String)] =
    resolved.manifests.get(dir).flatMap(_.pkg) match
      case None => Nil
      case Some(pkg) =>
        pkg.deps.toList.flatMap { case (alias, _) =>
          resolved.depResolutions.get((dir, alias)).map(d => (alias, d))
        }

  /** A "local" directory is the project root in single-package mode, or a
   *  workspace member in workspace mode. Mirrors `SyslLock.computeLocalDirs`
   *  but kept private here because the tree printer needs it for labeling. */
  private def isLocalDir(resolved: ResolvedDeps, dir: String): Boolean =
    resolved.projectRoot match
      case None => false
      case Some(root) =>
        resolved.manifests.get(root) match
          case Some(WorkspaceManifest(ws)) =>
            ws.members.exists(m => SyslResolver.resolveDepPath(io, root, m) == dir)
          case Some(PackageManifest(_)) => dir == root
          case _ => false

  /** Cargo-style "Updating <name> v0.1.0 -> v0.2.0" lines, plus sha
   *  transitions for git deps. Empty when nothing changed (so the user
   *  sees only the "rewrote" confirmation). */
  private def formatUpdateDiff(prior: Option[SyslLock], next: SyslLock): List[String] =
    val priorByName: Map[String, LockedPackage] =
      prior.map(_.packages.map(p => p.name -> p).toMap).getOrElse(Map.empty)
    val out = List.newBuilder[String]
    for p <- next.packages do
      priorByName.get(p.name) match
        case None =>
          if prior.isDefined then out += s"sysl: adding ${p.name} v${p.version}"
        case Some(old) =>
          if old.version != p.version then
            out += s"sysl: updating ${p.name} v${old.version} -> v${p.version}"
          else (old.source, p.source) match
            case (Some(LockedSource.Git(_, _, _, oldSha)), Some(LockedSource.Git(_, _, _, newSha)))
                if oldSha != newSha =>
              out += s"sysl: updating ${p.name} (${oldSha.take(7)} -> ${newSha.take(7)})"
            case _ => ()
    val nextNames = next.packages.map(_.name).toSet
    for p <- prior.toList.flatMap(_.packages) if !nextNames.contains(p.name) do
      out += s"sysl: removing ${p.name} v${p.version}"
    out.result()

  /** Helper that wraps `SyslResolver.resolve` to pull in the JVM-installed
   *  GitFetcher (if any) and pre-load the on-disk `sysl.lock` for ref pins.
   *
   *  `ignoreLockPins = true` is `sysl update`'s mode — it forces re-resolution
   *  of git refs even when the lock could satisfy them. Everything else
   *  (build/test/run/fetch) defaults to honoring pins.
   *
   *  `lockMode` selects between Default (network allowed, lock writable),
   *  Locked (network allowed, lock must already match), and Frozen (no
   *  network, lock must already match and pin every git url). */
  private def resolveDeps(
      inputs: Seq[String],
      ignoreLockPins: Boolean,
      lockMode: LockMode = LockMode.Default,
  ): Either[String, ResolvedDeps] =
    val pinsResult: Either[String, Map[(String, String, String), String]] =
      if ignoreLockPins then Right(Map.empty)
      else SyslResolver.findProjectRoot(io, inputs) match
          case None       => Right(Map.empty)
          case Some(root) =>
            // IncompatibleVersion is fatal (would silently downgrade a newer
            // lock). Malformed prints a warning and falls back to an empty pin
            // map so a corrupt lock doesn't break a normal build — the next
            // write will replace it with a valid one. `--locked` adds a
            // stricter byte-compare downstream via `applyLockMode`.
            SyslLock.loadFrom(io, root) match
              case Right(opt) => Right(opt.map(SyslLock.pinsFor).getOrElse(Map.empty))
              case Left(e: SyslLock.LockLoadError.IncompatibleVersion) => Left(e.message)
              case Left(SyslLock.LockLoadError.Malformed(msg)) =>
                System.err.println(s"warning: ignoring unparseable sysl.lock: $msg")
                Right(Map.empty)
    pinsResult.flatMap(
      SyslResolver.resolve(io, inputs, GitFetcherProvider.instance, _, offline = lockMode == LockMode.Frozen)
    )

  /** When `lockMode` is Default, write/refresh the lock as usual. Otherwise
   *  byte-compare the rendered lock against any on-disk `sysl.lock` and abort
   *  the command if they differ — the equivalent of Cargo's `--locked`/`--frozen`
   *  consistency check. Skips silently when `writeIfChanged` itself would skip
   *  (no project root, marker-only manifest, or zero packages). */
  private def applyLockMode(resolved: ResolvedDeps, lockMode: LockMode): Unit =
    lockMode match
      case LockMode.Default => SyslLock.writeIfChanged(io, resolved)
      case _ =>
        val newLock = SyslLock.fromResolved(io, resolved)
        if newLock.packages.isEmpty then return
        SyslLock.lockPathFor(io, resolved) match
          case None => ()
          case Some(path) =>
            val rendered = SyslLock.render(newLock)
            val flag = if lockMode == LockMode.Frozen then "--frozen" else "--locked"
            if !io.exists(path) then
              fail(s"error: $flag specified but ${SyslLock.LockFile} is missing at $path; run `sysl fetch` first")
            else
              val current = try io.readFile(path) catch case t: Throwable =>
                fail(s"error: $flag: cannot read $path: ${t.getMessage}")
              if current != rendered then
                fail(s"error: $flag: ${SyslLock.LockFile} at $path is out of date; run `sysl fetch` to refresh, or `sysl update` to re-resolve")

  private def executeProve(cmd: ProveCommand): Unit =
    // Phase 1: parse the input file and translate to WhyML directly. We do not run the
    // analyzer because contracts are woven into the body by the time the typed AST exists,
    // and WhyML wants them as separate declarative clauses. Type checking happens at the
    // WhyML/Why3 layer instead.
    val parser = new SyslParser
    val out = new StringBuilder
    var firstUnit = true
    for path <- cmd.inputs do
      if !io.exists(path) then fail(s"error: file not found: $path")
      val raw = io.readFile(path)
      val source = if io.fileName(path).endsWith(".lsysl") then
        LiterateRenderer.tangle(new LiterateParser().parse(raw))
      else raw
      parser.parseProgram(source) match
        case Left(err) => fail(s"parse error in $path: $err")
        case Right(ast) =>
          val moduleName = io.fileName(path)
            .stripSuffix(".lsysl").stripSuffix(".sysl")
            .replace('-', '_').replace('.', '_').capitalize
          val backend = new SyslWhyMLBackend(moduleName)
          val mlw =
            try backend.generate(ast)
            catch case e: RuntimeException => fail(s"WhyML translation of $path failed: ${e.getMessage}")
          if !firstUnit then out.append('\n')
          firstUnit = false
          out.append(mlw)
    cmd.output match
      case Some(path) =>
        io.writeFile(path, out.toString)
        System.err.println(s"  -> $path")
      case None =>
        print(out.toString)

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

  /** Resolve a source file, returning (relative-path-without-extension, source-code).
    *
    * Key computation is delegated to `SyslDriver.computeSourceKey`, which
    * walks up looking for a project marker (`sysl.toml`) when no explicit
    * `baseDir` is supplied — letting sysl-native repos declare module paths
    * relative to the package root regardless of where the repo lives on disk.
    */
  private def resolveSource(path: String, baseDir: String): (String, String) =
    val name = io.fileName(path)
    val raw = io.readFile(path)
    val key = SyslDriver.computeSourceKey(io, path, baseDir)
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
