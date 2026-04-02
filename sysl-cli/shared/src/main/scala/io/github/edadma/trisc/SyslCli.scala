package io.github.edadma.trisc

import scopt.OParser

sealed trait SyslCommand
case class CompileCommand(
    inputs: Seq[String] = Seq.empty,
    output: Option[String] = None,
    emit: String = "asm", // asm, tof, llvm
) extends SyslCommand
case class RunCommand(
    inputs: Seq[String] = Seq.empty,
    programArgs: Seq[String] = Seq.empty,
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
          case CompileCommand(inputs, _, _) if inputs.isEmpty =>
            failure("No input files specified")
          case RunCommand(inputs, _) if inputs.isEmpty =>
            failure("No input files specified for run")
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
    catch case CliError(_) => () // already printed

  private def executeCompile(cmd: CompileCommand): Unit =
    val sources = resolveSources(cmd.inputs)

    val driver = new SyslDriver
    val result = driver.compile(sources)

    cmd.emit match
      case "asm" =>
        val codegen = new SyslTriscCodegen
        for unit <- result.units do
          val asm = codegen.generate(unit.typed)
          val outFile = outputPath(cmd.output, unit.name, ".asm", result.units.size)
          io.writeFile(outFile, asm)
          System.err.println(s"  ${unit.name} -> $outFile")

      case "tof" =>
        val codegen = new SyslTriscCodegen
        val tofs = for unit <- result.units yield
          val asm = codegen.generate(unit.typed)
          assemble(asm, relocatable = true)
        val linked = Linker.link(tofs, relocatable = true)
        val outFile = cmd.output.getOrElse("out.tof")
        io.writeFile(outFile, linked.serialize)
        System.err.println(s"  -> $outFile")

      case "llvm" =>
        val codegen = new SyslLLVMCodegen
        for unit <- result.units do
          val ir = codegen.generate(unit.typed)
          val outFile = outputPath(cmd.output, unit.name, ".ll", result.units.size)
          io.writeFile(outFile, ir)
          System.err.println(s"  ${unit.name} -> $outFile")

      case _ => System.err.println(s"Unknown emit format: ${cmd.emit}")

  private def executeRun(cmd: RunCommand): Unit =
    val sources = resolveSources(cmd.inputs)
    val argv = cmd.programArgs.toArray

    if sources.size == 1 then
      // Single file: parse → analyze → interpret directly
      val source = sources.values.head
      val parser = new SyslParser
      parser.parseProgram(source) match
        case Left(err) =>
          fail(s"parse error: $err")
        case Right(ast) =>
          // Check for stdlib imports and register them with the analyzer
          val stdlibImports = ast.decls.collect {
            case ImportDeclAST(path, _) if SyslStdlib.modules.contains(path) => path
          }.toSet
          val analyzer = new SyslAnalyzer
          for mod <- stdlibImports do
            analyzer.registerImport(SyslStdlib.meta(mod))
          val typed = analyzer.analyze(ast)
          val interpreter = new SyslInterpreter()
          wireStdlib(interpreter, stdlibImports, argv)
          val result = interpreter.run(typed)
          if result != 0 then println(result)
    else
      // Multi-file: use driver, merge typed ASTs, then interpret
      val driver = new SyslDriver
      val result = driver.compile(sources)
      val stdlibImports = driver.collectStdlibImports(result.units)
      val merged = TProgram(result.units.flatMap(_.typed.decls))
      val interpreter = new SyslInterpreter()
      wireStdlib(interpreter, stdlibImports, argv)
      val value = interpreter.run(merged)
      if value != 0 then println(value)

  private def wireStdlib(interpreter: SyslInterpreter, imports: Set[String], argv: Array[String] = Array.empty): Unit =
    if imports.nonEmpty then
      val ctx = new SyslStdlib.StdlibContext(argv = argv)
      for mod <- imports do
        interpreter.registerBuiltins(SyslStdlib.builtins(mod, ctx))
      // Register constants (e.g., O_RDONLY, STDIN, etc.)
      if imports.contains("std/io") then
        for (name, value) <- SyslStdlib.ioConstants do
          interpreter.registerGlobal(name, value)

  private def io: FileOps = FileOps.instance

  private def isSyslSource(name: String): Boolean =
    name.endsWith(".sysl") || name.endsWith(".lsysl")

  private def resolveSource(path: String): (String, String) =
    val name = io.fileName(path)
    val raw = io.readFile(path)
    if name.endsWith(".lsysl") then
      val doc = new LiterateParser().parse(raw)
      (name.stripSuffix(".lsysl"), LiterateRenderer.tangle(doc))
    else
      (name.stripSuffix(".sysl"), raw)

  private def resolveSources(inputs: Seq[String]): Map[String, String] =
    if inputs.size == 1 && io.isDirectory(inputs.head) then
      val files = io.listFiles(inputs.head).filter(f => isSyslSource(io.fileName(f)))
      if files.isEmpty then
        fail(s"error: no .sysl or .lsysl files in directory: ${inputs.head}")
      files.map(f => resolveSource(f)).toMap
    else
      inputs.map { path =>
        if !io.exists(path) then
          fail(s"error: file not found: $path")
        resolveSource(path)
      }.toMap

  private def outputPath(output: Option[String], name: String, ext: String, unitCount: Int): String =
    output match
      case Some(out) if unitCount == 1 => out
      case Some(out) =>
        if !io.exists(out) then io.mkdirs(out)
        io.joinPath(out, name + ext)
      case None => name + ext
