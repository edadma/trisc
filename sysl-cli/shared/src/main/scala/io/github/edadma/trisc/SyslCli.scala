package io.github.edadma.trisc

import scopt.OParser

import java.io.File

sealed trait SyslCommand
case class CompileCommand(
    inputs: Seq[String] = Seq.empty,
    output: Option[String] = None,
    emit: String = "asm", // asm, tof, llvm
) extends SyslCommand
case class RunCommand(
    inputs: Seq[String] = Seq.empty,
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
          case RunCommand(inputs) if inputs.isEmpty =>
            failure("No input files specified for run")
          case _ => success
      ),
    )

  def parse(args: Seq[String]): Option[SyslConfig] =
    OParser.parse(parser, args, SyslConfig())

  def execute(config: SyslConfig): Unit =
    config.command match
      case cmd: CompileCommand => executeCompile(cmd)
      case cmd: RunCommand     => executeRun(cmd)

  private def executeCompile(cmd: CompileCommand): Unit =
    val sources = resolveSources(cmd.inputs)

    val driver = new SyslDriver
    val result = driver.compile(sources)

    cmd.emit match
      case "asm" =>
        val codegen = new SyslTriscCodegen
        for unit <- result.units do
          val asm = codegen.generate(unit.typed)
          val outFile = cmd.output match
            case Some(out) if result.units.size == 1 => out
            case Some(out) =>
              val dir = new File(out)
              if !dir.exists() then dir.mkdirs()
              new File(dir, unit.name + ".asm").getPath
            case None => unit.name + ".asm"
          writeFile(outFile, asm)
          System.err.println(s"  ${unit.name} -> $outFile")

      case "tof" =>
        val codegen = new SyslTriscCodegen
        val tofs = for unit <- result.units yield
          val asm = codegen.generate(unit.typed)
          assemble(asm, relocatable = true)
        val linked = Linker.link(tofs)
        val outFile = cmd.output.getOrElse("out.tof")
        writeFile(outFile, linked.serialize)
        System.err.println(s"  -> $outFile")

      case "llvm" =>
        val codegen = new SyslLLVMCodegen
        for unit <- result.units do
          val ir = codegen.generate(unit.typed)
          val outFile = cmd.output match
            case Some(out) if result.units.size == 1 => out
            case Some(out) =>
              val dir = new File(out)
              if !dir.exists() then dir.mkdirs()
              new File(dir, unit.name + ".ll").getPath
            case None => unit.name + ".ll"
          writeFile(outFile, ir)
          System.err.println(s"  ${unit.name} -> $outFile")

      case _ => System.err.println(s"Unknown emit format: ${cmd.emit}")

  private def executeRun(cmd: RunCommand): Unit =
    val sources = resolveSources(cmd.inputs)

    if sources.size == 1 then
      // Single file: parse → analyze → interpret directly
      val source = sources.values.head
      val parser = new SyslParser
      parser.parseProgram(source) match
        case Left(err) =>
          System.err.println(s"parse error: $err")
          sys.exit(1)
        case Right(ast) =>
          val analyzer = new SyslAnalyzer
          val typed = analyzer.analyze(ast)
          val interpreter = new SyslInterpreter()
          val result = interpreter.run(typed)
          if result != 0 then println(result)
    else
      // Multi-file: use driver, merge typed ASTs, then interpret
      val driver = new SyslDriver
      val result = driver.compile(sources)
      val merged = TProgram(result.units.flatMap(_.typed.decls))
      val interpreter = new SyslInterpreter()
      val value = interpreter.run(merged)
      if value != 0 then println(value)

  private def resolveSources(inputs: Seq[String]): Map[String, String] =
    if inputs.size == 1 then
      val f = new File(inputs.head)
      if f.isDirectory then
        val files = f.listFiles().filter(_.getName.endsWith(".sysl"))
        if files.isEmpty then
          System.err.println(s"error: no .sysl files in directory: ${inputs.head}")
          sys.exit(1)
        files.map(f => (f.getName.stripSuffix(".sysl"), readFile(f.getPath))).toMap
      else
        Map(f.getName.stripSuffix(".sysl") -> readFile(f.getPath))
    else
      inputs.map { path =>
        val f = new File(path)
        if !f.exists() then
          System.err.println(s"error: file not found: $path")
          sys.exit(1)
        (f.getName.stripSuffix(".sysl"), readFile(f.getPath))
      }.toMap

  private def readFile(path: String): String =
    val source = scala.io.Source.fromFile(path)
    try source.mkString
    finally source.close()

  private def writeFile(path: String, content: String): Unit =
    val writer = new java.io.PrintWriter(path)
    try writer.write(content)
    finally writer.close()
