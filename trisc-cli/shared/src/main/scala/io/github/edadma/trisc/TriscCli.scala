package io.github.edadma.trisc

import scopt.OParser

sealed trait TriscCommand
case class RunCommand(
    input: String = "",
    memSize: Int = 0x10000,
    limit: Int = 0,
    trace: Boolean = false,
    gui: Boolean = false,
) extends TriscCommand
case class AsmCommand(
    input: String = "",
    output: Option[String] = None,
) extends TriscCommand
case class LinkCommand(
    inputs: Seq[String] = Seq.empty,
    output: Option[String] = None,
) extends TriscCommand
case class DisasmCommand(
    input: String = "",
) extends TriscCommand

case class TriscConfig(
    command: TriscCommand = RunCommand(),
)

object TriscCli:
  private val builder = OParser.builder[TriscConfig]

  private val parser =
    import builder.*
    OParser.sequence(
      programName("trisc"),
      head("trisc", "0.1.0"),
      cmd("run")
        .text("Load and execute a TOF file")
        .action((_, c) => c.copy(command = RunCommand()))
        .children(
          opt[Int]('m', "mem")
            .text(s"Memory size in bytes (default 65536)")
            .action((v, c) =>
              c.copy(command = c.command match
                case rc: RunCommand => rc.copy(memSize = v)
                case other          => other
              )
            ),
          opt[Int]("limit")
            .text("Instruction execution limit (0 = unlimited)")
            .action((v, c) =>
              c.copy(command = c.command match
                case rc: RunCommand => rc.copy(limit = v)
                case other          => other
              )
            ),
          opt[Unit]("trace")
            .text("Trace instruction execution")
            .action((_, c) =>
              c.copy(command = c.command match
                case rc: RunCommand => rc.copy(trace = true)
                case other          => other
              )
            ),
          opt[Unit]("gui")
            .text("Open GUI emulator window")
            .action((_, c) =>
              c.copy(command = c.command match
                case rc: RunCommand => rc.copy(gui = true)
                case other          => other
              )
            ),
          arg[String]("<file.tof>")
            .text("TOF file to execute")
            .action((v, c) =>
              c.copy(command = c.command match
                case rc: RunCommand => rc.copy(input = v)
                case other          => other
              )
            ),
        ),
      cmd("asm")
        .text("Assemble a .asm file to TOF")
        .action((_, c) => c.copy(command = AsmCommand()))
        .children(
          opt[String]('o', "output")
            .text("Output TOF file")
            .action((v, c) =>
              c.copy(command = c.command match
                case ac: AsmCommand => ac.copy(output = Some(v))
                case other          => other
              )
            ),
          arg[String]("<file.asm>")
            .text("Assembly source file")
            .action((v, c) =>
              c.copy(command = c.command match
                case ac: AsmCommand => ac.copy(input = v)
                case other          => other
              )
            ),
        ),
      cmd("link")
        .text("Link TOF files")
        .action((_, c) => c.copy(command = LinkCommand()))
        .children(
          opt[String]('o', "output")
            .text("Output TOF file")
            .action((v, c) =>
              c.copy(command = c.command match
                case lc: LinkCommand => lc.copy(output = Some(v))
                case other           => other
              )
            ),
          arg[String]("<file.tof>...")
            .unbounded()
            .text("TOF files to link")
            .action((v, c) =>
              c.copy(command = c.command match
                case lc: LinkCommand => lc.copy(inputs = lc.inputs :+ v)
                case other           => other
              )
            ),
        ),
      cmd("disasm")
        .text("Disassemble a TOF file")
        .action((_, c) => c.copy(command = DisasmCommand()))
        .children(
          arg[String]("<file.tof>")
            .text("TOF file to disassemble")
            .action((v, c) =>
              c.copy(command = c.command match
                case dc: DisasmCommand => dc.copy(input = v)
                case other             => other
              )
            ),
        ),
      checkConfig(c =>
        c.command match
          case rc: RunCommand if rc.input.isEmpty =>
            failure("No input file specified for run")
          case AsmCommand(input, _) if input.isEmpty =>
            failure("No input file specified for asm")
          case LinkCommand(inputs, _) if inputs.isEmpty =>
            failure("No input files specified for link")
          case DisasmCommand(input) if input.isEmpty =>
            failure("No input file specified for disasm")
          case _ => success
      ),
    )

  def parse(args: Seq[String]): Option[TriscConfig] =
    OParser.parse(parser, args, TriscConfig())

  // Platform-specific GUI launcher — set by JVM entry point
  var guiLauncher: Option[(RunCommand, TOF) => Unit] = None

  def execute(config: TriscConfig): Unit =
    config.command match
      case cmd: RunCommand   => executeRun(cmd)
      case cmd: AsmCommand   => executeAsm(cmd)
      case cmd: LinkCommand  => executeLink(cmd)
      case cmd: DisasmCommand => executeDisasm(cmd)

  def loadTof(cmd: RunCommand): TOF =
    val tofStr = readFile(cmd.input)
    val tof = TOF.deserialize(tofStr)
    if tof.entryAddress.isDefined then tof else Linker.link(Seq(tof))

  def setupCpu(linked: TOF, outputFn: String => Unit = s => print(s)): (CPU, Memory) =
    val stdout = new Stdout(Runtime.stdoutAddress, outputFn)
    val ramSize = Runtime.stdoutAddress.toInt
    val ram = new RAM(0, ramSize)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem, Nil)
    cpu.pc = linked.entryAddress.getOrElse(0L)
    cpu.state = State.Run
    cpu.set(Status.Mode, true) // supervisor mode so halt works
    cpu.r(7).write(ramSize - 8)
    (cpu, mem)

  private def executeRun(cmd: RunCommand): Unit =
    val linked = loadTof(cmd)

    if cmd.gui then
      guiLauncher match
        case Some(launch) => launch(cmd, linked)
        case None =>
          System.err.println("error: --gui not available on this platform")
          return

    else
      val (cpu, _) = setupCpu(linked)
      if cmd.limit > 0 then cpu.limit = cmd.limit
      cpu.run()
      val result = cpu.r(1).read
      if result != 0 then System.err.println(s"exit: $result")

  private def executeAsm(cmd: AsmCommand): Unit =
    val source = readFile(cmd.input)
    val tof = assemble(source, relocatable = true)
    val outFile = cmd.output.getOrElse(cmd.input.stripSuffix(".asm") + ".tof")
    writeFile(outFile, tof.serialize)
    System.err.println(s"  ${cmd.input} -> $outFile")

  private def executeLink(cmd: LinkCommand): Unit =
    val tofs = cmd.inputs.map(f => TOF.deserialize(readFile(f)))
    val linked = Linker.link(tofs)
    val outFile = cmd.output.getOrElse("out.tof")
    writeFile(outFile, linked.serialize)
    System.err.println(s"  -> $outFile")

  private def executeDisasm(cmd: DisasmCommand): Unit =
    val tofStr = readFile(cmd.input)
    val tof = TOF.deserialize(tofStr)
    val linked =
      if tof.entryAddress.isDefined then tof
      else Linker.link(Seq(tof))

    val ram = new RAM(0, 0x10000)
    val mem = new Memory("Memory", ram)
    linked.load(mem)

    val disasm = Disassembler.fromTOF(mem, linked)
    // Disassemble all code segments
    for seg <- linked.segments do
      val end = seg.org + seg.chunks.map {
        case TOF.DataChunk(d) => d.length.toLong
        case TOF.ResChunk(s)  => s
      }.sum
      println(disasm.disassembleRange(seg.org, end))

  private def readFile(path: String): String =
    val source = scala.io.Source.fromFile(path)
    try source.mkString
    finally source.close()

  private def writeFile(path: String, content: String): Unit =
    val writer = new java.io.PrintWriter(path)
    try writer.write(content)
    finally writer.close()
