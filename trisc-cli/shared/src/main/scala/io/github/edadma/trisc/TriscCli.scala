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
    script: Option[String] = None,
) extends TriscCommand
case class DisasmCommand(
    input: String = "",
    fromHex: Option[String] = None,
    toHex: Option[String] = None,
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
          opt[String]('s', "script")
            .text("Linker script file")
            .action((v, c) =>
              c.copy(command = c.command match
                case lc: LinkCommand => lc.copy(script = Some(v))
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
          opt[String]("from")
            .text("Start address (hex, optional 0x) — list only [from, to)")
            .action((v, c) =>
              c.copy(command = c.command match
                case dc: DisasmCommand => dc.copy(fromHex = Some(v))
                case other               => other
              )
            ),
          opt[String]("to")
            .text("End address (hex, exclusive). Default with --from: from+0x100")
            .action((v, c) =>
              c.copy(command = c.command match
                case dc: DisasmCommand => dc.copy(toHex = Some(v))
                case other               => other
              )
            ),
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
          case LinkCommand(inputs, _, _) if inputs.isEmpty =>
            failure("No input files specified for link")
          case DisasmCommand(input, _, _) if input.isEmpty =>
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
    if tof.tofType == TOFType.Executable then tof else Linker.link(Seq(Runtime.bootTof, tof, Runtime.ioTof))

  def setupCpu(
      linked: TOF,
      outputFn: String => Unit = s => { print(s); System.out.flush() },
      extraDevices: Seq[Addressable] = Nil,
      intc: InterruptController = new InterruptController(Runtime.intcAddress),
  ): (CPU, Memory) =
    val stdout = new Stdout(Runtime.stdoutAddress, outputFn)
    val timer = new Timer(Runtime.timerAddress, intc, irq = 0)
    val ramSize = Runtime.stdoutAddress.toInt
    val ram = new RAM(0, ramSize)
    val ramdisk = new Ramdisk(
      Runtime.ramdiskAddress,
      ram,
      sectors = 256,
      sectorSize = 4096,
      intc,
      irq = 3,
      prefill = """
        /dev/tty0 char 0 0
        /dev/disk0 block 1 0
        /dev/null char 0 1
        /root dir
        /home dir
        /home/ed dir
        /etc/passwd file "root:x:0:0:root:/root:/nsh\ned:x:1000:1000:ed:/home/ed:/nsh"
        /etc/shadow file "root:slix:3b1b8291c0bdb62febcd914f45884bca403ae1c42a4bb1c41755881f3886d158\ned:slix:c638d5b6e91f70b96934aac8d7be42363ce4ea5927f9a9bbbe2d64a8b51926b5"
        /etc/ttytab file "tty0 login"
      """,
      files = RamdiskBinPrograms.loadEmbeddedBinaries(),
    )
    val sha = new ShaAccelerator(Runtime.shaAccelAddress)
    // std.mem memcpy/memset program DMA at Runtime.dmaAddress — must be present or stores fault (DataAccess 'D').
    val dma = new DMA(Runtime.dmaAddress, null, intc, irq = 4)
    val mem = new Memory("Memory", (Seq(ram, stdout, intc, timer, ramdisk, sha, dma) ++ extraDevices)*)
    dma.mem = mem
    linked.load(mem)
    val mmu = new SimpleMMU(mem)
    mmu.setIdentityRange(0x7FE000L, 0xC00000L) // kernel PTBR, identity-mapped up to 12MB
    dma.mmu = Some(mmu)
    val cpu = new CPU(mem, Seq(timer, intc), mmu = Some(mmu))
    cpu.reset() // like 68000: reads SSP from vector[0], PC from vector[1], enters supervisor mode
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
      if cmd.trace then cpu.trace = true
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
    val linked = cmd.script match
      case Some(path) =>
        val script = LinkerScriptParser.parse(readFile(path)) match
          case Right(s) => s
          case Left(e)  => throw new RuntimeException(s"Failed to parse linker script: $e")
        Linker.link(tofs, script, 0)
      case None => Linker.link(tofs)
    val outFile = cmd.output.getOrElse("out.tof")
    writeFile(outFile, linked.serialize)
    System.err.println(s"  -> $outFile")

  private def parseHexAddr(label: String, s: String): Long =
    val t = s.strip.replaceFirst("^0[xX]", "")
    try java.lang.Long.parseLong(t, 16)
    catch
      case _: NumberFormatException =>
        throw new IllegalArgumentException(s"disasm: invalid hex for $label: $s")

  private def executeDisasm(cmd: DisasmCommand): Unit =
    val tofStr = readFile(cmd.input)
    val tof = TOF.deserialize(tofStr)
    val linked =
      if tof.entryAddress.isDefined then tof
      else Linker.link(Seq(tof))

    val ramSize = Runtime.stdoutAddress.toInt
    val ram = new RAM(0, ramSize)
    val mem = new Memory("Memory", ram)
    linked.load(mem)

    val disasm = Disassembler.fromTOF(mem, linked)
    (cmd.fromHex, cmd.toHex) match
      case (Some(fh), Some(th)) =>
        val from = parseHexAddr("--from", fh)
        val to = parseHexAddr("--to", th)
        if from < 0 || to > ramSize then
          throw new IllegalArgumentException(
            s"disasm: range [$from%04x, $to%04x) must lie within RAM [0, $ramSize%x)",
          )
        if from >= to then throw new IllegalArgumentException("disasm: --from must be < --to")
        println(disasm.disassembleRange(from, to))
      case (Some(fh), None) =>
        val from = parseHexAddr("--from", fh)
        val to = math.min(from + 0x100, ramSize.toLong)
        if from < 0 || from >= ramSize then
          throw new IllegalArgumentException(s"disasm: --from out of RAM bounds (ram ends at $ramSize%x)")
        println(disasm.disassembleRange(from, to))
      case (None, Some(_)) =>
        throw new IllegalArgumentException("disasm: --to requires --from")
      case (None, None) =>
        for seg <- linked.segments do
          val end = seg.org + seg.chunks.map {
            case TOF.DataChunk(d)  => d.length.toLong
            case TOF.ResChunk(s)   => s
            case TOF.CommentChunk(_) => 0L
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
