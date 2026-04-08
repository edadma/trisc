package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

trait TFSTestHelpers extends AnyFreeSpec with Matchers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val tfsSource: String = readLsysl("oskit/fs/tfs.lsysl")
  private lazy val posixStringSysl: String = scala.io.Source.fromFile("posix/string/string.sysl").mkString
  private lazy val posixCtypeSysl: String = scala.io.Source.fromFile("posix/ctype/ctype.sysl").mkString
  private lazy val posixAllocSysl: String = scala.io.Source.fromFile("posix/stdlib/alloc.sysl").mkString

  // Inline sbrk for TFS tests — simple bump allocator in high RAM
  private val sbrk_inline: String =
    """module posix.unistd
      |var _brk: *byte = *byte(0xC0000)
      |sbrk(increment: int) -> *byte
      |    if increment == 0
      |        return _brk
      |    val old = _brk
      |    _brk = _brk + increment
      |    old
      |""".stripMargin

  // Inline ramdisk block I/O for tests — provides rd_read/rd_write
  // that the TFS library externs. Talks directly to the emulated ramdisk.
  private val ramdiskSource: String =
    s"""val RD_LBA      = ${Runtime.ramdiskAddress}
       |val RD_ADDR     = ${Runtime.ramdiskAddress + 4}
       |val RD_COUNT    = ${Runtime.ramdiskAddress + 12}
       |val RD_COMMAND  = ${Runtime.ramdiskAddress + 15}
       |
       |rd_read(lba: int, addr: *byte)
       |    *(*u32(RD_LBA)) = u32(lba)
       |    *(*u32(RD_ADDR)) = u32(i64(addr))
       |    *(*u16(RD_COUNT)) = u16(1)
       |    *(*byte(RD_COMMAND)) = 1
       |
       |rd_write(lba: int, addr: *byte)
       |    *(*u32(RD_LBA)) = u32(lba)
       |    *(*u32(RD_ADDR)) = u32(i64(addr))
       |    *(*u16(RD_COUNT)) = u16(1)
       |    *(*byte(RD_COMMAND)) = 2
       |
       |slen(s: *byte) -> int
       |    var i = 0
       |    while s[i] != 0
       |        i += 1
       |    i
       |""".stripMargin

  private val tfsBoot: String =
    s"""STDOUT = 0x100000
       |
       |segment vectors
       |
       |  dl 0xFFF8
       |  dl boot
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |  dl default_isr
       |
       |segment code
       |
       |extern main
       |
       |global boot, func
       |entry boot
       |
       |boot
       |  movi r4, main
       |  jalr r6, r4
       |  halt
       |
       |global putchar, func
       |
       |putchar
       |  movi r2, STDOUT
       |  stb r1, r2, r0
       |  jalr r0, r6
       |
       |global default_isr, func
       |
       |default_isr
       |  halt
       |  align 8
       |""".stripMargin

  private var _tracing = false

  /** Wrap a test body to enable CPU instruction tracing to /tmp/trisc_tfs_debug.log */
  def withTrace(testName: String)(body: => Unit): Unit =
    _tracing = true
    try body
    finally _tracing = false

  def runTFS(
      mainSource: String,
      prefill: String = "",
      maxCycles: Int = 5000000,
  ): (CPU, String) =
    runTFS(Map("main" -> mainSource), prefill, maxCycles)

  def runTFS(
      sources: Map[String, String],
      prefill: String,
      maxCycles: Int,
  ): (CPU, String) =
    val bootTof = assemble(tfsBoot, relocatable = true)
    val allSources = sources + ("oskit/fs/tfs" -> tfsSource) + ("posix/string/string" -> posixStringSysl) + ("posix/ctype/ctype" -> posixCtypeSysl) + ("posix/stdlib/alloc" -> posixAllocSysl) + ("posix/unistd/sbrk" -> sbrk_inline) + ("ramdisk" -> ramdiskSource)
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val progTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(bootTof, progTof))

    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = Runtime.stdoutAddress
      val size: Long = 4
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val intc = new InterruptController(Runtime.intcAddress)
    val timer = new Timer(Runtime.timerAddress, intc, irq = 0)
    val ramSize = Runtime.stdoutAddress.toInt
    val ram = new RAM(0, ramSize)
    val ramdisk = new Ramdisk(
      Runtime.ramdiskAddress,
      ram,
      sectors = 64,
      sectorSize = 512,
      intc,
      irq = 3,
      prefill = prefill,
      maxInodes = 32,
    )
    val mem = new Memory("Memory", ram, stdout, intc, timer, ramdisk)
    linked.load(mem)
    val cpu = new CPU(mem, Seq(timer, intc)) { this.limit = maxCycles }
    if _tracing then
      cpu.log.setLogLevel(io.github.edadma.logger.LogLevel.TRACE)
      cpu.log.setHandler(new io.github.edadma.logger.FileHandler("/tmp/trisc_tfs_debug.log"))
    else
      cpu.log.setLogLevel(io.github.edadma.logger.LogLevel.OFF)
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  /** Generate SYSL code to declare a null-terminated byte array from a string.
    * Usage: `syslBytes("path", "/dev/tty0")` produces:
    *   var path: [11]byte
    *   path[0] = 47
    *   path[1] = 100
    *   ...
    *   path[10] = 0
    */
  def syslBytes(name: String, s: String): String =
    val bytes = s.getBytes("UTF-8") :+ 0.toByte
    val decl = s"    var $name: [${bytes.length}]byte"
    val assigns = bytes.zipWithIndex.map { (b, i) => s"    $name[$i] = ${b & 0xff}" }.mkString("\n")
    val lenDecl = s"    val ${name}_len = ${s.length}"
    s"$decl\n$assigns\n$lenDecl"
}
