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
  private lazy val memSysl: String = readLsysl("std/mem/mem.lsysl")
  private lazy val halMemSysl: String = readLsysl("oskit/hal/mem_dma.lsysl")
  private lazy val debugSysl: String = readLsysl("std/debug/debug.lsysl")

  // Inline sbrk for TFS tests — simple bump allocator in high RAM
  private val sbrk_inline: String =
    """module posix.unistd
      |var _brk: *byte = *byte(0x600000)
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
    s"""STDOUT = ${Runtime.stdoutAddress}
       |
       |segment vectors
       |
       |  dl ${Runtime.stdoutAddress - 8}
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

  // Library source keys — these never change between tests
  private val libSourceKeys = Set(
    "oskit/fs/tfs", "posix/string/string", "posix/ctype/ctype",
    "posix/stdlib/alloc", "posix/unistd/sbrk", "ramdisk", "std/mem/mem", "std/debug/debug",
  )

  // Cache: boot TOF + compiled+assembled library TOFs (compiled once with dummy main)
  private lazy val cachedBootTof: TOF = assemble(tfsBoot, relocatable = true)
  private lazy val cachedLibTof: TOF =
    val dummySources = Map("main" -> "main() -> int = 0") ++ libSources
    val driver = new SyslDriver
    val result = driver.compile(dummySources)
    val codegen = new SyslTriscCodegen
    val libTofs = for unit <- result.units if libSourceKeys.contains(unit.name) yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    Linker.link(libTofs, relocatable = true)

  private val libSources: Map[String, String] = Map(
    "oskit/fs/tfs" -> tfsSource,
    "posix/string/string" -> posixStringSysl,
    "posix/ctype/ctype" -> posixCtypeSysl,
    "posix/stdlib/alloc" -> posixAllocSysl,
    "posix/unistd/sbrk" -> sbrk_inline,
    "ramdisk" -> ramdiskSource,
    "std/mem/mem" -> memSysl,
    "oskit/hal/mem" -> halMemSysl,
    "std/debug/debug" -> debugSysl,
  )

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
      sectors: Int = 64,
      files: Map[String, Array[Byte]] = Map.empty,
  ): (CPU, String) =
    runTFS(Map("main" -> mainSource), prefill, maxCycles, sectors, files)

  def runTFS(
      sources: Map[String, String],
      prefill: String,
      maxCycles: Int,
      sectors: Int,
      files: Map[String, Array[Byte]],
  ): (CPU, String) =
    // Compile all sources together (needed for import resolution), but only
    // codegen+assemble the user sources — library TOFs are cached.
    val allSources = sources ++ libSources
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val userTofs = for unit <- result.units if !libSourceKeys.contains(unit.name) yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val userTof = Linker.link(userTofs, relocatable = true)
    val linked = Linker.link(Seq(cachedBootTof, cachedLibTof, userTof))

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
      sectors = sectors,
      sectorSize = 4096,
      intc,
      irq = 3,
      prefill = prefill,
      maxInodes = 32,
      files = files,
    )
    val dma = new DMA(Runtime.dmaAddress, null, intc, 4)
    val mem = new Memory("Memory", ram, stdout, intc, timer, ramdisk, dma)
    dma.mem = mem
    linked.load(mem)
    val cpu = new CPU(mem, Seq(timer, intc)) { this.limit = maxCycles; quiet = true }
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
