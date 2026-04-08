package io.github.edadma.trisc

class OSKitLoaderTests extends OSKitTestHelpers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val ipcSysl: String      = readLsysl("oskit/ipc/ipc.lsysl")
  private lazy val diskSysl: String     = readLsysl("oskit/drivers/disk/disk.lsysl")
  private lazy val kbdSysl: String      = readLsysl("oskit/drivers/kbd/keyboard.lsysl")
  private lazy val ttySysl: String      = readLsysl("oskit/drivers/tty/tty.lsysl")
  private lazy val tfsSysl: String      = readLsysl("oskit/fs/tfs.lsysl")
  private lazy val tfsSrvSysl: String   = readLsysl("oskit/servers/tfs.lsysl")
  private lazy val loaderSysl: String   = readLsysl("oskit/loader/loader.lsysl")
  private lazy val stringsSysl: String  = readLsysl("std/strings/strings.lsysl")
  private lazy val builderSysl: String  = readLsysl("std/builder/builder.lsysl")
  private lazy val strconvSysl: String  = readLsysl("std/strconv/strconv.lsysl")
  private lazy val resultSysl: String   = readLsysl("std/result/result.lsysl")
  private lazy val errorsSysl: String   = readLsysl("std/errors/errors.lsysl")
  private lazy val debugSysl: String    = readLsysl("std/debug/debug.lsysl")
  private lazy val memSysl: String      = readLsysl("std/mem/mem.lsysl")
  private lazy val binarySysl: String   = readLsysl("std/encoding/binary/binary.lsysl")
  private lazy val utf8Sysl: String     = readLsysl("std/utf8/utf8.lsysl")

  // Compile a standalone hello world program as an executable TOF.
  // It writes "Hello" to STDOUT MMIO and halts.
  private lazy val helloTofText: String =
    val helloSource =
      s"""val STDOUT = ${Runtime.stdoutAddress}
         |main()
         |    val out = *byte(STDOUT)
         |    *out = byte('H')
         |    *out = byte('e')
         |    *out = byte('l')
         |    *out = byte('l')
         |    *out = byte('o')
         |""".stripMargin
    val helloDriver = new SyslDriver
    val helloResult = helloDriver.compile(Map("main" -> helloSource))
    val helloCodegen = new SyslTriscCodegen
    val helloTofs = for unit <- helloResult.units yield
      val asm = helloCodegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    // Link with a linker script that places code at 0xD0000
    val helloScript = LinkerScriptParser.parse(
      """SECTIONS
        |    code: 0xD0000
        |    rodata
        |    data
        |    bss
        |ENTRY main
        |""".stripMargin) match
      case Right(s) => s
      case Left(e) => throw new RuntimeException(s"Failed to parse linker script: $e")
    val helloLinked = Linker.link(helloTofs, helloScript, 0)
    helloLinked.serialize

  // Escape TOF text for TFS prefill (double quotes and backslash-n for newlines)
  private def tofToPrefill(tof: String): String =
    tof.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")

  // Build OS with loader support
  private lazy val loaderLinked: TOF =
    val bootTof = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel/kernel"          -> kernelSysl,
      "oskit/services/services"      -> servicesSysl,
      "oskit/kernel/timer"           -> timerSysl,
      "oskit/sync/semaphore"         -> semaphoreSysl,
      "oskit/sync/mutex"             -> mutexSysl,
      "oskit/ipc/ipc"                -> ipcSysl,
      "oskit/drivers/disk/disk"      -> diskSysl,
      "oskit/drivers/kbd/keyboard"   -> kbdSysl,
      "oskit/drivers/tty/tty"        -> ttySysl,
      "oskit/fs/tfs"                 -> tfsSysl,
      "oskit/servers/tfs"            -> tfsSrvSysl,
      "posix/unistd/sbrk"           -> sbrkSysl,
      "posix/string/string"         -> posixStringSysl,
      "posix/ctype/ctype"           -> posixCtypeSysl,
      "posix/stdlib/alloc"          -> posixAllocSysl,
      "std/debug/debug"              -> debugSysl,
      "std/mem/mem"                  -> memSysl,
      "std/encoding/binary/binary"  -> binarySysl,
      "std/strings/strings"         -> stringsSysl,
      "std/builder/builder"         -> builderSysl,
      "std/strconv/strconv"         -> strconvSysl,
      "std/result/result"           -> resultSysl,
      "std/errors/errors"           -> errorsSysl,
      "std/utf8/utf8"               -> utf8Sysl,
      "oskit/loader/loader"         -> loaderSysl,
      "app" ->
        """import oskit.kernel.*
import oskit.ipc.*
import oskit.drivers.disk.disk_server
import oskit.servers.tfs_server
import oskit.drivers.tty.{tty_server, tty_puts}
import oskit.loader.{load_tof}
import oskit.services.sleep
          |
          |test_loader()
          |    // Wait for servers
          |    sleep(10)
          |    // Load /bin/hello
          |    val entry = load_tof("/bin/hello")
          |    if entry > 0
          |        // Create thread at the loaded entry point
          |        create_thread(entry, 0xB8000, 0xB6000, "hello")
          |    else
          |        // Report error
          |        var msg: [6]byte
          |        msg[0] = byte('E')
          |        msg[1] = byte('R')
          |        msg[2] = byte('R')
          |        msg[3] = byte('\n')
          |        tty_puts(msg, 4)
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x80000, 0x80000, "disk")
          |    sleep(5)
          |    create_thread(tfs_server, 0x90000, 0x90000, "tfs")
          |    create_thread(tty_server, 0xA0000, 0xA0000, "tty")
          |    sleep(5)
          |    create_thread(test_loader, 0xB0000, 0xB0000, "loader")
          |    timer_init(1000)
          |    first_thread_ssp()
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    Linker.link(Seq(bootTof, syslTof), linkerScript, 0)

  def runLoader(
      prefill: String,
      maxCycles: Int = 15000000,
  ): (CPU, String) =
    val linked = loaderLinked

    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name                                            = "stdout"
      val base: Long                                      = Runtime.stdoutAddress
      val size: Long                                      = 4
      def writeByte(addr: Long, data: Long): Unit         = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val intc    = new InterruptController(Runtime.intcAddress)
    val timer   = new Timer(Runtime.timerAddress, intc, irq = 0)
    val kbd     = new KeyboardDevice(Runtime.keyboardAddress, intc, irq = 1)
    val ramSize = Runtime.stdoutAddress.toInt
    val ram     = new RAM(0, ramSize)
    val ramdisk = new Ramdisk(
      Runtime.ramdiskAddress,
      ram,
      sectors = 256,
      sectorSize = 512,
      intc,
      irq = 3,
      prefill = prefill,
      maxInodes = 32,
    )
    val sha = new ShaAccelerator(Runtime.shaAccelAddress)
    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, ramdisk, sha)
    linked.load(mem)

    val cpu = new CPU(mem, Seq(timer, intc)) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  "Loader: load and run hello world from filesystem" in {
    val prefill = s"""/bin/hello file "${tofToPrefill(helloTofText)}"\n"""
    val (_, output) = runLoader(prefill)
    output should include("Hello")
  }
}
