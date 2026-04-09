package io.github.edadma.trisc

class OSKitLoaderTests extends OSKitTestHelpers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  // --- OS modules ---
  private lazy val ipcSysl: String      = readLsysl("oskit/ipc/ipc.lsysl")
  private lazy val diskSysl: String     = readLsysl("oskit/drivers/disk/disk.lsysl")
  private lazy val kbdSysl: String      = readLsysl("oskit/drivers/kbd/keyboard.lsysl")
  private lazy val ttySysl: String      = readLsysl("oskit/drivers/tty/tty.lsysl")
  private lazy val tfsSysl: String      = readLsysl("oskit/fs/tfs.lsysl")
  private lazy val tfsSrvSysl: String   = readLsysl("oskit/servers/tfs.lsysl")
  private lazy val loaderSysl: String   = readLsysl("oskit/loader/loader.lsysl")
  private lazy val nshSysl: String      = readLsysl("oskit/apps/nsh.lsysl")
  private lazy val initSysl: String     = readLsysl("oskit/apps/init.lsysl")
  private lazy val loginSysl: String    = readLsysl("oskit/apps/login.lsysl")
  private lazy val sha256Sysl: String   = readLsysl("std/crypto/sha256/sha256.lsysl")
  private lazy val hmacSysl: String     = readLsysl("std/crypto/hmac/hmac.lsysl")
  private lazy val pbkdf2Sysl: String   = readLsysl("std/crypto/pbkdf2/pbkdf2.lsysl")

  // --- std lib ---
  private lazy val stringsSysl: String  = readLsysl("std/strings/strings.lsysl")
  private lazy val builderSysl: String  = readLsysl("std/builder/builder.lsysl")
  private lazy val strconvSysl: String  = readLsysl("std/strconv/strconv.lsysl")
  private lazy val resultSysl: String   = readLsysl("std/result/result.lsysl")
  private lazy val errorsSysl: String   = readLsysl("std/errors/errors.lsysl")
  private lazy val debugSysl: String    = readLsysl("std/debug/debug.lsysl")
  private lazy val memSysl: String      = readLsysl("std/mem/mem.lsysl")
  private lazy val binarySysl: String   = readLsysl("std/encoding/binary/binary.lsysl")
  private lazy val utf8Sysl: String     = readLsysl("std/utf8/utf8.lsysl")

  // --- User library for external programs ---
  private lazy val ulibSysl: String     = readLsysl("oskit/ulib/ulib.lsysl")
  private lazy val helloSysl: String    = readLsysl("oskit/bin/hello.lsysl")

  // Linker script for external programs — all load at same base address.
  private lazy val progScript: LinkerScript = LinkerScriptParser.parse(
    """SECTIONS
      |    code: 0xD0000
      |    rodata
      |    data
      |    bss
      |SYMBOL _heap_start = AFTER bss
      |SYMBOL _heap_end = 0xCC000
      |ENTRY main
      |""".stripMargin) match
    case Right(s) => s
    case Left(e) => throw new RuntimeException(s"Failed to parse linker script: $e")

  // Compile an external program with the user library into an executable TOF.
  private def compileProgram(progSources: Map[String, String]): String =
    val syscallTof = assemble(
      scala.io.Source.fromFile("oskit/ulib/syscall.asm").mkString,
      relocatable = true,
    )
    val allSources = progSources + ("oskit/ulib/ulib" -> ulibSysl)
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(syscallTof, syslTof), progScript, 0)
    linked.serialize

  private lazy val helloTofText: String =
    compileProgram(Map("oskit/bin/hello/hello" -> helloSysl))

  // Build OS with nsh + loader + init
  private lazy val osLinked: TOF =
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
      "std/crypto/sha256/sha256"   -> sha256Sysl,
      "std/crypto/hmac/hmac"       -> hmacSysl,
      "std/crypto/pbkdf2/pbkdf2"   -> pbkdf2Sysl,
      "oskit/loader/loader"         -> loaderSysl,
      "oskit/apps/nsh/nsh"          -> nshSysl,
      "oskit/apps/init/init"        -> initSysl,
      "oskit/apps/login/login"      -> loginSysl,
      "app" ->
        """import oskit.kernel.*
import oskit.ipc.*
import oskit.apps.init.{init}
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(init, 0x640000, 0x640000, "init")
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

  private val ttytab = "/etc/ttytab file \"tty0 nsh\"\n"

  import java.awt.event.KeyEvent
  private def charToVK(ch: Char): (Int, Int) = ch match
    case c if c >= 'a' && c <= 'z' => (KeyEvent.VK_A + (c - 'a'), 0)
    case c if c >= 'A' && c <= 'Z' => (KeyEvent.VK_A + (c - 'A'), 1)
    case c if c >= '0' && c <= '9' => (KeyEvent.VK_0 + (c - '0'), 0)
    case '\n'                      => (KeyEvent.VK_ENTER, 0)
    case ' '                       => (KeyEvent.VK_SPACE, 0)
    case '/'                       => (KeyEvent.VK_SLASH, 0)
    case '-'                       => (KeyEvent.VK_MINUS, 0)
    case '.'                       => (KeyEvent.VK_PERIOD, 0)
    case _                         => (KeyEvent.VK_SPACE, 0)

  def typeString(s: String, startTick: Int, spacing: Int = 2000): Seq[(Int, Int, Boolean, Int)] =
    s.flatMap { ch =>
      val (scancode, mods) = charToVK(ch)
      Seq((0, scancode, true, mods), (0, scancode, false, mods))
    }.zipWithIndex.map { case ((_, sc, press, mods), i) =>
      (startTick + i * spacing, sc, press, mods)
    }

  def runWithKeys(
      prefill: String,
      scheduledKeys: Seq[(Int, Int, Boolean, Int)],
      maxCycles: Int = 15000000,
      files: Map[String, Array[Byte]] = Map.empty,
  ): (CPU, String, RAM) =
    val linked = osLinked
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
      Runtime.ramdiskAddress, ram, sectors = 64, sectorSize = 4096,
      intc, irq = 3, prefill = ttytab + prefill, maxInodes = 32,
      files = files,
    )
    val sha = new ShaAccelerator(Runtime.shaAccelAddress)
    val dma = new DMA(Runtime.dmaAddress, null, intc, 4)
    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, ramdisk, sha, dma)
    dma.mem = mem
    linked.load(mem)
    val pending                  = scheduledKeys.sortBy(_._1).to(scala.collection.mutable.Queue)
    val keyInjector: CPU => Unit = cpu => {
      val cycle = cpu.cycles
      while pending.nonEmpty && cycle >= pending.head._1 do
        val (_, vk, press, mods) = pending.dequeue()
        kbd.enqueue(vk, press,
          shiftDown = (mods & 1) != 0, ctrlDown = (mods & 2) != 0,
          altDown = (mods & 4) != 0, metaDown = (mods & 8) != 0)
    }
    val ticks: Seq[CPU => Unit] = Seq(timer, intc, keyInjector)
    val cpu = new CPU(mem, ticks) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString, ram)

  "Loader: run hello from shell" in {
    val tofBytes = helloTofText.getBytes("UTF-8")
    info(s"Hello TOF size: ${tofBytes.length} bytes (${(tofBytes.length + 511) / 512} blocks)")
    val keys = typeString("hello\n", startTick = 500000)
    val (_, output, _) = runWithKeys("", keys, maxCycles = 30000000,
      files = Map("/bin/hello" -> tofBytes))
    val cleaned = output.filterNot(_ == '\n')
    cleaned should include("Hello, world!")
  }

  "Loader: unknown program shows not found" in {
    val keys = typeString("nosuchprog\n", startTick = 500000)
    val (_, output, _) = runWithKeys("", keys)
    output should include("not found")
  }

  // Pad the hello TOF with comment lines to inflate file size.
  // The TOF parser skips lines starting with '#'.
  private def padTof(tof: String, targetSize: Int): String =
    val sb = new StringBuilder(tof)
    val pad = "# padding comment line to inflate TOF file size for testing\n"
    while sb.length < targetSize do
      sb.append(pad)
    sb.toString

  private def testPaddedHello(size: Int, maxCycles: Int = 100000000): Unit =
    val padded = padTof(helloTofText, size)
    val tofBytes = padded.getBytes("UTF-8")
    info(s"Padded TOF size: ${tofBytes.length} bytes (${(tofBytes.length + 511) / 512} blocks)")
    val keys = typeString("hello\n", startTick = 500000)
    val (cpu, output, _) = runWithKeys("", keys, maxCycles = maxCycles,
      files = Map("/bin/hello" -> tofBytes))
    info(s"CPU state: ${cpu.state}, cycles: ${cpu.cycles}, PC: 0x${cpu.pc.toHexString}")
    info(s"Output: ${output.take(200)}")
    val cleaned = output.filterNot(_ == '\n')
    cleaned should include("Hello, world!")

  "Loader: padded hello 62KB" in { testPaddedHello(62000) }

  // Syscall trampoline without malloc/free stubs (for linking with real posix alloc).
  private val syscallOnlyAsm =
    """segment code
      |global syscall, func
      |syscall
      |    ldd  r2, r7, r0
      |    trap 0
      |    jalr r0, r6
      |""".stripMargin

  // Compile hello with posix modules linked in (larger TOF, exercises loader with many DATA lines).
  private def compileFatProgram(progSources: Map[String, String]): String =
    val syscallTof = assemble(syscallOnlyAsm, relocatable = true)
    val allSources = progSources + ("oskit/ulib/ulib" -> ulibSysl)
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(syscallTof, syslTof), progScript, 0)
    linked.serialize

  private lazy val fatHelloTofText: String =
    compileFatProgram(Map(
      "oskit/bin/hello/hello"    -> helloSysl,
      "posix/string/string"     -> posixStringSysl,
      "posix/ctype/ctype"       -> posixCtypeSysl,
      "posix/stdlib/alloc"      -> posixAllocSysl,
      "posix/unistd/sbrk"       -> sbrkSysl,
    ))

  "Loader: run fat hello (60KB with posix modules)" in {
    val tofBytes = fatHelloTofText.getBytes("UTF-8")
    info(s"Fat hello TOF size: ${tofBytes.length} bytes")
    val keys = typeString("hello\n", startTick = 500000)
    val (_, output, _) = runWithKeys("", keys, maxCycles = 100000000,
      files = Map("/bin/hello" -> tofBytes))
    val cleaned = output.filterNot(_ == '\n')
    cleaned should include("Hello, world!")
  }
}
