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
  private lazy val pmSrvSysl: String   = readLsysl("oskit/servers/pm.lsysl")
  private lazy val halMemSysl: String  = readLsysl("oskit/hal/mem_dma.lsysl")

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
  private lazy val ulibSysl: String      = readLsysl("oskit/ulib/ulib.lsysl")
  private lazy val ulibSbrkSysl: String  = scala.io.Source.fromFile("oskit/ulib/sbrk.sysl").mkString
  private lazy val helloSysl: String     = readLsysl("oskit/bin/hello.lsysl")

  // Linker script for external programs — all load at same base address.
  private lazy val progScript: LinkerScript = LinkerScriptParser.parse(
    """SECTIONS
      |    code: 0xD0000
      |    rodata
      |    data
      |    bss
      |SYMBOL _heap_start = AFTER bss
      |SYMBOL _heap_end = 0x100000
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
    val allSources = progSources ++ Map(
      "oskit/ulib/ulib" -> ulibSysl,
      "posix/unistd/sbrk" -> ulibSbrkSysl,
      "posix/stdlib/alloc" -> posixAllocSysl,
      "posix/string/string" -> posixStringSysl,
      "posix/ctype/ctype" -> posixCtypeSysl,
    )
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
      "oskit/servers/pm"             -> pmSrvSysl,
      "posix/unistd/sbrk"           -> sbrkSysl,
      "posix/string/string"         -> posixStringSysl,
      "posix/ctype/ctype"           -> posixCtypeSysl,
      "posix/stdlib/alloc"          -> posixAllocSysl,
      "std/debug/debug"              -> debugSysl,
      "std/mem/mem"                  -> memSysl,
      "oskit/hal/mem"               -> halMemSysl,
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
          |    create_thread(init, kernel_stack_usp(0), kernel_stack_ssp(0), "init")
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
    case '!'                       => (KeyEvent.VK_1, 1)
    case '&'                       => (KeyEvent.VK_7, 1)
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
    val pending                       = scheduledKeys.sortBy(_._1).to(scala.collection.mutable.Queue)
    val keyInjector: Processor => Unit = proc => {
      val cycle = proc.cycles
      while pending.nonEmpty && cycle >= pending.head._1 do
        val (_, vk, press, mods) = pending.dequeue()
        kbd.enqueue(vk, press,
          shiftDown = (mods & 1) != 0, ctrlDown = (mods & 2) != 0,
          altDown = (mods & 4) != 0, metaDown = (mods & 8) != 0)
    }
    val ticks: Seq[Processor => Unit] = Seq(timer, intc, keyInjector)
    val testMmu = new SimpleMMU(mem); testMmu.setIdentityRange(0x7FE000L, 0xC00000L)
    dma.mmu = Some(testMmu)
    val cpu = new CPU(mem, ticks, mmu = Some(testMmu)) { this.limit = maxCycles; quiet = true }
    cpu.reset()
    cpu.run()
    (cpu, output.toString, ram)

  "Loader: run hello from shell" in {
    val tofBytes = helloTofText.getBytes("UTF-8")
    val keys = typeString("hello\n", startTick = 500000)
    val (_, output, _) = runWithKeys("", keys, maxCycles = 100000000,
      files = Map("/bin/hello" -> tofBytes))
    val cleaned = output.filterNot(_ == '\n')
    cleaned should include("Hello, world!")
  }

  "Loader: run hello from shell (TRB v1)" in {
    val linked = TOF.deserialize(helloTofText)
    val trb = TriscBinary.serialize(linked)
    val keys = typeString("hello\n", startTick = 500000)
    val (_, output, _) = runWithKeys("", keys, maxCycles = 30000000,
      files = Map("/bin/hello" -> trb))
    val cleaned = output.filterNot(_ == '\n')
    cleaned should include("Hello, world!")
  }

  // Regression: thread_count capped at MAX_THREADS; without reusing STATE_TERMINATED
  // slots, the fourth /bin/hello would use idx 8 and corrupt memory past threads[7].
  "Loader: hello six times reuses thread slots" in {
    val trb = TriscBinary.serialize(TOF.deserialize(helloTofText))
    val script = List.fill(6)("hello\n").mkString
    // Wider spacing than default: puts()-based hello finishes a line much faster than
    // 13× putc, so keys can outpace the shell unless we inject more slowly.
    val keys   = typeString(script, startTick = 500000, spacing = 12000)
    val (cpu, output, _) = runWithKeys("", keys, maxCycles = 200000000,
      files = Map("/bin/hello" -> trb))
    cpu.state shouldNot be(State.DoubleFault)
    "Hello, world!".r.findAllIn(output).length should be >= 6
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
    val keys = typeString("hello\n", startTick = 500000)
    val (cpu, output, _) = runWithKeys("", keys, maxCycles = maxCycles,
      files = Map("/bin/hello" -> tofBytes))
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
      "posix/unistd/sbrk"       -> ulibSbrkSysl,
    ))

  // echo program — prints its arguments via get_args()
  private val echoSysl: String =
    """module oskit.bin.echo
      |import oskit.ulib.{putc, get_args, exit}
      |main()
      |    val args = get_args()
      |    val p = *byte(args)
      |    for var i = 0; i < len(args); i++
      |        putc(p[i])
      |    putc('\n')
      |    exit()
      |""".stripMargin

  private lazy val echoTofText: String =
    compileProgram(Map("oskit/bin/echo/echo" -> echoSysl))

  "Loader: arg passing works" in {
    val tofBytes = echoTofText.getBytes("UTF-8")
    val keys = typeString("./echo foo bar\n", startTick = 500000)
    val (_, output, _) = runWithKeys("", keys, maxCycles = 30000000,
      files = Map("/bin/echo" -> tofBytes))
    val cleaned = output.filterNot(_ == '\n')
    cleaned should include("foo bar")
  }

  "Loader: ./echo with punctuation (get_args)" in {
    val tofBytes = echoTofText.getBytes("UTF-8")
    val keys = typeString("./echo asdf!2\n", startTick = 500000)
    val (_, output, _) = runWithKeys("", keys, maxCycles = 30000000,
      files = Map("/bin/echo" -> tofBytes))
    val cleaned = output.filterNot(_ == '\n')
    cleaned should include("asdf!2")
  }

  "String temp leak: function arg temporaries" in {
    val source =
      """// main defined first so that string(p, 100) sets needsAllocExtern
        |// before identity is compiled, ensuring identity gets string param cleanup.
        |main() -> int
        |    var buf: [100]byte
        |    for var i = 0; i < 100; i++
        |        buf[i] = byte(65 + i % 26)
        |    val p: *byte = &buf[0]
        |    // Call identity with string(ptr, len) 50 times.
        |    // string(p, 100) allocates ~108 bytes each time.
        |    // If temporaries leak, 50 * 108 = 5.4KB leaked.
        |    // With 32KB heap, ~300 leaks would exhaust it.
        |    var total = 0
        |    for var i = 0; i < 50; i++
        |        total += identity(string(p, 100))
        |    putchar(79)  // 'O'
        |    putchar(75)  // 'K'
        |    putchar(10)
        |    0
        |
        |identity(s: string) -> int
        |    len(s)
        |""".stripMargin

    val inlineSbrk =
      """module posix.unistd
        |var _brk: *byte = *byte(0x8000)
        |sbrk(increment: int) -> *byte
        |    if increment == 0
        |        return _brk
        |    val old = _brk
        |    _brk = _brk + increment
        |    old
        |""".stripMargin

    val (cpu, output) = runWithBoot(Map(
      "main" -> source,
      "posix/stdlib/alloc" -> posixAllocSysl,
      "posix/unistd/sbrk" -> inlineSbrk,
      "posix/string/string" -> posixStringSysl,
      "posix/ctype/ctype" -> posixCtypeSysl,
    ), maxCycles = 500000)
    output should not include "!"
    output should include ("OK")
  }

  "Loader: run fat hello (60KB with posix modules)" in {
    val tofBytes = fatHelloTofText.getBytes("UTF-8")
    val keys = typeString("hello\n", startTick = 500000)
    val (_, output, _) = runWithKeys("", keys, maxCycles = 100000000,
      files = Map("/bin/hello" -> tofBytes))
    val cleaned = output.filterNot(_ == '\n')
    cleaned should include("Hello, world!")
  }

  private lazy val countSysl: String = readLsysl("oskit/bin/count.lsysl")
  private lazy val countTrb: Array[Byte] =
    TriscBinary.serialize(TOF.deserialize(compileProgram(Map("oskit/bin/count/count" -> countSysl))))
  private lazy val psSysl: String = readLsysl("oskit/bin/ps.lsysl")
  private lazy val psTrb: Array[Byte] =
    TriscBinary.serialize(TOF.deserialize(compileProgram(Map("oskit/bin/ps/ps" -> psSysl))))

  "Loader: count & then ps does not crash" in {
    val keys = typeString("count &\nps\n", startTick = 500000)
    val (cpu, output, _) = runWithKeys("", keys, maxCycles = 100000000,
      files = Map("/bin/count" -> countTrb, "/bin/ps" -> psTrb))
    // Should reach cycle limit (Wfi), not crash (Halt)
    cpu.state.toString should be ("Wfi")
  }
}
