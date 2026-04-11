package io.github.edadma.trisc

class OSKitNshTests extends OSKitTestHelpers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val ipcSysl: String    = readLsysl("oskit/ipc/ipc.lsysl")
  private lazy val diskSysl: String   = readLsysl("oskit/drivers/disk/disk.lsysl")
  private lazy val kbdSysl: String    = readLsysl("oskit/drivers/kbd/keyboard.lsysl")
  private lazy val ttySysl: String    = readLsysl("oskit/drivers/tty/tty.lsysl")
  private lazy val tfsSysl: String    = readLsysl("oskit/fs/tfs.lsysl")
  private lazy val tfsSrvSysl: String = readLsysl("oskit/servers/tfs.lsysl")
  private lazy val nshSysl: String    = readLsysl("oskit/apps/nsh.lsysl")
  private lazy val initSysl: String   = readLsysl("oskit/apps/init.lsysl")
  private lazy val loginSysl: String  = readLsysl("oskit/apps/login.lsysl")
  private lazy val debugSysl: String    = readLsysl("std/debug/debug.lsysl")
  private lazy val memSysl: String     = readLsysl("std/mem/mem.lsysl")
  private lazy val binarySysl: String  = readLsysl("std/encoding/binary/binary.lsysl")
  private lazy val stringsSysl: String = readLsysl("std/strings/strings.lsysl")
  private lazy val builderSysl: String = readLsysl("std/builder/builder.lsysl")
  private lazy val strconvSysl: String = readLsysl("std/strconv/strconv.lsysl")
  private lazy val resultSysl: String  = readLsysl("std/result/result.lsysl")
  private lazy val errorsSysl: String  = readLsysl("std/errors/errors.lsysl")
  private lazy val utf8Sysl: String    = readLsysl("std/utf8/utf8.lsysl")
  private lazy val loaderSysl: String  = readLsysl("oskit/loader/loader.lsysl")
  private lazy val sha256Sysl: String  = readLsysl("std/crypto/sha256/sha256.lsysl")
  private lazy val hmacSysl: String   = readLsysl("std/crypto/hmac/hmac.lsysl")
  private lazy val pbkdf2Sysl: String = readLsysl("std/crypto/pbkdf2/pbkdf2.lsysl")

  // Shared OS source set — init reads /etc/ttytab to decide what to spawn.
  private def buildOS(): TOF =
    val bootTof    = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel/kernel"         -> kernelSysl,
      "oskit/services/services"     -> servicesSysl,
      "oskit/kernel/timer"          -> timerSysl,
      "oskit/sync/semaphore"        -> semaphoreSysl,
      "oskit/sync/mutex"            -> mutexSysl,
      "oskit/ipc/ipc"               -> ipcSysl,
      "oskit/drivers/disk/disk"     -> diskSysl,
      "oskit/drivers/kbd/keyboard"  -> kbdSysl,
      "oskit/drivers/tty/tty"       -> ttySysl,
      "oskit/fs/tfs"                -> tfsSysl,
      "oskit/servers/tfs"           -> tfsSrvSysl,
      "posix/unistd/sbrk"          -> sbrkSysl,
      "posix/string/string"        -> posixStringSysl,
      "posix/ctype/ctype"          -> posixCtypeSysl,
      "posix/stdlib/alloc"         -> posixAllocSysl,
      "std/debug/debug"             -> debugSysl,
      "std/mem/mem"                 -> memSysl,
      "std/encoding/binary/binary" -> binarySysl,
      "std/crypto/sha256/sha256"   -> sha256Sysl,
      "std/crypto/hmac/hmac"       -> hmacSysl,
      "std/crypto/pbkdf2/pbkdf2"   -> pbkdf2Sysl,
      "std/strings/strings"         -> stringsSysl,
      "std/builder/builder"         -> builderSysl,
      "std/strconv/strconv"         -> strconvSysl,
      "std/result/result"           -> resultSysl,
      "std/errors/errors"           -> errorsSysl,
      "std/utf8/utf8"               -> utf8Sysl,
      "oskit/loader/loader"         -> loaderSysl,
      "oskit/apps/nsh/nsh"           -> nshSysl,
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
    val driver  = new SyslDriver
    val result  = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs    =
      for unit <- result.units yield
        val asm = codegen.generate(unit.typed)
        assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    Linker.link(Seq(bootTof, syslTof), linkerScript, 0)

  private lazy val osLinked: TOF = buildOS()

  private val nshTtytab = "/etc/ttytab file \"tty0 nsh\"\n"

  def runNsh(
      maxCycles: Int = 5200000,
      prefill: String = "\n",
      scheduledKeys: Seq[(Int, Int, Boolean, Int)] = Seq.empty,
  ): (CPU, String) =
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
      Runtime.ramdiskAddress,
      ram,
      sectors = 64,
      sectorSize = 4096,
      intc,
      irq = 3,
      prefill = nshTtytab + prefill,
      maxInodes = 64,
      files = RamdiskBinPrograms.loadEmbeddedBinaries(),
    )
    val sha = new ShaAccelerator(Runtime.shaAccelAddress)
    val dma = new DMA(Runtime.dmaAddress, null, intc, 4)
    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, ramdisk, sha, dma)
    dma.mem = mem
    linked.load(mem)

    val pending                  = scheduledKeys.sortBy(_._1).to(scala.collection.mutable.Queue)
    val keyInjector: Processor => Unit = cpu => {
      val cycle = cpu.cycles
      while pending.nonEmpty && cycle >= pending.head._1 do
        val (_, vk, press, mods) = pending.dequeue()
        kbd.enqueue(
          vk,
          press,
          shiftDown = (mods & 1) != 0,
          ctrlDown = (mods & 2) != 0,
          altDown = (mods & 4) != 0,
          metaDown = (mods & 8) != 0,
        )
    }
    val ticks: Seq[Processor => Unit] = if scheduledKeys.nonEmpty then Seq(timer, intc, keyInjector) else Seq(timer, intc)
    val testMmu = new SimpleMMU(mem); testMmu.setIdentityRange(0x7FE000L, 0xC00000L)
    dma.mmu = Some(testMmu)
    val cpu                     = new CPU(mem, ticks, mmu = Some(testMmu)) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

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
    case _                         => (KeyEvent.VK_SPACE, 0)

  def typeString(s: String, startTick: Int, spacing: Int = 2000): Seq[(Int, Int, Boolean, Int)] =
    s.flatMap { ch =>
      val (scancode, mods) = charToVK(ch)
      Seq((0, scancode, true, mods), (0, scancode, false, mods))
    }.zipWithIndex.map { case ((_, sc, press, mods), i) =>
      (startTick + i * spacing, sc, press, mods)
    }

  // === NSH integration tests ===

  "NSH: pwd shows root" in {
    val keys        = typeString("pwd\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys)
    output should include("/")
  }

  "NSH: echo command" in {
    val keys        = typeString("echo hi\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys)
    output should include("hi")
  }

  "NSH: ls on root with prefilled file" in {
    val keys        = typeString("ls\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys, prefill = "/hello file \"world\"\n")
    output should include("hello")
  }

  "NSH: hello prints greeting" in {
    val keys        = typeString("hello\n", startTick = 500000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 80000000)
    output should include("Hello")
  }


  // TODO: un-ignore when TFS prefilled file read is fixed
  "NSH: cat prefilled /hello (short file)" ignore {
    val keys = typeString("cat /hello\n", startTick = 500000, spacing = 12000)
    val (cpu, output) =
      runNsh(scheduledKeys = keys, prefill = "/hello file \"world\"\n", maxCycles = 200000000)
    output should include("world")
  }

  "NSH: cat reads file" in {
    val keys =
      typeString("touch /hello\nwrite /hello world\ncat /hello\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 25000000)
    output should include("world")
  }

  // TODO: un-ignore when TFS prefilled file read is fixed
  "NSH: cat /etc/ttytab shows prefilled line" ignore {
    val keys = typeString("cat /etc/ttytab\n", startTick = 500000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 100000000)
    output should include("tty0 nsh")
  }

  "NSH: help command" in {
    val keys        = typeString("help\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys)
    output should include("echo cat ls")
  }

  "NSH: whoami returns 0" in {
    val keys        = typeString("whoami\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 10000000)
    output should include("0")
  }

  "NSH: unknown command" in {
    val keys        = typeString("foo\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 15000000)
    output should include("not found")
  }

  "NSH: cd dev then pwd" in {
    val keys        = typeString("cd dev\npwd\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys, prefill = "/dev dir\n", maxCycles = 10000000)
    output should include("/dev")
  }

  "NSH: touch creates file" in {
    val keys        = typeString("touch /hello\nls\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 10000000)
    output should include("hello")
  }

  "NSH: write and cat" in {
    val keys        = typeString("touch /msg\nwrite /msg hi\ncat /msg\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 15000000)
    output should include("hi")
  }

  "NSH: mv renames file" in {
    val keys =
      typeString("touch /old\nwrite /old data\nmv /old /new\ncat /new\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 30000000)
    output should include("data")
  }

  "NSH: uptime shows ticks" in {
    val keys        = typeString("uptime\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 10000000)
    output should include("up ")
    output should include("ticks")
  }

  // === Login integration tests ===

  private val passwdPrefill =
    "/etc/ttytab file \"tty0 login\"\n" +
    "/root dir\n" +
    "/home dir\n" +
    "/home/ed dir\n" +
    "/etc/passwd file \"root:x:0:0:root:/root:/nsh\\ned:x:1000:1000:ed:/home/ed:/nsh\"\n" +
    "/etc/shadow file \"root:slix:3b1b8291c0bdb62febcd914f45884bca403ae1c42a4bb1c41755881f3886d158\\ned:slix:c638d5b6e91f70b96934aac8d7be42363ce4ea5927f9a9bbbe2d64a8b51926b5\"\n"

  def runLogin(
      maxCycles: Int = 15000000,
      prefill: String = passwdPrefill,
      scheduledKeys: Seq[(Int, Int, Boolean, Int)] = Seq.empty,
  ): (CPU, String) =
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
      Runtime.ramdiskAddress,
      ram,
      sectors = 64,
      sectorSize = 4096,
      intc,
      irq = 3,
      prefill = prefill,
      maxInodes = 64,
      files = RamdiskBinPrograms.loadEmbeddedBinaries(),
    )
    val sha = new ShaAccelerator(Runtime.shaAccelAddress)
    val dma = new DMA(Runtime.dmaAddress, null, intc, 4)
    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, ramdisk, sha, dma)
    dma.mem = mem
    linked.load(mem)

    val pending                  = scheduledKeys.sortBy(_._1).to(scala.collection.mutable.Queue)
    val keyInjector: Processor => Unit = cpu => {
      val cycle = cpu.cycles
      while pending.nonEmpty && cycle >= pending.head._1 do
        val (_, vk, press, mods) = pending.dequeue()
        kbd.enqueue(
          vk,
          press,
          shiftDown = (mods & 1) != 0,
          ctrlDown = (mods & 2) != 0,
          altDown = (mods & 4) != 0,
          metaDown = (mods & 8) != 0,
        )
    }
    val ticks: Seq[Processor => Unit] = if scheduledKeys.nonEmpty then Seq(timer, intc, keyInjector) else Seq(timer, intc)
    val testMmu = new SimpleMMU(mem); testMmu.setIdentityRange(0x7FE000L, 0xC00000L)
    dma.mmu = Some(testMmu)
    val cpu                     = new CPU(mem, ticks, mmu = Some(testMmu)) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  private def loginAndType(cmd: String, startTick: Int = 800000): Seq[(Int, Int, Boolean, Int)] =
    // Slower than default 2000: login + external cat need time for TTY/prompts (fast keys corrupt the line).
    val sp = 12000
    typeString("root\n", startTick = startTick, spacing = sp) ++
    typeString("toor\n", startTick = startTick + 180000, spacing = sp) ++
    typeString(cmd, startTick = startTick + 450000, spacing = sp)

  "Login: prompts for credentials" in {
    val keys        = typeString("root\n", startTick = 800000)
    val (_, output) = runLogin(scheduledKeys = keys)
    output should include("login: ")
  }

  "Login: successful login shows shell prompt in home dir" in {
    val keys        = loginAndType("")
    val (_, output) = runLogin(scheduledKeys = keys)
    output should include("login: ")
    output should include("password: ")
    output should include("/root> ")
  }

  "Login: whoami returns 0 for root" in {
    val keys        = loginAndType("whoami\n")
    val (_, output) = runLogin(scheduledKeys = keys)
    output should include("0")
  }

  "Login: pwd shows home directory" in {
    val keys        = loginAndType("pwd\n")
    val (_, output) = runLogin(scheduledKeys = keys)
    output should include("/root")
  }

  // TODO: un-ignore when TFS prefilled file read is fixed
  "Login: cat /etc/ttytab prints ttytab contents" ignore {
    val keys        = loginAndType("cat /etc/ttytab\n")
    val (_, output) = runLogin(scheduledKeys = keys, maxCycles = 180000000)
    output should include("tty0 login")
  }

  "Login: hello after login" in {
    val keys = loginAndType("hello\n")
    val (_, output) = runLogin(scheduledKeys = keys, maxCycles = 180000000)
    output should include("Hello")
  }

  "Login: user ed gets home /home/ed" in {
    val keys = typeString("ed\n", startTick = 800000) ++
               typeString("ed\n", startTick = 840000)
    val (_, output) = runLogin(scheduledKeys = keys)
    output should include("/home/ed> ")
  }

  "Login: bad password rejected" in {
    val keys = typeString("root\n", startTick = 800000) ++
               typeString("wrong\n", startTick = 840000)
    val (_, output) = runLogin(scheduledKeys = keys)
    output should include("Login incorrect")
  }

  "Login: bad username rejected" in {
    val keys = typeString("nobody\n", startTick = 800000) ++
               typeString("x\n", startTick = 840000)
    val (_, output) = runLogin(scheduledKeys = keys)
    output should include("Login incorrect")
  }
}
