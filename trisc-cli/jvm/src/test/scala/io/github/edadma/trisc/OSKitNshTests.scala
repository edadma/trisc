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
  private lazy val loginSysl: String  = readLsysl("oskit/apps/login.lsysl")

  // Build OS with nsh launched directly (no login).
  private lazy val nshLinked: TOF =
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
      "oskit/apps/nsh"              -> nshSysl,
      "app" ->
        """import oskit.kernel.*
import oskit.ipc.*
import oskit.drivers.disk.disk_server
import oskit.servers.tfs_server
import oskit.drivers.tty.tty_server
import oskit.apps.nsh
import oskit.services.sleep
          |
          |init()
          |    create_thread(disk_server, 0x80000, 0x80000, "disk")
          |    sleep(5)
          |    create_thread(tfs_server, 0x90000, 0x90000, "tfs")
          |    create_thread(tty_server, 0xA0000, 0xA0000, "tty")
          |    sleep(5)
          |    create_thread(nsh, 0xB0000, 0xB0000, "nsh")
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(init, 0xC0000, 0xC0000, "init")
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

  // Build OS with login → nsh.
  private lazy val loginLinked: TOF =
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
      "oskit/apps/nsh"              -> nshSysl,
      "oskit/apps/login"            -> loginSysl,
      "app" ->
        """import oskit.kernel.*
import oskit.ipc.*
import oskit.drivers.disk.disk_server
import oskit.servers.tfs_server
import oskit.drivers.tty.tty_server
import oskit.apps.login
import oskit.services.sleep
          |
          |init()
          |    create_thread(disk_server, 0x80000, 0x80000, "disk")
          |    sleep(5)
          |    create_thread(tfs_server, 0x90000, 0x90000, "tfs")
          |    create_thread(tty_server, 0xA0000, 0xA0000, "tty")
          |    sleep(5)
          |    create_thread(login, 0xB0000, 0xB0000, "login")
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(init, 0xC0000, 0xC0000, "init")
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

  def runNsh(
      maxCycles: Int = 5200000,
      prefill: String = "\n",
      scheduledKeys: Seq[(Int, Int, Boolean, Int)] = Seq.empty,
  ): (CPU, String) =
    val linked = nshLinked

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
      sectorSize = 512,
      intc,
      irq = 3,
      prefill = prefill,
      maxInodes = 32,
    )
    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, ramdisk)
    linked.load(mem)

    val pending                  = scheduledKeys.sortBy(_._1).to(scala.collection.mutable.Queue)
    val keyInjector: CPU => Unit = cpu => {
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
    val ticks: Seq[CPU => Unit] = if scheduledKeys.nonEmpty then Seq(timer, intc, keyInjector) else Seq(timer, intc)
    val cpu                     = new CPU(mem, ticks) { this.limit = maxCycles }
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

  "NSH: cat reads file" in {
    val keys        = typeString("cat /hello\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys, prefill = "/hello file \"world\"\n")
    output should include("world")
  }

  "NSH: help command" in {
    val keys        = typeString("help\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys)
    output should include("H")
  }

  "NSH: whoami returns 0" in {
    val keys        = typeString("whoami\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 10000000)
    output should include("0")
  }

  "NSH: unknown command" in {
    val keys        = typeString("foo\n", startTick = 500000)
    val (_, output) = runNsh(scheduledKeys = keys)
    output should include("not found")
  }

  // === Login integration tests ===

  private val passwdPrefill = "/etc/passwd file \"root:x:0:0:root:/:/nsh\"\n/etc/shadow file \"root:toor\"\n"

  def runLogin(
      maxCycles: Int = 15000000,
      prefill: String = passwdPrefill,
      scheduledKeys: Seq[(Int, Int, Boolean, Int)] = Seq.empty,
  ): (CPU, String) =
    val linked = loginLinked

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
      sectorSize = 512,
      intc,
      irq = 3,
      prefill = prefill,
      maxInodes = 32,
    )
    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, ramdisk)
    linked.load(mem)

    val pending                  = scheduledKeys.sortBy(_._1).to(scala.collection.mutable.Queue)
    val keyInjector: CPU => Unit = cpu => {
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
    val ticks: Seq[CPU => Unit] = if scheduledKeys.nonEmpty then Seq(timer, intc, keyInjector) else Seq(timer, intc)
    val cpu                     = new CPU(mem, ticks) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  private def loginAndType(cmd: String, startTick: Int = 800000): Seq[(Int, Int, Boolean, Int)] =
    typeString("root\n", startTick = startTick) ++
    typeString("toor\n", startTick = startTick + 40000) ++
    typeString(cmd, startTick = startTick + 200000)

  "Login: prompts for credentials" in {
    val keys        = typeString("root\n", startTick = 800000)
    val (_, output) = runLogin(scheduledKeys = keys)
    output should include("login: ")
  }

  "Login: successful login shows shell prompt" in {
    val keys        = loginAndType("")
    val (_, output) = runLogin(scheduledKeys = keys)
    output should include("login: ")
    output should include("password: ")
    output should include("> ")
  }

  "Login: whoami returns 0 for root" in {
    val keys        = loginAndType("whoami\n")
    val (_, output) = runLogin(scheduledKeys = keys)
    output should include("0")
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
