package io.github.edadma.trisc

class OSKitShellTests extends OSKitTestHelpers {

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
  private lazy val stringSysl: String = readLsysl("oskit/lib/string.lsysl")
  private lazy val shSysl: String     = readLsysl("oskit/apps/sh.lsysl")
  private lazy val initSysl: String   = readLsysl("oskit/apps/init.lsysl")

  // Cache the compiled+linked OS image — all shell tests use the same app source
  private lazy val shellLinked: TOF =
    val bootTof    = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel/kernel"      -> kernelSysl,
      "oskit/services/services"  -> servicesSysl,
      "oskit/kernel/timer"       -> timerSysl,
      "oskit/sync/semaphore"     -> semaphoreSysl,
      "oskit/sync/mutex"         -> mutexSysl,
      "oskit/ipc/ipc"            -> ipcSysl,
      "oskit/drivers/disk/disk"  -> diskSysl,
      "oskit/drivers/kbd/keyboard" -> kbdSysl,
      "oskit/drivers/tty/tty"    -> ttySysl,
      "oskit/fs/tfs"             -> tfsSysl,
      "oskit/servers/tfs"        -> tfsSrvSysl,
      "oskit/lib/string"         -> stringSysl,
      "oskit/apps/sh"            -> shSysl,
      "oskit/apps/init"          -> initSysl,
      "app" ->
        """import oskit.kernel.*
import oskit.ipc.*
import oskit.apps.*
          |var _n: [2]i8
          |
          |kernel_main() -> int
          |    _n[0] = 73
          |    _n[1] = 0
          |    ipc_init()
          |    create_thread(init, 0x90000, 0x8E000, &_n[0])
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

  // Shell tests need the full stack: keyboard, TTY, disk, TFS, shell
  def runShell(
      maxCycles: Int = 5200000,
      prefill: String = "\n",
      scheduledKeys: Seq[(Int, Int, Boolean, Int)] = Seq.empty,
  ): (CPU, String) =
    val linked = shellLinked

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
      // Use CPU cycle count so key delivery is deterministic
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

  // Helper: convert a typed string to scheduled key events (press + release)
  // starting at the given tick, with spacing between each key
  def typeString(s: String, startTick: Int, spacing: Int = 100): Seq[(Int, Int, Boolean, Int)] =
    s.flatMap { ch =>
      val (scancode, mods) = charToVK(ch)
      Seq((0, scancode, true, mods), (0, scancode, false, mods))
    }.zipWithIndex.map { case ((_, sc, press, mods), i) =>
      (startTick + i * spacing, sc, press, mods)
    }

  // Map ASCII character to Java VK code + modifier bits
  // KeyboardDevice.enqueue takes VK codes and converts to HID internally
  import java.awt.event.KeyEvent
  private def charToVK(ch: Char): (Int, Int) = ch match
    case c if c >= 'a' && c <= 'z' => (KeyEvent.VK_A + (c - 'a'), 0)
    case c if c >= 'A' && c <= 'Z' => (KeyEvent.VK_A + (c - 'A'), 1) // shift
    case c if c >= '0' && c <= '9' => (KeyEvent.VK_0 + (c - '0'), 0)
    case '\n'                      => (KeyEvent.VK_ENTER, 0)
    case ' '                       => (KeyEvent.VK_SPACE, 0)
    case '/'                       => (KeyEvent.VK_SLASH, 0)
    case '-'                       => (KeyEvent.VK_MINUS, 0)
    case '.'                       => (KeyEvent.VK_PERIOD, 0)
    case _                         => (KeyEvent.VK_SPACE, 0)

  // === Shell integration tests ===

  "Shell: init boots system" in {
    val keys          = typeString("pwd\n", startTick = 500000, spacing = 2000)
    val (cpu, output) = runShell(scheduledKeys = keys)
    output should include("/")
  }

  "Shell: echo command via putc" in {
    val keys          = typeString("echo hi\n", startTick = 500000, spacing = 2000)
    val (cpu, output) = runShell(scheduledKeys = keys)
    output should include("hi")
  }

  "Shell: echo command" in {
    val keys        = typeString("echo hi\n", startTick = 500000, spacing = 2000)
    val (_, output) = runShell(scheduledKeys = keys)
    output should include("hi")
  }

  "Shell: ls on root with prefilled file" in {
    val keys        = typeString("ls\n", startTick = 500000, spacing = 2000)
    val (_, output) = runShell(
      prefill = """/hello file "world"""",
      scheduledKeys = keys,
    )
    output should include("hello")
  }

  "Shell: pwd shows root" in {
    val keys        = typeString("pwd\n", startTick = 500000, spacing = 2000)
    val (_, output) = runShell(scheduledKeys = keys)
    // Output should show "/" from pwd
    // The prompt is "/ $ " and pwd prints "/"
    val pwdLines = output.split('\n').filter(_.trim == "/")
    pwdLines.length should be >= 1
  }
}
