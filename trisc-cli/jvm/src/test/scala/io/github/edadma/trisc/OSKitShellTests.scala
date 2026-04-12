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
  private lazy val shSysl: String     = readLsysl("oskit/apps/sh.lsysl")
  private lazy val memSysl: String   = readLsysl("std/mem/mem.lsysl")
  private lazy val halMemSysl: String = readLsysl("oskit/hal/mem_dma.lsysl")
  private lazy val configSysl: String = scala.io.Source.fromFile("oskit/config/config.sysl").mkString
  private lazy val debugSysl: String = readLsysl("std/debug/debug.lsysl")

  // Cache the compiled+linked OS image — all shell tests use the same app source.
  // Uses inline init (no ttytab) since sh.lsysl is the legacy shell.
  private lazy val shellLinked: TOF =
    val bootTof    = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel/kernel"      -> kernelSysl,
      "oskit/services/services"  -> servicesSysl,
      "oskit/kernel/timer"       -> timerSysl,
      "oskit/sync/semaphore"     -> semaphoreSysl,
      "oskit/sync/mutex"         -> mutexSysl,
      "oskit/ipc/ipc"            -> ipcSysl,
      "std/mem/mem"              -> memSysl,
      "oskit/hal/mem"            -> halMemSysl,
      "oskit/config/config"      -> configSysl,
      "std/debug/debug"          -> debugSysl,
      "oskit/drivers/disk/disk"  -> diskSysl,
      "oskit/drivers/kbd/keyboard" -> kbdSysl,
      "oskit/drivers/tty/tty"    -> ttySysl,
      "oskit/fs/tfs"             -> tfsSysl,
      "oskit/servers/tfs"        -> tfsSrvSysl,
      "posix/unistd/sbrk"        -> sbrkSysl,
      "posix/string/string"      -> posixStringSysl,
      "posix/ctype/ctype"        -> posixCtypeSysl,
      "posix/stdlib/alloc"       -> posixAllocSysl,
      "oskit/apps/sh"            -> shSysl,
      "app" ->
        """import oskit.kernel.*
import oskit.ipc.*
import oskit.drivers.disk.disk_server
import oskit.servers.tfs_server
import oskit.drivers.tty.tty_server
import oskit.apps.shell
import oskit.services.sleep
          |
          |init()
          |    create_thread(disk_server, 0x600000, 0x600000, "disk")
          |    sleep(5)
          |    create_thread(tfs_server, 0x610000, 0x610000, "tfs")
          |    create_thread(tty_server, 0x620000, 0x620000, "tty")
          |    sleep(5)
          |    create_thread(shell, 0x630000, 0x630000, "sh")
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
      sectorSize = 4096,
      intc,
      irq = 3,
      prefill = prefill,
      maxInodes = 32,
    )
    val dma = new DMA(Runtime.dmaAddress, null, intc, 4)
    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, ramdisk, dma)
    dma.mem = mem
    linked.load(mem)

    val pending                  = scheduledKeys.sortBy(_._1).to(scala.collection.mutable.Queue)
    val keyInjector: Processor => Unit = cpu => {
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
    val ticks: Seq[Processor => Unit] = if scheduledKeys.nonEmpty then Seq(timer, intc, keyInjector) else Seq(timer, intc)
    val testMmu = new SimpleMMU(mem); testMmu.setIdentityRange(0x7FE000L, 0xC00000L)
    dma.mmu = Some(testMmu)
    val cpu                     = new CPU(mem, ticks, mmu = Some(testMmu)) { this.limit = maxCycles; quiet = true }
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
    case '!'                       => (KeyEvent.VK_1, 1)
    case _                         => (KeyEvent.VK_SPACE, 0)

  // === Shell integration tests ===

  "Shell: init boots system" taggedAs Slow in {
    val keys          = typeString("pwd\n", startTick = 500000, spacing = 2000)
    val (cpu, output) = runShell(scheduledKeys = keys)
    output should include("/")
  }

  "Shell: echo command via putc" taggedAs Slow in {
    val keys          = typeString("echo hi\n", startTick = 500000, spacing = 2000)
    val (cpu, output) = runShell(scheduledKeys = keys)
    output should include("hi")
  }

  "Shell: echo command" taggedAs Slow in {
    val keys        = typeString("echo hi\n", startTick = 500000, spacing = 2000)
    val (_, output) = runShell(scheduledKeys = keys)
    output should include("hi")
  }

  "Shell: ls on root with prefilled file" taggedAs Slow in {
    val keys        = typeString("ls\n", startTick = 500000, spacing = 2000)
    val (_, output) = runShell(
      prefill = """/hello file "world"""",
      scheduledKeys = keys,
    )
    output should include("hello")
  }

  "Shell: pwd shows root" taggedAs Slow in {
    val keys        = typeString("pwd\n", startTick = 500000, spacing = 2000)
    val (_, output) = runShell(scheduledKeys = keys)
    // Output should show "/" from pwd
    // The prompt is "> " and pwd prints "/"
    val pwdLines = output.split('\n').filter(_.trim == "/")
    pwdLines.length should be >= 1
  }

  "Shell: type 20 characters without crash" taggedAs Slow in {
    val keys        = typeString("echo abcdefghijklmn\n", startTick = 500000, spacing = 50000)
    val (_, output) = runShell(maxCycles = 20000000, scheduledKeys = keys)
    output should include("abcdefghijklmn")
  }

  "Shell: keyboard buffer overflow drops keys gracefully" taggedAs Slow in {
    // With KB_BUF_SIZE=16, burst 20 keypresses at the same tick to overflow the buffer.
    // The system must not crash — excess events are silently dropped.
    // Then type a normal command to prove the shell is still alive.
    val burst = typeString("abcdefghijklmnopqrst", startTick = 500000, spacing = 1)
    val cmd   = typeString("\npwd\n", startTick = 600000, spacing = 50000)
    val (cpu, output) = runShell(maxCycles = 20000000, scheduledKeys = burst ++ cmd)
    // Shell must still be responsive — pwd should produce "/"
    output should include("/")
    // Must not crash
    cpu.state should not be State.Halt
  }
}
