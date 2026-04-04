package io.github.edadma.trisc

class OSKitShellTests extends OSKitTestHelpers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val ipcSysl: String = readLsysl("oskit/ipc/ipc.lsysl")
  private lazy val diskSysl: String = readLsysl("oskit/drivers/disk/disk.lsysl")
  private lazy val kbdSysl: String = readLsysl("oskit/drivers/kbd/keyboard.lsysl")
  private lazy val ttySysl: String = readLsysl("oskit/drivers/tty/tty.lsysl")
  private lazy val tfsSysl: String = readLsysl("oskit/fs/tfs.lsysl")
  private lazy val tfsSrvSysl: String = readLsysl("oskit/servers/tfs.lsysl")
  private lazy val stringSysl: String = readLsysl("oskit/lib/string.lsysl")
  private lazy val shSysl: String = readLsysl("oskit/apps/sh.lsysl")

  // String library tests use minimal boot (no OS)
  def runStringTest(source: String, maxCycles: Int = 500000): (CPU, String) =
    runWithBoot(Map("oskit/lib/string" -> stringSysl, "main" -> source))

  // Shell tests need the full stack: keyboard, TTY, disk, TFS, shell
  def runShell(
      userSources: Map[String, String],
      maxCycles: Int = 100000000,
      prefill: String = "\n",
      scheduledKeys: Seq[(Int, Int, Boolean, Int)] = Seq.empty,
  ): (CPU, String) =
    val bootTof = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel" -> kernelSysl,
      "oskit/services" -> servicesSysl,
      "oskit/timer" -> timerSysl,
      "oskit/semaphore" -> semaphoreSysl,
      "oskit/mutex" -> mutexSysl,
      "oskit/ipc" -> ipcSysl,
      "oskit/disk" -> diskSysl,
      "oskit/kbd" -> kbdSysl,
      "oskit/tty" -> ttySysl,
      "oskit/fs/tfs" -> tfsSysl,
      "oskit/tfs_srv" -> tfsSrvSysl,
      "oskit/lib/string" -> stringSysl,
      "oskit/sh" -> shSysl,
    ) ++ userSources
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(bootTof, syslTof), linkerScript, 0)

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
    val kbd = new KeyboardDevice(Runtime.keyboardAddress, intc, irq = 1)
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
    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, ramdisk)
    linked.load(mem)

    val pending = scheduledKeys.sortBy(_._1).to(scala.collection.mutable.Queue)
    var tickCount = 0L
    val keyInjector: CPU => Unit = _ => {
      tickCount += 1
      while pending.nonEmpty && tickCount >= pending.head._1 do
        val (_, vk, press, mods) = pending.dequeue()
        kbd.enqueue(vk, press,
          shiftDown = (mods & 1) != 0,
          ctrlDown = (mods & 2) != 0,
          altDown = (mods & 4) != 0,
          metaDown = (mods & 8) != 0)
    }
    val ticks: Seq[CPU => Unit] = if scheduledKeys.nonEmpty then Seq(timer, intc, keyInjector) else Seq(timer, intc)
    val cpu = new CPU(mem, ticks) { this.limit = maxCycles }
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
    case '\n' => (KeyEvent.VK_ENTER, 0)
    case ' ' => (KeyEvent.VK_SPACE, 0)
    case '/' => (KeyEvent.VK_SLASH, 0)
    case '-' => (KeyEvent.VK_MINUS, 0)
    case '.' => (KeyEvent.VK_PERIOD, 0)
    case _ => (KeyEvent.VK_SPACE, 0)

  // === String library tests ===

  "strtok: single token" in {
    val (_, output) = runStringTest(
      """import oskit.lib.*
        |
        |main() -> int
        |    var s: [6]i8
        |    s[0] = 104  // h
        |    s[1] = 101  // e
        |    s[2] = 108  // l
        |    s[3] = 108  // l
        |    s[4] = 111  // o
        |    s[5] = 0
        |    var d: [2]i8
        |    d[0] = 32
        |    d[1] = 0
        |    val t = strtok(&s[0], &d[0])
        |    if i64(t) != 0
        |        var i = 0
        |        while t[i] != 0
        |            putchar(t[i])
        |            i += 1
        |    0
        |""".stripMargin)
    output shouldBe "hello"
  }

  "strtok: multiple tokens" in {
    val (_, output) = runStringTest(
      """import oskit.lib.*
        |
        |main() -> int
        |    var s: [8]i8
        |    s[0] = 108  // l
        |    s[1] = 115  // s
        |    s[2] = 32   // space
        |    s[3] = 45   // -
        |    s[4] = 108  // l
        |    s[5] = 32   // space
        |    s[6] = 47   // /
        |    s[7] = 0
        |    var d: [2]i8
        |    d[0] = 32
        |    d[1] = 0
        |    var tok = strtok(&s[0], &d[0])
        |    var count = 0
        |    while i64(tok) != 0
        |        if count > 0
        |            putchar(44)  // comma
        |        var i = 0
        |        while tok[i] != 0
        |            putchar(tok[i])
        |            i += 1
        |        count += 1
        |        tok = strtok(*i8(0), &d[0])
        |    0
        |""".stripMargin)
    output shouldBe "ls,-l,/"
  }

  "strtok: leading and trailing spaces" in {
    val (_, output) = runStringTest(
      """import oskit.lib.*
        |
        |main() -> int
        |    var s: [8]i8
        |    s[0] = 32   // space
        |    s[1] = 32   // space
        |    s[2] = 104  // h
        |    s[3] = 105  // i
        |    s[4] = 32   // space
        |    s[5] = 32   // space
        |    s[6] = 0
        |    var d: [2]i8
        |    d[0] = 32
        |    d[1] = 0
        |    var tok = strtok(&s[0], &d[0])
        |    var count = 0
        |    while i64(tok) != 0
        |        var i = 0
        |        while tok[i] != 0
        |            putchar(tok[i])
        |            i += 1
        |        count += 1
        |        tok = strtok(*i8(0), &d[0])
        |    putchar(48 + count)  // print count
        |    0
        |""".stripMargin)
    output shouldBe "hi1"
  }

  "strtok: empty string" in {
    val (_, output) = runStringTest(
      """import oskit.lib.*
        |
        |main() -> int
        |    var s: [1]i8
        |    s[0] = 0
        |    var d: [2]i8
        |    d[0] = 32
        |    d[1] = 0
        |    val tok = strtok(&s[0], &d[0])
        |    if i64(tok) == 0
        |        putchar(89)  // Y
        |    else
        |        putchar(78)  // N
        |    0
        |""".stripMargin)
    output shouldBe "Y"
  }

  "streq: equal strings" in {
    val (_, output) = runStringTest(
      """import oskit.lib.*
        |
        |main() -> int
        |    var a: [3]i8
        |    a[0] = 108
        |    a[1] = 115
        |    a[2] = 0
        |    var b: [3]i8
        |    b[0] = 108
        |    b[1] = 115
        |    b[2] = 0
        |    if streq(&a[0], &b[0]) == 1
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin)
    output shouldBe "Y"
  }

  "streq: different strings" in {
    val (_, output) = runStringTest(
      """import oskit.lib.*
        |
        |main() -> int
        |    var a: [3]i8
        |    a[0] = 108
        |    a[1] = 115
        |    a[2] = 0
        |    var b: [3]i8
        |    b[0] = 99
        |    b[1] = 100
        |    b[2] = 0
        |    if streq(&a[0], &b[0]) == 0
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin)
    output shouldBe "Y"
  }

  // === Shell integration tests ===

  "Shell: echo command via putc" in {
    // Simpler test: type "echo hi\n" and check output
    val keys = typeString("echo hi\n", startTick = 2000, spacing = 300)
    val (cpu, output) = runShell(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(tfs_server, 0x20000, 0x1E000, "tfs")
          |    create_thread(tty_server, 0x30000, 0x2E000, "tty")
          |    create_thread(shell, 0x40000, 0x3E000, "sh")
          |    timer_init(1000)
          |    first_thread_ssp()
          |""".stripMargin
    ), scheduledKeys = keys, maxCycles = 100000000)
    println(s"SHELL output (${output.length} chars): '${output.take(200)}' cycles=${cpu.cycles}")
    output should include("hi")
  }

  "Shell: echo command" in {
    // Type "echo hi\n" starting at tick 5000 (give servers time to init)
    val keys = typeString("echo hi\n", startTick = 1000, spacing = 200)
    val (_, output) = runShell(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(tfs_server, 0x20000, 0x1E000, "tfs")
          |    create_thread(tty_server, 0x30000, 0x2E000, "tty")
          |    create_thread(shell, 0x40000, 0x3E000, "sh")
          |    timer_init(1000)
          |    first_thread_ssp()
          |""".stripMargin
    ), scheduledKeys = keys)
    // Output should contain the prompt, echoed input, and "hi"
    output should include("hi")
  }

  "Shell: ls on root with prefilled file" in {
    val keys = typeString("ls\n", startTick = 1000, spacing = 200)
    val (_, output) = runShell(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(tfs_server, 0x20000, 0x1E000, "tfs")
          |    create_thread(tty_server, 0x30000, 0x2E000, "tty")
          |    create_thread(shell, 0x40000, 0x3E000, "sh")
          |    timer_init(1000)
          |    first_thread_ssp()
          |""".stripMargin
    ), prefill = """/hello file "world"""", scheduledKeys = keys)
    output should include("hello")
  }

  "Shell: pwd shows root" in {
    val keys = typeString("pwd\n", startTick = 1000, spacing = 200)
    val (_, output) = runShell(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(tfs_server, 0x20000, 0x1E000, "tfs")
          |    create_thread(tty_server, 0x30000, 0x2E000, "tty")
          |    create_thread(shell, 0x40000, 0x3E000, "sh")
          |    timer_init(1000)
          |    first_thread_ssp()
          |""".stripMargin
    ), scheduledKeys = keys)
    // Output should show "/" from pwd
    // The prompt is "/ $ " and pwd prints "/"
    val pwdLines = output.split('\n').filter(_.trim == "/")
    pwdLines.length should be >= 1
  }
}
