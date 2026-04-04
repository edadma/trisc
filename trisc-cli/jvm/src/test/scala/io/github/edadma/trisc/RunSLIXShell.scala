package io.github.edadma.trisc

import java.awt.event.KeyEvent

/** Build and run the SLIX shell demo in text terminal mode.
  * Run with: sbt "triscCliJVM/testOnly *RunSLIXShell*"
  *
  * This boots SLIX with disk, filesystem, TTY, and shell servers,
  * then connects keyboard input from stdin (via a background thread)
  * and stdout to the terminal.
  */
class RunSLIXShell extends OSKitTestHelpers {

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
  private lazy val initSysl: String = readLsysl("oskit/apps/init.lsysl")
  private lazy val slixMainSysl: String = readLsysl("examples/slix-shell/main.lsysl")

  "run SLIX shell" ignore {
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
      "oskit/init" -> initSysl,
      "app" -> slixMainSysl,
    )
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(bootTof, syslTof), linkerScript, 0)

    // Set up devices
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
      prefill =
        """/etc dir
          |/etc/motd file "Welcome to SLIX!"
          |/bin dir
          |/tmp dir
          |""".stripMargin,
      maxInodes = 32,
    )

    // Stdout → terminal
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = Runtime.stdoutAddress
      val size: Long = 4
      def writeByte(addr: Long, data: Long): Unit = {
        print(data.toChar)
        System.out.flush()
      }
      override def loadByte(addr: Long, data: Long): Unit = ()
    }

    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, ramdisk)
    linked.load(mem)
    val cpu = new CPU(mem, Seq(timer, intc))
    cpu.reset()

    // Background thread: read keypresses from stdin, inject into keyboard device
    @volatile var stopInput = false
    val inputThread = new Thread(() => {
      // Put terminal in raw mode (best effort — works on Unix terminals)
      try {
        val rt = java.lang.Runtime.getRuntime
        rt.exec(Array("sh", "-c", "stty -echo raw < /dev/tty")).waitFor()
      } catch { case _: Exception => }

      try {
        val in = System.in
        var running = true
        while running do
          val b = in.read()
          if b == -1 || b == 3 then // EOF or Ctrl-C
            running = false
            stopInput = true
          else
            // Map byte to VK code
            val (vk, shift) = b match
              case 13 | 10 => (KeyEvent.VK_ENTER, false)
              case 127 | 8 => (KeyEvent.VK_BACK_SPACE, false)
              case 27      => (KeyEvent.VK_ESCAPE, false)
              case 9       => (KeyEvent.VK_TAB, false)
              case 32      => (KeyEvent.VK_SPACE, false)
              case c if c >= 'a' && c <= 'z' => (KeyEvent.VK_A + (c - 'a'), false)
              case c if c >= 'A' && c <= 'Z' => (KeyEvent.VK_A + (c - 'A'), true)
              case c if c >= '0' && c <= '9' => (KeyEvent.VK_0 + (c - '0'), false)
              case '/' => (KeyEvent.VK_SLASH, false)
              case '-' => (KeyEvent.VK_MINUS, false)
              case '.' => (KeyEvent.VK_PERIOD, false)
              case ',' => (KeyEvent.VK_COMMA, false)
              case '=' => (KeyEvent.VK_EQUALS, false)
              case _ => (KeyEvent.VK_SPACE, false) // fallback
            kbd.enqueue(vk, press = true, shiftDown = shift, ctrlDown = false, altDown = false, metaDown = false)
            kbd.enqueue(vk, press = false, shiftDown = shift, ctrlDown = false, altDown = false, metaDown = false)
      } catch { case _: Exception => }
    })
    inputThread.setDaemon(true)
    inputThread.start()

    // Run — no cycle limit
    try {
      cpu.run()
    } finally {
      // Restore terminal
      try {
        val rt = java.lang.Runtime.getRuntime
        rt.exec(Array("sh", "-c", "stty echo cooked < /dev/tty")).waitFor()
      } catch { case _: Exception => }
      println()
      println(s"[SLIX halted after ${cpu.cycles} cycles]")
    }
  }
}
