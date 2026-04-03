package io.github.edadma.trisc

import java.awt.event.KeyEvent

class OSKitTTYTests extends OSKitTestHelpers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val ipcSysl: String = readLsysl("oskit/ipc/ipc.lsysl")
  private lazy val kbdSysl: String = readLsysl("oskit/drivers/kbd/keyboard.lsysl")
  private lazy val ttySysl: String = readLsysl("oskit/drivers/tty/tty.lsysl")

  // keyEvents: (vkCode, press, modifierBits) — vkCode is java.awt.event.KeyEvent.VK_*
  def runTTY(userSources: Map[String, String], maxCycles: Int = 2000000,
             keyEvents: Seq[(Int, Boolean, Int)] = Seq.empty): (CPU, String) =
    val bootTof = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel" -> kernelSysl, "oskit/services" -> servicesSysl, "oskit/timer" -> timerSysl,
      "oskit/semaphore" -> semaphoreSysl, "oskit/mutex" -> mutexSysl,
      "oskit/ipc" -> ipcSysl, "oskit/kbd" -> kbdSysl, "oskit/tty" -> ttySysl,
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
      val base: Long = 0x100000
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val intc = new InterruptController(Runtime.intcAddress)
    val timer = new Timer(Runtime.timerAddress, intc, irq = 0)
    val kbd = new KeyboardDevice(Runtime.keyboardAddress, intc, irq = 1)
    val mem = new Memory("Memory", new RAM(0, 0x100000), stdout, intc, timer, kbd)
    linked.load(mem)

    // Pre-enqueue keyboard events
    for (scancode, press, mods) <- keyEvents do
      kbd.enqueue(scancode, press,
        shiftDown = (mods & 1) != 0,
        ctrlDown = (mods & 2) != 0,
        altDown = (mods & 4) != 0,
        metaDown = (mods & 8) != 0)

    val cpu = new CPU(mem, Seq(timer, intc)) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  "TTY: write single character via IPC" in {
    val (_, output) = runTTY(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(tty_server, 0x10000, 0xF000, "tty")
          |    create_thread(client, 0x14000, 0x13000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |client()
          |    sleep(50)
          |    tty_putc(72)
          |    tty_putc(105)
          |""".stripMargin
    ))

    // Should output "Hi" via TTY write
    output should include("Hi")
  }

  "TTY: write string via IPC" in {
    val (_, output) = runTTY(Map(
      "app" ->
        """import oskit.*
          |
          |var hello: [6]i8
          |
          |kernel_main() -> int
          |    hello[0] = 72
          |    hello[1] = 101
          |    hello[2] = 108
          |    hello[3] = 108
          |    hello[4] = 111
          |    hello[5] = 0
          |    ipc_init()
          |    create_thread(tty_server, 0x10000, 0xF000, "tty")
          |    create_thread(client, 0x14000, 0x13000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |client()
          |    sleep(50)
          |    tty_puts(&hello[0], 5)
          |""".stripMargin
    ))

    output should include("Hello")
  }

  "TTY: multiple clients write to same tty" in {
    val (_, output) = runTTY(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(tty_server, 0x10000, 0xF000, "tty")
          |    create_thread(clientA, 0x14000, 0x13000, "cA")
          |    create_thread(clientB, 0x18000, 0x17000, "cB")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |clientA()
          |    sleep(10)
          |    tty_putc(65)
          |    tty_putc(66)
          |
          |clientB()
          |    sleep(15)
          |    tty_putc(67)
          |    tty_putc(68)
          |""".stripMargin
    ))

    // Both clients write through the TTY
    output should include("A")
    output should include("B")
    output should include("C")
    output should include("D")
  }

  "TTY: client discovers tty port by name" in {
    val (_, output) = runTTY(Map(
      "app" ->
        """import oskit.*
          |
          |var tname: [4]i8
          |
          |kernel_main() -> int
          |    tname[0] = 116
          |    tname[1] = 116
          |    tname[2] = 121
          |    tname[3] = 0
          |    ipc_init()
          |    create_thread(tty_server, 0x10000, 0xF000, "tty")
          |    create_thread(client, 0x14000, 0x13000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |client()
          |    sleep(50)
          |    val port = port_lookup(&tname[0])
          |    if port >= 0
          |        putc('F')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("F")
  }

  "TTY: read single character from keyboard" in {
    val (_, output) = runTTY(
      Map(
        "app" ->
          """import oskit.*
            |
            |kernel_main() -> int
            |    ipc_init()
            |    create_thread(tty_server, 0x10000, 0xF000, "tty")
            |    create_thread(client, 0x14000, 0x13000, "cli")
            |    timer_init(1000)
            |    first_thread_ssp()
            |
            |client()
            |    sleep(50)
            |    val ch = tty_getc()
            |    putc(ch)
            |""".stripMargin
      ),
      keyEvents = Seq(
        (KeyEvent.VK_H, true, 0),  // press 'h'
      )
    )

    info(s"output: '$output'")
    output should include("h")
  }

  "TTY: read shifted character (uppercase)" in {
    val (_, output) = runTTY(
      Map(
        "app" ->
          """import oskit.*
            |
            |kernel_main() -> int
            |    ipc_init()
            |    create_thread(tty_server, 0x10000, 0xF000, "tty")
            |    create_thread(client, 0x14000, 0x13000, "cli")
            |    timer_init(1000)
            |    first_thread_ssp()
            |
            |client()
            |    sleep(50)
            |    val ch = tty_getc()
            |    putc(ch)
            |""".stripMargin
      ),
      keyEvents = Seq(
        (KeyEvent.VK_H, true, 1),  // press 'H' with shift
      )
    )

    output should include("H")
  }

  "TTY: read multiple characters" in {
    val (_, output) = runTTY(
      Map(
        "app" ->
          """import oskit.*
            |
            |kernel_main() -> int
            |    ipc_init()
            |    create_thread(tty_server, 0x10000, 0xF000, "tty")
            |    create_thread(client, 0x14000, 0x13000, "cli")
            |    timer_init(1000)
            |    first_thread_ssp()
            |
            |client()
            |    sleep(50)
            |    var i = 0
            |    while i < 3
            |        val ch = tty_getc()
            |        putc(ch)
            |        i += 1
            |""".stripMargin
      ),
      keyEvents = Seq(
        (KeyEvent.VK_A, true, 0),
        (KeyEvent.VK_B, true, 0),
        (KeyEvent.VK_C, true, 0),
      )
    )

    output should include("abc")
  }
}
