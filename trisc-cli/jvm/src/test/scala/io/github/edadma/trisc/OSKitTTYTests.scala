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

  // keyEvents: (vkCode, press, modifierBits) — pre-enqueued before CPU starts
  // scheduledKeys: (cycle, vkCode, press, modifierBits) — injected at specific cycle count
  def runTTY(userSources: Map[String, String], maxCycles: Int = 8000000,
             keyEvents: Seq[(Int, Boolean, Int)] = Seq.empty,
             scheduledKeys: Seq[(Int, Int, Boolean, Int)] = Seq.empty): (CPU, String) =
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

    // Build tick callbacks: timer, intc, plus scheduled key injector
    // Uses tick count (not cpu.cycles) since cycles don't advance during WFI
    val pending = scheduledKeys.sortBy(_._1).to(scala.collection.mutable.Queue)
    var tickCount = 0L
    var injectedCount = 0
    val keyInjector: CPU => Unit = _ => {
      tickCount += 1
      while pending.nonEmpty && tickCount >= pending.head._1 do
        val (_, vk, press, mods) = pending.dequeue()
        kbd.enqueue(vk, press,
          shiftDown = (mods & 1) != 0,
          ctrlDown = (mods & 2) != 0,
          altDown = (mods & 4) != 0,
          metaDown = (mods & 8) != 0)
        injectedCount += 1
    }
    val ticks: Seq[CPU => Unit] = if scheduledKeys.nonEmpty then Seq(timer, intc, keyInjector) else Seq(timer, intc)
    val cpu = new CPU(mem, ticks) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    if false then println(s"  [debug] ticks=$tickCount injected=$injectedCount cycles=${cpu.cycles}")
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
          |    sleep(50)
          |    tty_putc(65)
          |    tty_putc(66)
          |
          |clientB()
          |    sleep(60)
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

    //info(s"output: '$output'")
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

  "TTY: ipc_recv_notify wakes on keyboard notification" in {
    val (_, output) = runTTY(
      Map(
        "app" ->
          """import oskit.*
            |
            |kernel_main() -> int
            |    ipc_init()
            |    create_thread(server, 0x10000, 0xF000, "srv")
            |    timer_init(1000)
            |    first_thread_ssp()
            |
            |server()
            |    keyboard_init()
            |    keyboard_set_notify(thread_id())
            |    val port = port_create()
            |    putc('W')
            |    var buf: [64]i8
            |    val result = ipc_recv_notify(port, &buf[0], 64)
            |    if result == -2
            |        putc('N')
            |    else
            |        putc('?')
            |    putc('!')
            |""".stripMargin
      ),
      maxCycles = 5000000,
      scheduledKeys = Seq(
        (200000, KeyEvent.VK_A, true, 0),
      )
    )

    //info(s"output: '$output'")
    output should include("W")
    output should include("N")
    output should include("!")
  }

  "TTY: recv_notify server loop with pending reader and delayed key" in {
    val (_, output) = runTTY(
      Map(
        "app" ->
          """import oskit.*
            |
            |kernel_main() -> int
            |    ipc_init()
            |    create_thread(tty_server, 0x10000, 0xF000, "tty")
            |    create_thread(reader, 0x14000, 0x13000, "rdr")
            |    timer_init(1000)
            |    first_thread_ssp()
            |
            |reader()
            |    sleep(50)
            |    putc('R')
            |    // Manual tty_getc with diagnostics
            |    var tname: [4]i8
            |    tname[0] = 116
            |    tname[1] = 116
            |    tname[2] = 121
            |    tname[3] = 0
            |    val port = port_lookup(&tname[0])
            |    if port >= 0
            |        putc('P')
            |    else
            |        putc('p')
            |    var msg: [2]i8
            |    msg[0] = 2
            |    msg[1] = 0
            |    var reply: [2]i8
            |    val r = ipc_send(port, &msg[0], 2, &reply[0], 2)
            |    putc('S')
            |    putc(reply[1])
            |    putc('!')
            |""".stripMargin
      ),
      maxCycles = 5000000,
      scheduledKeys = Seq(
        (200000, KeyEvent.VK_Z, true, 0),
      )
    )

    //info(s"output: '$output'")
    output should include("R")
    output should include("z")
    output should include("!")
  }

  "TTY: keyboard ISR notifies blocked thread" in {
    val (_, output) = runTTY(
      Map(
        "app" ->
          """import oskit.*
            |
            |var my_tid = -1
            |
            |kernel_main() -> int
            |    ipc_init()
            |    create_thread(waiter, 0x10000, 0xF000, "w")
            |    timer_init(1000)
            |    first_thread_ssp()
            |
            |waiter()
            |    keyboard_init()
            |    my_tid = thread_id()
            |    keyboard_set_notify(my_tid)
            |    putc('W')
            |    // Wait for notification from keyboard ISR
            |    notify_wait()
            |    putc('N')
            |    // Check keyboard buffer
            |    if kb_has_key() == 1
            |        putc('K')
            |    putc('!')
            |""".stripMargin
      ),
      maxCycles = 5000000,
      scheduledKeys = Seq(
        (200000, KeyEvent.VK_A, true, 0),
      )
    )

    //info(s"output: '$output'")
    output should include("W")
    output should include("N")
    output should include("K")
    output should include("!")
  }

  "TTY: blocking read — key arrives after client blocks" in {
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
            |    putc('W')
            |    val ch = tty_getc()
            |    putc(ch)
            |    putc('!')
            |""".stripMargin
      ),
      // No pre-enqueued keys — key arrives at tick 200000 (well after client blocks)
      maxCycles = 5000000,
      scheduledKeys = Seq(
        (200000, KeyEvent.VK_X, true, 0),
      )
    )

    //info(s"output: '$output'")
    // W = client about to block, x = got the key, ! = completed
    output should include("W")
    output should include("x")
    output should include("!")
  }

  "TTY: blocking read — multiple keys arrive after client blocks" in {
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
            |    putc('!')
            |""".stripMargin
      ),
      scheduledKeys = Seq(
        (200000, KeyEvent.VK_H, true, 0),
        (400000, KeyEvent.VK_I, true, 1),  // shift = uppercase I
        (600000, KeyEvent.VK_1, true, 1),  // shift+1 = !
      )
    )

    //info(s"output: '$output'")
    output should include("h")
    output should include("I")
    output should include("!")
  }
}
