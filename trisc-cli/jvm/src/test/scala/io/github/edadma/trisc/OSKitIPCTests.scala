package io.github.edadma.trisc

class OSKitIPCTests extends OSKitTestHelpers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val ipcSysl: String = readLsysl("oskit/ipc/ipc.lsysl")

  def runIPC(userSources: Map[String, String], maxCycles: Int = 2000000): (CPU, String) =
    val bootTof = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel" -> kernelSysl, "oskit/services" -> servicesSysl, "oskit/timer" -> timerSysl,
      "oskit/semaphore" -> semaphoreSysl, "oskit/mutex" -> mutexSysl,
      "oskit/ipc" -> ipcSysl,
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
    val mem = new Memory("Memory", new RAM(0, 0x100000), stdout, intc, timer)
    linked.load(mem)
    val cpu = new CPU(mem, Seq(timer, intc)) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  "IPC: port_create returns port ID" in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    val p = port_create()
          |    if p == 0
          |        putc('Y')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("Y")
  }

  "IPC: basic send and recv" in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(server, 0x10000, 0xF000, "srv")
          |    create_thread(client, 0x14000, 0x13000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |server()
          |    val port = port_create()
          |    var buf: [64]i8
          |    val sender = ipc_recv(port, &buf[0], 64)
          |    // Check first byte of message
          |    if buf[0] == 72
          |        putc('Y')
          |    else
          |        putc('N')
          |    // Reply with 'O' 'K'
          |    var reply: [2]i8
          |    reply[0] = 79
          |    reply[1] = 75
          |    ipc_reply(sender, &reply[0], 2)
          |
          |client()
          |    sleep(5)
          |    // Send "Hi" (72, 105)
          |    var msg: [2]i8
          |    msg[0] = 72
          |    msg[1] = 105
          |    var reply: [64]i8
          |    ipc_send(0, &msg[0], 2, &reply[0], 64)
          |    // Check reply
          |    if reply[0] == 79
          |        if reply[1] == 75
          |            putc('!')
          |""".stripMargin
    ))

    // Y = server got 'H', ! = client got "OK" reply
    output should include("Y")
    output should include("!")
  }

  "IPC: multi-client FIFO ordering" in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(server, 0x10000, 0xF000, "srv")
          |    create_thread(clientA, 0x14000, 0x13000, "cA")
          |    create_thread(clientB, 0x18000, 0x17000, "cB")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |server()
          |    val port = port_create()
          |    sleep(20)
          |    // Both clients should be queued by now
          |    var buf: [64]i8
          |    // First recv — should get client A (sent first)
          |    val s1 = ipc_recv(port, &buf[0], 64)
          |    putc(buf[0])
          |    var r1: [1]i8
          |    r1[0] = 1
          |    ipc_reply(s1, &r1[0], 1)
          |    // Second recv — should get client B
          |    val s2 = ipc_recv(port, &buf[0], 64)
          |    putc(buf[0])
          |    var r2: [1]i8
          |    r2[0] = 2
          |    ipc_reply(s2, &r2[0], 1)
          |
          |clientA()
          |    sleep(5)
          |    var msg: [1]i8
          |    msg[0] = 65
          |    var reply: [1]i8
          |    ipc_send(0, &msg[0], 1, &reply[0], 1)
          |    putc('a')
          |
          |clientB()
          |    sleep(10)
          |    var msg: [1]i8
          |    msg[0] = 66
          |    var reply: [1]i8
          |    ipc_send(0, &msg[0], 1, &reply[0], 1)
          |    putc('b')
          |""".stripMargin
    ))

    // Server should receive A then B (FIFO order)
    // 'A' = 65, 'B' = 66 from server putc(buf[0])
    // 'a', 'b' from clients after reply
    output should include("A")
    output should include("B")
    output.indexOf('A') should be < output.indexOf('B')
  }

  "IPC: send to invalid port returns error" in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    var msg: [1]i8
          |    msg[0] = 1
          |    var reply: [1]i8
          |    val r = ipc_send(99, &msg[0], 1, &reply[0], 1)
          |    if r == -1
          |        putc('E')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("E")
  }

  "IPC: port_close wakes blocked senders" in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(server, 0x10000, 0xF000, "srv")
          |    create_thread(client, 0x14000, 0x13000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |server()
          |    val port = port_create()
          |    sleep(20)
          |    // Client is blocked on send — close the port
          |    val r = port_close(port)
          |    if r == 0
          |        putc('C')
          |
          |client()
          |    sleep(5)
          |    var msg: [1]i8
          |    msg[0] = 1
          |    var reply: [1]i8
          |    val r = ipc_send(0, &msg[0], 1, &reply[0], 1)
          |    if r == -1
          |        putc('W')
          |""".stripMargin
    ))

    // C = port closed successfully, W = client woke with error
    output should include("C")
    output should include("W")
  }

  "IPC: recv on unowned port returns error" in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(owner, 0x10000, 0xF000, "own")
          |    create_thread(thief, 0x14000, 0x13000, "thf")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |owner()
          |    val port = port_create()
          |    sleep(100)
          |
          |thief()
          |    sleep(5)
          |    var buf: [64]i8
          |    val r = ipc_recv(0, &buf[0], 64)
          |    if r == -1
          |        putc('E')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("E")
  }

  "IPC: client blocks until server recvs" in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(server, 0x10000, 0xF000, "srv")
          |    create_thread(client, 0x14000, 0x13000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |server()
          |    val port = port_create()
          |    sleep(20)
          |    putc('R')
          |    var buf: [64]i8
          |    val sender = ipc_recv(port, &buf[0], 64)
          |    putc('D')
          |    var reply: [1]i8
          |    reply[0] = 1
          |    ipc_reply(sender, &reply[0], 1)
          |
          |client()
          |    sleep(5)
          |    putc('S')
          |    var msg: [1]i8
          |    msg[0] = 42
          |    var reply: [1]i8
          |    ipc_send(0, &msg[0], 1, &reply[0], 1)
          |    putc('E')
          |""".stripMargin
    ))

    // S = client sends (blocks), R = server starts recv (after sleep),
    // D = server got message, E = client got reply
    output should include("S")
    output should include("R")
    output should include("D")
    output should include("E")
    // S must come before R (client sends before server recvs)
    output.indexOf('S') should be < output.indexOf('R')
    // D must come before E (server processes before client resumes)
    output.indexOf('D') should be < output.indexOf('E')
  }

  "IPC: send to closed port returns error" in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(server, 0x10000, 0xF000, "srv")
          |    create_thread(client, 0x14000, 0x13000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |server()
          |    val port = port_create()
          |    port_close(port)
          |    sleep(100)
          |
          |client()
          |    sleep(10)
          |    var msg: [1]i8
          |    msg[0] = 1
          |    var reply: [1]i8
          |    val r = ipc_send(0, &msg[0], 1, &reply[0], 1)
          |    if r == -1
          |        putc('E')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("E")
  }

  "IPC: reply to non-blocked thread returns error" in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    create_thread(other, 0x14000, 0x13000, "o")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    // Try to reply to thread 1 which is not send-blocked
          |    var msg: [1]i8
          |    msg[0] = 1
          |    ipc_reply(1, &msg[0], 1)
          |    putc('D')
          |
          |other()
          |    sleep(100)
          |""".stripMargin
    ))

    // D = task continued after invalid reply (didn't crash)
    output should include("D")
  }

  "IPC: port_create at capacity returns -1" in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    // Create 8 ports (MAX_PORTS)
          |    var i = 0
          |    while i < 8
          |        port_create()
          |        i += 1
          |    // 9th should fail
          |    val r = port_create()
          |    if r == -1
          |        putc('F')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("F")
  }

  "IPC: message truncation when buf too small" in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(server, 0x10000, 0xF000, "srv")
          |    create_thread(client, 0x14000, 0x13000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |server()
          |    val port = port_create()
          |    // Recv with small buffer (2 bytes)
          |    var buf: [2]i8
          |    buf[0] = 0
          |    buf[1] = 0
          |    val sender = ipc_recv(port, &buf[0], 2)
          |    // Should get first 2 bytes: 'A', 'B'
          |    if buf[0] == 65
          |        if buf[1] == 66
          |            putc('T')
          |    var reply: [1]i8
          |    reply[0] = 1
          |    ipc_reply(sender, &reply[0], 1)
          |
          |client()
          |    sleep(5)
          |    // Send 4 bytes: A B C D
          |    var msg: [4]i8
          |    msg[0] = 65
          |    msg[1] = 66
          |    msg[2] = 67
          |    msg[3] = 68
          |    var reply: [1]i8
          |    ipc_send(0, &msg[0], 4, &reply[0], 1)
          |    putc('!')
          |""".stripMargin
    ))

    // T = server got truncated message correctly, ! = client got reply
    output should include("T")
    output should include("!")
  }

  "IPC: reply truncation when reply_buf too small" in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(server, 0x10000, 0xF000, "srv")
          |    create_thread(client, 0x14000, 0x13000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |server()
          |    val port = port_create()
          |    var buf: [64]i8
          |    val sender = ipc_recv(port, &buf[0], 64)
          |    // Reply with 4 bytes
          |    var reply: [4]i8
          |    reply[0] = 65
          |    reply[1] = 66
          |    reply[2] = 67
          |    reply[3] = 68
          |    ipc_reply(sender, &reply[0], 4)
          |
          |client()
          |    sleep(5)
          |    var msg: [1]i8
          |    msg[0] = 1
          |    // Reply buffer only 2 bytes — should truncate
          |    var reply: [2]i8
          |    reply[0] = 0
          |    reply[1] = 0
          |    ipc_send(0, &msg[0], 1, &reply[0], 2)
          |    // Should get first 2 bytes: A, B
          |    if reply[0] == 65
          |        if reply[1] == 66
          |            putc('R')
          |""".stripMargin
    ))

    output should include("R")
  }

  "IPC: full lifecycle — multi-client, handle all, close" in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(server, 0x10000, 0xF000, "srv")
          |    create_thread(clientA, 0x14000, 0x13000, "cA")
          |    create_thread(clientB, 0x18000, 0x17000, "cB")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |server()
          |    val port = port_create()
          |    // Handle two requests then close
          |    var buf: [64]i8
          |    var i = 0
          |    while i < 2
          |        val sender = ipc_recv(port, &buf[0], 64)
          |        putc(buf[0])
          |        var reply: [1]i8
          |        reply[0] = buf[0] + 32
          |        ipc_reply(sender, &reply[0], 1)
          |        i += 1
          |    port_close(port)
          |    putc('X')
          |
          |clientA()
          |    sleep(5)
          |    var msg: [1]i8
          |    msg[0] = 65
          |    var reply: [1]i8
          |    ipc_send(0, &msg[0], 1, &reply[0], 1)
          |    // Reply should be 'a' (65 + 32 = 97)
          |    if reply[0] == 97
          |        putc('a')
          |
          |clientB()
          |    sleep(10)
          |    var msg: [1]i8
          |    msg[0] = 66
          |    var reply: [1]i8
          |    ipc_send(0, &msg[0], 1, &reply[0], 1)
          |    // Reply should be 'b' (66 + 32 = 98)
          |    if reply[0] == 98
          |        putc('b')
          |""".stripMargin
    ))

    // Server echoes A, B (uppercase), clients confirm a, b (lowercase replies)
    // X = server closed port after handling both
    output should include("A")
    output should include("B")
    output should include("a")
    output should include("b")
    output should include("X")
  }
}
