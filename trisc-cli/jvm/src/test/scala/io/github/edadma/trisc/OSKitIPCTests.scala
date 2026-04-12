package io.github.edadma.trisc

class OSKitIPCTests extends OSKitTestHelpers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val ipcSysl: String = readLsysl("oskit/ipc/ipc.lsysl")
  private lazy val memSysl: String = readLsysl("std/mem/mem.lsysl")
  private lazy val halMemSysl: String = readLsysl("oskit/hal/mem_dma.lsysl")
  private lazy val configSysl: String = scala.io.Source.fromFile("oskit/config/config.sysl").mkString

  def runIPC(userSources: Map[String, String], maxCycles: Int = 2000000): (CPU, String) =
    val bootTof = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel/kernel" -> kernelSysl, "oskit/services/services" -> servicesSysl, "oskit/kernel/timer" -> timerSysl,
      "oskit/sync/semaphore" -> semaphoreSysl, "oskit/sync/mutex" -> mutexSysl,
      "oskit/ipc/ipc" -> ipcSysl, "std/mem/mem" -> memSysl, "oskit/hal/mem" -> halMemSysl, "oskit/config/config" -> configSysl,
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
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val intc = new InterruptController(Runtime.intcAddress)
    val timer = new Timer(Runtime.timerAddress, intc, irq = 0)
    val dma = new DMA(Runtime.dmaAddress, null, intc, 4)
    val mem = new Memory("Memory", new RAM(0, Runtime.stdoutAddress.toInt), stdout, intc, timer, dma)
    dma.mem = mem
    linked.load(mem)
    val testMmu = new SimpleMMU(mem); testMmu.setIdentityRange(0x7FE000L, 0xC00000L)
    val cpu = new CPU(mem, Seq(timer, intc), mmu = Some(testMmu)) { this.limit = maxCycles; quiet = true }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  "IPC: port_create returns port ID" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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

  "IPC: basic send and recv" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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
          |    var buf: [64]byte
          |    val sender = ipc_recv(port, &buf[0], 64)
          |    // Check first byte of message
          |    if buf[0] == 72
          |        putc('Y')
          |    else
          |        putc('N')
          |    // Reply with 'O' 'K'
          |    var reply: [2]byte
          |    reply[0] = 79
          |    reply[1] = 75
          |    ipc_reply(sender, &reply[0], 2)
          |
          |client()
          |    sleep(5)
          |    // Send "Hi" (72, 105)
          |    var msg: [2]byte
          |    msg[0] = 72
          |    msg[1] = 105
          |    var reply: [64]byte
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

  "IPC: multi-client FIFO ordering" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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
          |    var buf: [64]byte
          |    // First recv — should get client A (sent first)
          |    val s1 = ipc_recv(port, &buf[0], 64)
          |    putc(buf[0])
          |    var r1: [1]byte
          |    r1[0] = 1
          |    ipc_reply(s1, &r1[0], 1)
          |    // Second recv — should get client B
          |    val s2 = ipc_recv(port, &buf[0], 64)
          |    putc(buf[0])
          |    var r2: [1]byte
          |    r2[0] = 2
          |    ipc_reply(s2, &r2[0], 1)
          |
          |clientA()
          |    sleep(5)
          |    var msg: [1]byte
          |    msg[0] = 65
          |    var reply: [1]byte
          |    ipc_send(0, &msg[0], 1, &reply[0], 1)
          |    putc('a')
          |
          |clientB()
          |    sleep(10)
          |    var msg: [1]byte
          |    msg[0] = 66
          |    var reply: [1]byte
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

  "IPC: send to invalid port returns error" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    var msg: [1]byte
          |    msg[0] = 1
          |    var reply: [1]byte
          |    val r = ipc_send(99, &msg[0], 1, &reply[0], 1)
          |    if r == -1
          |        putc('E')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("E")
  }

  "IPC: port_close wakes blocked senders" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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
          |    var msg: [1]byte
          |    msg[0] = 1
          |    var reply: [1]byte
          |    val r = ipc_send(0, &msg[0], 1, &reply[0], 1)
          |    if r == -1
          |        putc('W')
          |""".stripMargin
    ))

    // C = port closed successfully, W = client woke with error
    output should include("C")
    output should include("W")
  }

  "IPC: recv on unowned port returns error" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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
          |    var buf: [64]byte
          |    val r = ipc_recv(0, &buf[0], 64)
          |    if r == -1
          |        putc('E')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("E")
  }

  "IPC: client blocks until server recvs" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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
          |    var buf: [64]byte
          |    val sender = ipc_recv(port, &buf[0], 64)
          |    putc('D')
          |    var reply: [1]byte
          |    reply[0] = 1
          |    ipc_reply(sender, &reply[0], 1)
          |
          |client()
          |    sleep(5)
          |    putc('S')
          |    var msg: [1]byte
          |    msg[0] = 42
          |    var reply: [1]byte
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

  "IPC: send to closed port returns error" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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
          |    var msg: [1]byte
          |    msg[0] = 1
          |    var reply: [1]byte
          |    val r = ipc_send(0, &msg[0], 1, &reply[0], 1)
          |    if r == -1
          |        putc('E')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("E")
  }

  "IPC: reply to non-blocked thread returns error" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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
          |    var msg: [1]byte
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

  "IPC: port_create at capacity returns -1" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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

  "IPC: message truncation when buf too small" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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
          |    var buf: [2]byte
          |    buf[0] = 0
          |    buf[1] = 0
          |    val sender = ipc_recv(port, &buf[0], 2)
          |    // Should get first 2 bytes: 'A', 'B'
          |    if buf[0] == 65
          |        if buf[1] == 66
          |            putc('T')
          |    var reply: [1]byte
          |    reply[0] = 1
          |    ipc_reply(sender, &reply[0], 1)
          |
          |client()
          |    sleep(5)
          |    // Send 4 bytes: A B C D
          |    var msg: [4]byte
          |    msg[0] = 65
          |    msg[1] = 66
          |    msg[2] = 67
          |    msg[3] = 68
          |    var reply: [1]byte
          |    ipc_send(0, &msg[0], 4, &reply[0], 1)
          |    putc('!')
          |""".stripMargin
    ))

    // T = server got truncated message correctly, ! = client got reply
    output should include("T")
    output should include("!")
  }

  "IPC: reply truncation when reply_buf too small" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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
          |    var buf: [64]byte
          |    val sender = ipc_recv(port, &buf[0], 64)
          |    // Reply with 4 bytes
          |    var reply: [4]byte
          |    reply[0] = 65
          |    reply[1] = 66
          |    reply[2] = 67
          |    reply[3] = 68
          |    ipc_reply(sender, &reply[0], 4)
          |
          |client()
          |    sleep(5)
          |    var msg: [1]byte
          |    msg[0] = 1
          |    // Reply buffer only 2 bytes — should truncate
          |    var reply: [2]byte
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

  "IPC: full lifecycle — multi-client, handle all, close" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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
          |    var buf: [64]byte
          |    var i = 0
          |    while i < 2
          |        val sender = ipc_recv(port, &buf[0], 64)
          |        putc(buf[0])
          |        var reply: [1]byte
          |        reply[0] = buf[0] + 32
          |        ipc_reply(sender, &reply[0], 1)
          |        i += 1
          |    port_close(port)
          |    putc('X')
          |
          |clientA()
          |    sleep(5)
          |    var msg: [1]byte
          |    msg[0] = 65
          |    var reply: [1]byte
          |    ipc_send(0, &msg[0], 1, &reply[0], 1)
          |    // Reply should be 'a' (65 + 32 = 97)
          |    if reply[0] == 97
          |        putc('a')
          |
          |clientB()
          |    sleep(10)
          |    var msg: [1]byte
          |    msg[0] = 66
          |    var reply: [1]byte
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

  "IPC: port register and lookup by name" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
          |
          |var tty_name: [5]byte
          |
          |kernel_main() -> int
          |    tty_name[0] = 116
          |    tty_name[1] = 116
          |    tty_name[2] = 121
          |    tty_name[3] = 48
          |    tty_name[4] = 0
          |    ipc_init()
          |    create_thread(server, 0x10000, 0xF000, "srv")
          |    create_thread(client, 0x14000, 0x13000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |server()
          |    val port = port_create()
          |    val r = port_register(port, &tty_name[0])
          |    if r == 0
          |        putc('R')
          |    var buf: [64]byte
          |    val sender = ipc_recv(port, &buf[0], 64)
          |    putc(buf[0])
          |    var reply: [1]byte
          |    reply[0] = 1
          |    ipc_reply(sender, &reply[0], 1)
          |
          |client()
          |    sleep(10)
          |    // Look up "tty0" by name
          |    val port = port_lookup(&tty_name[0])
          |    if port >= 0
          |        putc('L')
          |    var msg: [1]byte
          |    msg[0] = 72
          |    var reply: [1]byte
          |    ipc_send(port, &msg[0], 1, &reply[0], 1)
          |    putc('!')
          |""".stripMargin
    ))

    // R = registered, L = looked up, H = server got message, ! = client done
    output should include("R")
    output should include("L")
    output should include("H")
    output should include("!")
  }

  "IPC: lookup nonexistent name returns -1" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
          |
          |var bad_name: [4]byte
          |
          |kernel_main() -> int
          |    bad_name[0] = 120
          |    bad_name[1] = 121
          |    bad_name[2] = 122
          |    bad_name[3] = 0
          |    ipc_init()
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    val r = port_lookup(&bad_name[0])
          |    if r == -1
          |        putc('N')
          |    else
          |        putc('F')
          |""".stripMargin
    ))

    output should include("N")
  }

  "IPC: server loop handles multiple requests" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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
          |    // Server loop: handle requests until told to stop
          |    var running = 1
          |    while running == 1
          |        var buf: [64]byte
          |        val sender = ipc_recv(port, &buf[0], 64)
          |        if buf[0] == 0
          |            // Stop command
          |            running = 0
          |            var reply: [1]byte
          |            reply[0] = 0
          |            ipc_reply(sender, &reply[0], 1)
          |        else
          |            // Echo command: reply with same byte + 32
          |            putc(buf[0])
          |            var reply: [1]byte
          |            reply[0] = buf[0] + 32
          |            ipc_reply(sender, &reply[0], 1)
          |    putc('Q')
          |
          |client()
          |    sleep(5)
          |    // Send 3 requests then stop
          |    var msg: [1]byte
          |    var reply: [1]byte
          |
          |    msg[0] = 65
          |    ipc_send(0, &msg[0], 1, &reply[0], 1)
          |    if reply[0] == 97
          |        putc('1')
          |
          |    msg[0] = 66
          |    ipc_send(0, &msg[0], 1, &reply[0], 1)
          |    if reply[0] == 98
          |        putc('2')
          |
          |    msg[0] = 67
          |    ipc_send(0, &msg[0], 1, &reply[0], 1)
          |    if reply[0] == 99
          |        putc('3')
          |
          |    // Send stop
          |    msg[0] = 0
          |    ipc_send(0, &msg[0], 1, &reply[0], 1)
          |""".stripMargin
    ))

    // Server echoes A, B, C then quits
    // Client confirms replies: 1, 2, 3
    output should include("A")
    output should include("B")
    output should include("C")
    output should include("1")
    output should include("2")
    output should include("3")
    output should include("Q")
  }

  "IPC: register on unowned port returns -1" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
          |
          |var name: [4]byte
          |
          |kernel_main() -> int
          |    name[0] = 97
          |    name[1] = 98
          |    name[2] = 99
          |    name[3] = 0
          |    ipc_init()
          |    create_thread(owner, 0x10000, 0xF000, "own")
          |    create_thread(thief, 0x14000, 0x13000, "thf")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |owner()
          |    port_create()
          |    sleep(100)
          |
          |thief()
          |    sleep(5)
          |    // Try to register name on port 0 (owned by other thread)
          |    val r = port_register(0, &name[0])
          |    if r == -1
          |        putc('E')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("E")
  }

  "IPC: lookup after port closed returns -1" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
          |
          |var name: [4]byte
          |
          |kernel_main() -> int
          |    name[0] = 97
          |    name[1] = 98
          |    name[2] = 99
          |    name[3] = 0
          |    ipc_init()
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    val port = port_create()
          |    port_register(port, &name[0])
          |    // Verify it's found
          |    val r1 = port_lookup(&name[0])
          |    if r1 == 0
          |        putc('F')
          |    // Close and verify gone
          |    port_close(port)
          |    val r2 = port_lookup(&name[0])
          |    if r2 == -1
          |        putc('G')
          |""".stripMargin
    ))

    // F = found before close, G = gone after close
    output should include("F")
    output should include("G")
  }

  "IPC: multiple named ports lookup correctly" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
          |
          |var n_tty: [5]byte
          |var n_dsk: [5]byte
          |
          |kernel_main() -> int
          |    n_tty[0] = 116
          |    n_tty[1] = 116
          |    n_tty[2] = 121
          |    n_tty[3] = 48
          |    n_tty[4] = 0
          |    n_dsk[0] = 100
          |    n_dsk[1] = 115
          |    n_dsk[2] = 107
          |    n_dsk[3] = 48
          |    n_dsk[4] = 0
          |    ipc_init()
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    val p0 = port_create()
          |    val p1 = port_create()
          |    port_register(p0, &n_tty[0])
          |    port_register(p1, &n_dsk[0])
          |    // Look up each
          |    val r0 = port_lookup(&n_tty[0])
          |    val r1 = port_lookup(&n_dsk[0])
          |    if r0 == 0
          |        putc('T')
          |    if r1 == 1
          |        putc('D')
          |""".stripMargin
    ))

    // T = tty0 found at port 0, D = dsk0 found at port 1
    output should include("T")
    output should include("D")
  }

  "IPC: send_timeout returns -2 when server never recvs" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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
          |    // Server creates port but never calls recv
          |    sleep(200)
          |
          |client()
          |    sleep(5)
          |    var msg: [1]byte
          |    msg[0] = 1
          |    var reply: [1]byte
          |    val r = ipc_send_timeout(0, &msg[0], 1, &reply[0], 1, 30)
          |    if r == -2
          |        putc('T')
          |    else
          |        putc('N')
          |""".stripMargin
    ))

    output should include("T")
  }

  "IPC: send_timeout succeeds when server replies in time" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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
          |    var buf: [64]byte
          |    val sender = ipc_recv(port, &buf[0], 64)
          |    putc(buf[0])
          |    var reply: [1]byte
          |    reply[0] = 1
          |    ipc_reply(sender, &reply[0], 1)
          |
          |client()
          |    sleep(5)
          |    var msg: [1]byte
          |    msg[0] = 72
          |    var reply: [1]byte
          |    val r = ipc_send_timeout(0, &msg[0], 1, &reply[0], 1, 100)
          |    if r == 0
          |        putc('S')
          |    else
          |        putc('F')
          |""".stripMargin
    ))

    // H = server got message, S = client send succeeded
    output should include("H")
    output should include("S")
  }

  "IPC: send_timeout with server delayed but replies before deadline" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
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
          |    var buf: [64]byte
          |    val sender = ipc_recv(port, &buf[0], 64)
          |    var reply: [1]byte
          |    reply[0] = 1
          |    ipc_reply(sender, &reply[0], 1)
          |    putc('D')
          |
          |client()
          |    sleep(5)
          |    var msg: [1]byte
          |    msg[0] = 1
          |    var reply: [1]byte
          |    val r = ipc_send_timeout(0, &msg[0], 1, &reply[0], 1, 100)
          |    if r == 0
          |        putc('S')
          |    else
          |        putc('F')
          |""".stripMargin
    ))

    // D = server done, S = client succeeded
    output should include("D")
    output should include("S")
  }

  "IPC: re-register changes port name" taggedAs Slow in {
    val (_, output) = runIPC(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
          |
          |var name1: [4]byte
          |var name2: [4]byte
          |
          |kernel_main() -> int
          |    name1[0] = 97
          |    name1[1] = 98
          |    name1[2] = 99
          |    name1[3] = 0
          |    name2[0] = 120
          |    name2[1] = 121
          |    name2[2] = 122
          |    name2[3] = 0
          |    ipc_init()
          |    create_thread(task, 0x10000, 0xF000, "t")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |task()
          |    val port = port_create()
          |    port_register(port, &name1[0])
          |    // Verify "abc" found
          |    val r1 = port_lookup(&name1[0])
          |    if r1 == 0
          |        putc('A')
          |    // Re-register as "xyz"
          |    port_register(port, &name2[0])
          |    // Old name gone
          |    val r2 = port_lookup(&name1[0])
          |    if r2 == -1
          |        putc('B')
          |    // New name works
          |    val r3 = port_lookup(&name2[0])
          |    if r3 == 0
          |        putc('C')
          |""".stripMargin
    ))

    // A = found under old name, B = old name gone, C = found under new name
    output should include("A")
    output should include("B")
    output should include("C")
  }
}
