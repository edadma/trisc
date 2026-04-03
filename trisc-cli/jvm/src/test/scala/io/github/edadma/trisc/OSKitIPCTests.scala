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
}
