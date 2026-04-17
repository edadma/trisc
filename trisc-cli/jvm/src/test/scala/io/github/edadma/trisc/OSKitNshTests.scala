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
  private lazy val fsClientSysl: String = readLsysl("oskit/fs/client.lsysl")
  private lazy val tfsSrvSysl: String = readLsysl("oskit/servers/tfs.lsysl")
  private lazy val initSysl: String   = readLsysl("oskit/apps/init.lsysl")
  private lazy val debugSysl: String    = readLsysl("std/debug/debug.lsysl")
  private lazy val memSysl: String     = readLsysl("std/mem/mem.lsysl")
  private lazy val binarySysl: String  = readLsysl("std/encoding/binary/binary.lsysl")
  private lazy val stringsSysl: String = readLsysl("std/strings/strings.lsysl")
  private lazy val builderSysl: String = readLsysl("std/builder/builder.lsysl")
  private lazy val strconvSysl: String = readLsysl("std/strconv/strconv.lsysl")
  private lazy val resultSysl: String  = readLsysl("std/result/result.lsysl")
  private lazy val errorsSysl: String  = readLsysl("std/errors/errors.lsysl")
  private lazy val utf8Sysl: String    = readLsysl("std/utf8/utf8.lsysl")
  private lazy val loaderSysl: String  = readLsysl("oskit/loader/loader.lsysl")
  private lazy val stdAllocSysl: String = readLsysl("std/alloc/alloc.lsysl")
  private lazy val sha256Sysl: String  = readLsysl("std/crypto/sha256/sha256.lsysl")
  private lazy val hmacSysl: String   = readLsysl("std/crypto/hmac/hmac.lsysl")
  private lazy val pbkdf2Sysl: String = readLsysl("std/crypto/pbkdf2/pbkdf2.lsysl")
  private lazy val pmSrvSysl: String = readLsysl("oskit/servers/pm.lsysl")
  private lazy val vfsSrvSysl: String = readLsysl("oskit/servers/vfs.lsysl")
  private lazy val rsSrvSysl: String  = readLsysl("oskit/servers/rs.lsysl")
  private lazy val halMemSysl: String = readLsysl("oskit/hal/mem_dma.lsysl")
  private lazy val archVmSysl: String = readLsysl("oskit/arch/trisc/vm.lsysl")
  private lazy val archCpuSysl: String = readLsysl("oskit/arch/trisc/cpu.lsysl")
  private lazy val archProgConfigSysl: String = scala.io.Source.fromFile("oskit/arch/trisc/prog_config.sysl").mkString
  private lazy val configSysl: String = scala.io.Source.fromFile("oskit/config/config.sysl").mkString

  // Shared OS source set — init reads /etc/ttytab to decide what to spawn.
  private def buildOS(): TOF =
    val bootTof    = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel/kernel"         -> kernelSysl,
      "oskit/services/services"     -> servicesSysl,
      "oskit/kernel/timer"          -> timerSysl,
      "oskit/sync/semaphore"        -> semaphoreSysl,
      "oskit/sync/mutex"            -> mutexSysl,
      "oskit/ipc/ipc"               -> ipcSysl,
      "oskit/drivers/kbd/keyboard"  -> kbdSysl,
      "posix/unistd/sbrk"          -> sbrkSysl,
      "posix/string/string"        -> posixStringSysl,
      "posix/ctype/ctype"          -> posixCtypeSysl,
      "posix/stdlib/alloc"         -> posixAllocSysl,
      "oskit/hal/mem"              -> halMemSysl,
      "oskit/arch/vm"              -> archVmSysl,
      "oskit/arch/cpu"             -> archCpuSysl,
      "oskit/arch/prog_config"     -> archProgConfigSysl,
      "oskit/config/config"        -> configSysl,
      "app" ->
        """import oskit.kernel.*
import oskit.ipc.*
import oskit.drivers.kbd.{keyboard_init}
import oskit.config.{BOOT_INFO_ADDR}
import oskit.arch.{vm_copy_to, vm_create_server_pt}
import oskit.hal.memset
          |
          |// Read little-endian u32 from byte pointer
          |k_read_u32(p: *byte) -> i64
          |    i64(p[0] & 0xff) | (i64(p[1] & 0xff) << 8) | (i64(p[2] & 0xff) << 16) | (i64(p[3] & 0xff) << 24)
          |
          |k_read_i64(p: *byte) -> i64
          |    k_read_u32(p) | (k_read_u32(p + 4) << 32)
          |
          |// Load TRB v1 binary from memory into a target page table.
          |k_load_trb(buf: *byte, buflen: int, ptbr: int) -> i64
          |    if buflen < 12
          |        return -1
          |    if buf[0] != byte('T')
          |        return -1
          |    if buf[1] != byte('R')
          |        return -1
          |    if buf[2] != byte('B')
          |        return -1
          |    if buf[3] != byte(1)
          |        return -1
          |    val entry = k_read_u32(buf + 4)
          |    val nrec = int(k_read_u32(buf + 8))
          |    var pos = 12
          |    var ri = 0
          |    while ri < nrec
          |        if pos + 12 > buflen
          |            return -1
          |        val hp = buf + pos
          |        val org = int(k_read_u32(hp))
          |        val kind = int(k_read_u32(hp + 4))
          |        val sz = int(k_read_u32(hp + 8))
          |        pos += 12
          |        if kind == 0
          |            if pos + sz > buflen
          |                return -1
          |            vm_copy_to(ptbr, org, buf + pos, sz)
          |            pos += sz
          |        else if kind == 1
          |            var zero_buf: [1024]byte
          |            memset(&zero_buf[0], 0, 1024)
          |            var rem = sz
          |            var dst = org
          |            while rem > 0
          |                var chunk = rem
          |                if chunk > 1024
          |                    chunk = 1024
          |                vm_copy_to(ptbr, dst, &zero_buf[0], chunk)
          |                rem -= chunk
          |                dst += chunk
          |        else
          |            return -1
          |        ri += 1
          |    entry
          |
          |kernel_main() -> int
          |    ipc_init()
          |    keyboard_init()
          |
          |    // --- Bootstrap RS from boot info ---
          |    val bi = *byte(BOOT_INFO_ADDR)
          |    if bi[0] != byte('S')
          |        return -1
          |    if bi[1] != byte('L')
          |        return -1
          |    if bi[2] != byte('I')
          |        return -1
          |    if bi[3] != byte('X')
          |        return -1
          |
          |    val mod_count = int(k_read_u32(bi + 4))
          |    var rs_idx = -1
          |    var mi = 0
          |    while mi < mod_count
          |        val entry_p = bi + 8 + mi * 24
          |        if entry_p[0] == byte('r')
          |            if entry_p[1] == byte('s')
          |                if entry_p[2] == 0
          |                    rs_idx = mi
          |        mi += 1
          |    if rs_idx < 0
          |        return -1
          |
          |    val rs_entry_p = bi + 8 + rs_idx * 24
          |    val rs_addr = int(k_read_i64(rs_entry_p + 8))
          |    val rs_size = int(k_read_i64(rs_entry_p + 16))
          |
          |    val rs_ptbr = vm_create_server_pt(0)
          |    if rs_ptbr == 0
          |        return -1
          |
          |    val rs_entry_pt = k_load_trb(*byte(rs_addr), rs_size, rs_ptbr)
          |    if rs_entry_pt < 0
          |        return -1
          |
          |    var info_page: [256]byte
          |    memset(&info_page[0], 0, 256)
          |    val bi_copy_len = 8 + mod_count * 24
          |    var ci = 0
          |    while ci < bi_copy_len
          |        if ci + 24 >= 256
          |            break
          |        info_page[ci + 24] = bi[ci]
          |        ci += 1
          |    vm_copy_to(rs_ptbr, 0xBF000, &info_page[0], 256)
          |
          |    val rs_pid = create_process_suspended(rs_entry_pt, 0xD0000, 0xCF000, "rs", rs_ptbr)
          |    if rs_pid < 0
          |        return -1
          |    resume_process(rs_pid)
          |
          |    timer_init(1000)
          |    first_thread_ssp()
          |""".stripMargin,
    )
    val driver  = new SyslDriver(Some(JvmTestFileOps), List("."), tangler = Some(raw => LiterateRenderer.tangle(new LiterateParser().parse(raw))))
    val result  = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs    =
      for unit <- result.units yield
        val asm = codegen.generate(unit.typed)
        assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    Linker.link(Seq(bootTof, syslTof), linkerScript, 0)

  private lazy val osLinked: TOF = buildOS()

  private val nshTtytab = "/etc/ttytab file \"tty0 nsh\"\n"

  def runNsh(
      maxCycles: Int = 30000000,
      prefill: String = "\n",
      scheduledKeys: Seq[(Int, Int, Boolean, Int)] = Seq.empty,
  ): (CPU, String) =
    val linked = osLinked

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
      sectors = 256,
      sectorSize = 4096,
      intc,
      irq = 3,
      prefill = nshTtytab + prefill,
      maxInodes = 64,
      files = RamdiskBinPrograms.loadEmbeddedBinaries(),
    )
    val sha = new ShaAccelerator(Runtime.shaAccelAddress)
    val dma = new DMA(Runtime.dmaAddress, null, intc, 4)
    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, ramdisk, sha, dma)
    dma.mem = mem
    linked.load(mem)
    TriscCli.writeBootInfo(mem, OskitDemoBuilder.compileBootModules())

    val pending                  = scheduledKeys.sortBy(_._1).to(scala.collection.mutable.Queue)
    val keyInjector: Processor => Unit = cpu => {
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

  import java.awt.event.KeyEvent
  private def charToVK(ch: Char): (Int, Int) = ch match
    case c if c >= 'a' && c <= 'z' => (KeyEvent.VK_A + (c - 'a'), 0)
    case c if c >= 'A' && c <= 'Z' => (KeyEvent.VK_A + (c - 'A'), 1)
    case c if c >= '0' && c <= '9' => (KeyEvent.VK_0 + (c - '0'), 0)
    case '\u0003'                  => (KeyEvent.VK_C, 2) // Ctrl-C (mods: ctrl=2)
    case '\n'                      => (KeyEvent.VK_ENTER, 0)
    case ' '                       => (KeyEvent.VK_SPACE, 0)
    case '/'                       => (KeyEvent.VK_SLASH, 0)
    case '-'                       => (KeyEvent.VK_MINUS, 0)
    case '.'                       => (KeyEvent.VK_PERIOD, 0)
    case '!'                       => (KeyEvent.VK_1, 1)
    case _                         => (KeyEvent.VK_SPACE, 0)

  def typeString(s: String, startTick: Int, spacing: Int = 2000): Seq[(Int, Int, Boolean, Int)] =
    s.flatMap { ch =>
      val (scancode, mods) = charToVK(ch)
      Seq((0, scancode, true, mods), (0, scancode, false, mods))
    }.zipWithIndex.map { case ((_, sc, press, mods), i) =>
      (startTick + i * spacing, sc, press, mods)
    }

  // === NSH integration tests ===

  "NSH: pwd shows root" taggedAs Slow in {
    val keys        = typeString("pwd\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 200000000)
    output should include("/")
  }

  "NSH: echo command" taggedAs Slow in {
    val keys        = typeString("echo hi\n", startTick = 2000000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 100000000)
    output should include("\nhi\n")
  }

  "NSH: ls on root with prefilled file" taggedAs Slow in {
    val keys        = typeString("ls\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, prefill = "/hello file \"world\"\n", maxCycles = 100000000)
    output should include("hello")
  }

  "NSH: hello prints greeting" taggedAs Slow in {
    val keys        = typeString("hello\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 200000000)
    output should include("Hello, world!")
  }


  "NSH: cat prefilled /hello (short file)" taggedAs Slow in {
    val keys = typeString("cat /hello\n", startTick = 2000000, spacing = 12000)
    val (cpu, output) =
      runNsh(scheduledKeys = keys, prefill = "/hello file \"world\"\n", maxCycles = 200000000)
    output should include("world")
  }

  "NSH: cat reads file" taggedAs Slow in {
    val keys =
      typeString("touch /hello\nwrite /hello world\ncat /hello\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 200000000)
    output should include("world")
  }

  "NSH: cat /etc/ttytab shows prefilled line" taggedAs Slow in {
    val keys = typeString("cat /etc/ttytab\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 100000000)
    output should include("tty0 nsh")
  }

  "NSH: help command" taggedAs Slow in {
    val keys        = typeString("help\n", startTick = 2000000)
    val (_, output) = runNsh(scheduledKeys = keys)
    output should include("builtins: pwd cd kill help")
  }

  "NSH: whoami returns 0" taggedAs Slow in {
    val keys        = typeString("whoami\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 100000000)
    output should include("0")
  }

  "NSH: unknown command" taggedAs Slow in {
    val keys        = typeString("foo\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 100000000)
    output should include("not found")
  }

  "NSH: cd dev then pwd" taggedAs Slow in {
    val keys        = typeString("cd dev\npwd\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, prefill = "/dev dir\n", maxCycles = 100000000)
    output should include("/dev")
  }

  "NSH: touch creates file" taggedAs Slow in {
    val keys        = typeString("touch /hello\nls\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 200000000)
    output should include("hello")
  }

  "NSH: write and cat" taggedAs Slow in {
    val keys        = typeString("touch /msg\nwrite /msg hi\ncat /msg\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 300000000)
    output should include("hi")
  }

  "NSH: mv renames file" taggedAs Slow in {
    val keys =
      typeString("touch /old\nwrite /old data\nmv /old /new\ncat /new\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 400000000)
    output should include("data")
  }

  "NSH: uptime shows ticks" taggedAs Slow in {
    val keys        = typeString("uptime\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 100000000)
    output should include("up ")
    output should include("ticks")
  }

  // === Login integration tests ===

  private val passwdPrefill =
    "/etc/ttytab file \"tty0 login\"\n" +
    "/root dir\n" +
    "/home dir\n" +
    "/home/ed dir\n" +
    "/etc/passwd file \"root:x:0:0:root:/root:/nsh\\ned:x:1000:1000:ed:/home/ed:/nsh\"\n" +
    "/etc/shadow file \"root:slix:3b1b8291c0bdb62febcd914f45884bca403ae1c42a4bb1c41755881f3886d158\\ned:slix:c638d5b6e91f70b96934aac8d7be42363ce4ea5927f9a9bbbe2d64a8b51926b5\"\n"

  def runLogin(
      maxCycles: Int = 50000000,
      prefill: String = passwdPrefill,
      scheduledKeys: Seq[(Int, Int, Boolean, Int)] = Seq.empty,
  ): (CPU, String, RAM) =
    val linked = osLinked

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
      sectors = 256,
      sectorSize = 4096,
      intc,
      irq = 3,
      prefill = prefill,
      maxInodes = 64,
      files = RamdiskBinPrograms.loadEmbeddedBinaries(),
    )
    val sha = new ShaAccelerator(Runtime.shaAccelAddress)
    val dma = new DMA(Runtime.dmaAddress, null, intc, 4)
    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, ramdisk, sha, dma)
    dma.mem = mem
    linked.load(mem)
    TriscCli.writeBootInfo(mem, OskitDemoBuilder.compileBootModules())

    val pending                  = scheduledKeys.sortBy(_._1).to(scala.collection.mutable.Queue)
    val keyInjector: Processor => Unit = cpu => {
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
    (cpu, output.toString, ram)

  private def loginAndType(cmd: String, startTick: Int = 2000000): Seq[(Int, Int, Boolean, Int)] =
    // Login is now an external program — needs much more time to load and start.
    val sp = 12000
    typeString("root\n", startTick = startTick, spacing = sp) ++
    typeString("toor\n", startTick = startTick + 500000, spacing = sp) ++
    typeString(cmd, startTick = startTick + 2000000, spacing = sp)

  "Login: prompts for credentials" taggedAs Slow in {
    val keys        = typeString("root\n", startTick = 2000000)
    val (_, output, _) = runLogin(scheduledKeys = keys)
    output should include("login: ")
  }

  "Login: successful login shows shell prompt in home dir" taggedAs Slow in {
    val keys        = loginAndType("")
    val (_, output, _) = runLogin(scheduledKeys = keys, maxCycles = 800000000)
    output should include("login: ")
    output should include("password: ")
    output should include("/root> ")
  }

  "Login: whoami returns 0 for root" taggedAs Slow in {
    val keys        = loginAndType("whoami\n")
    val (_, output, _) = runLogin(scheduledKeys = keys)
    output should include("0")
  }

  "Login: pwd shows home directory" taggedAs Slow in {
    val keys        = loginAndType("pwd\n")
    val (_, output, _) = runLogin(scheduledKeys = keys)
    output should include("/root")
  }

  "Login: cat /etc/ttytab prints ttytab contents" taggedAs Slow in {
    val keys        = loginAndType("cat /etc/ttytab\n")
    val (_, output, _) = runLogin(scheduledKeys = keys, maxCycles = 180000000)
    output should include("tty0 login")
  }

  "Login: hello after login" taggedAs Slow in {
    val keys = loginAndType("hello\n")
    val (_, output, _) = runLogin(scheduledKeys = keys, maxCycles = 800000000)
    output should include("Hello")
  }

  "Login: hello three times" taggedAs Slow in {
    val sp = 12000
    val keys = typeString("root\n", startTick = 2000000, spacing = sp) ++
      typeString("toor\n", startTick = 2500000, spacing = sp) ++
      typeString("hello\n", startTick = 4000000, spacing = sp) ++
      typeString("hello\n", startTick = 30000000, spacing = sp) ++
      typeString("hello\n", startTick = 60000000, spacing = sp)
    val (cpu, output, _) = runLogin(scheduledKeys = keys, maxCycles = 400000000)
    val helloCount = "Hello, world!".r.findAllIn(output).length
    helloCount shouldBe 3
  }

  "Login: echo after login" taggedAs Slow in {
    val keys = loginAndType("echo hi\n")
    val (_, output, _) = runLogin(scheduledKeys = keys, maxCycles = 180000000)
    output should include("hi")
  }

  "Login: user ed gets home /home/ed" taggedAs Slow in {
    val keys = typeString("ed\n", startTick = 2000000, spacing = 12000) ++
               typeString("ed\n", startTick = 2500000, spacing = 12000)
    val (_, output, _) = runLogin(scheduledKeys = keys)
    output should include("/home/ed> ")
  }

  "Login: bad password rejected" taggedAs Slow in {
    val keys = typeString("root\n", startTick = 2000000, spacing = 12000) ++
               typeString("wrong\n", startTick = 2500000, spacing = 12000)
    val (_, output, _) = runLogin(scheduledKeys = keys)
    output should include("Login incorrect")
  }

  "Login: bad username rejected" taggedAs Slow in {
    val keys = typeString("nobody\n", startTick = 2000000, spacing = 12000) ++
               typeString("x\n", startTick = 2500000, spacing = 12000)
    val (_, output, _) = runLogin(scheduledKeys = keys)
    output should include("Login incorrect")
  }

  // --- wc tests ---

  "NSH: wc counts bytes in prefilled file" taggedAs Slow in {
    val keys = typeString("wc /hello\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, prefill = "/hello file \"abc\"\n", maxCycles = 200000000)
    output should include("3")  // 3 bytes
  }

  "NSH: wc counts lines" taggedAs Slow in {
    val keys = typeString("wc /data\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, prefill = "/data file \"one\\ntwo\\n\"\n", maxCycles = 200000000)
    output should include("2")  // 2 lines
  }

  // --- grep tests ---

  "NSH: grep finds matching line" taggedAs Slow in {
    val keys = typeString("grep root /etc/passwd\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, maxCycles = 200000000)
    output should include("root")
  }

  "NSH: grep filters non-matching lines" taggedAs Slow in {
    val keys = typeString("grep xyz /hello\n", startTick = 2000000, spacing = 12000)
    val (_, output) = runNsh(scheduledKeys = keys, prefill = "/hello file \"abc\"\n", maxCycles = 200000000)
    output should not include("abc")
  }
}
