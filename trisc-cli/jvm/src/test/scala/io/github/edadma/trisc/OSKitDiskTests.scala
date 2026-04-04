package io.github.edadma.trisc

class OSKitDiskTests extends OSKitTestHelpers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val ipcSysl: String = readLsysl("oskit/ipc/ipc.lsysl")
  private lazy val diskSysl: String = readLsysl("oskit/drivers/disk/disk.lsysl")
  private lazy val tfsSysl: String = readLsysl("oskit/fs/tfs.lsysl")
  private lazy val tfsSrvSysl: String = readLsysl("oskit/servers/tfs.lsysl")

  // Stack layout for 2-thread tests (disk server + client):
  //   disk:   USP=0x10000 SSP=0xE000
  //   client: USP=0x20000 SSP=0x1E000
  //
  // Stack layout for 3-thread tests (disk + tfs + client):
  //   disk:   USP=0x10000 SSP=0xE000
  //   tfs:    USP=0x20000 SSP=0x1E000
  //   client: USP=0x30000 SSP=0x2E000

  def runDisk(
      userSources: Map[String, String],
      maxCycles: Int = 5000000,
      prefill: String = "",
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
    val mem = new Memory("Memory", ram, stdout, intc, timer, ramdisk)
    linked.load(mem)
    val cpu = new CPU(mem, Seq(timer, intc)) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  def runFS(
      userSources: Map[String, String],
      maxCycles: Int = 20000000,
      prefill: String = "",
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
      "oskit/tfs" -> tfsSysl,
      "oskit/tfs_srv" -> tfsSrvSysl,
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
    val mem = new Memory("Memory", ram, stdout, intc, timer, ramdisk)
    linked.load(mem)
    val cpu = new CPU(mem, Seq(timer, intc)) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  // --- Disk server tests ---

  "Disk: server registers and client discovers port" in {
    val (_, output) = runDisk(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(client, 0x20000, 0x1E000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |client()
          |    sleep(50)
          |    var name: [5]i8
          |    name[0] = 100  // d
          |    name[1] = 105  // i
          |    name[2] = 115  // s
          |    name[3] = 107  // k
          |    name[4] = 0
          |    val port = port_lookup(&name[0])
          |    if port >= 0
          |        putc('Y')
          |    else
          |        putc('N')
          |""".stripMargin
    ))
    output should include("Y")
  }

  "Disk: read block written by hardware" in {
    val (_, output) = runDisk(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(client, 0x20000, 0x1E000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |client()
          |    sleep(50)
          |    var wbuf: [512]i8
          |    wbuf[0] = 72   // H
          |    wbuf[1] = 101  // e
          |    wbuf[2] = 108  // l
          |    wbuf[3] = 108  // l
          |    wbuf[4] = 111  // o
          |    disk_write(5, &wbuf[0])
          |
          |    var rbuf: [512]i8
          |    disk_read(5, &rbuf[0])
          |
          |    putc(rbuf[0])
          |    putc(rbuf[1])
          |    putc(rbuf[2])
          |    putc(rbuf[3])
          |    putc(rbuf[4])
          |""".stripMargin
    ))
    output should include("Hello")
  }

  "Disk: capacity returns sector count" in {
    val (_, output) = runDisk(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(client, 0x20000, 0x1E000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |client()
          |    sleep(50)
          |    val cap = disk_capacity()
          |    if cap == 64
          |        putc('Y')
          |    else
          |        putc('N')
          |""".stripMargin
    ))
    output should include("Y")
  }

  "Disk: multiple block read/write" in {
    val (_, output) = runDisk(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(client, 0x20000, 0x1E000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |write_block(lba: int, ch: i8)
          |    var buf: [512]i8
          |    buf[0] = ch
          |    disk_write(lba, &buf[0])
          |
          |read_and_print(lba: int)
          |    var buf: [512]i8
          |    disk_read(lba, &buf[0])
          |    putc(buf[0])
          |
          |client()
          |    sleep(50)
          |    write_block(10, 65)
          |    write_block(20, 66)
          |    read_and_print(10)
          |    read_and_print(20)
          |""".stripMargin
    ))
    output should include("AB")
  }

  // --- TFS server tests ---

  "FS: open root directory" in {
    val (_, output) = runFS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(tfs_server, 0x20000, 0x1E000, "tfs")
          |    create_thread(client, 0x30000, 0x2E000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |client()
          |    sleep(100)
          |    var path: [2]i8
          |    path[0] = 47  // /
          |    path[1] = 0
          |    val ino = fs_open(&path[0])
          |    if ino == 1
          |        putc('Y')
          |    else
          |        putc('N')
          |""".stripMargin
    ), prefill = "\n")
    output should include("Y")
  }

  "FS: create and open a file" in {
    val (_, output) = runFS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(tfs_server, 0x20000, 0x1E000, "tfs")
          |    create_thread(client, 0x30000, 0x2E000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |client()
          |    sleep(100)
          |    var name: [3]i8
          |    name[0] = 104  // h
          |    name[1] = 105  // i
          |    name[2] = 0
          |    val ino = fs_create(1, &name[0], 1, 0x1B6)
          |    if ino > 1
          |        putc('C')
          |    var path: [4]i8
          |    path[0] = 47   // /
          |    path[1] = 104  // h
          |    path[2] = 105  // i
          |    path[3] = 0
          |    val ino2 = fs_open(&path[0])
          |    if ino2 == ino
          |        putc('O')
          |""".stripMargin
    ), prefill = "\n")
    output should include("CO")
  }

  "FS: write and read file data" in {
    val (_, output) = runFS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(tfs_server, 0x20000, 0x1E000, "tfs")
          |    create_thread(client, 0x30000, 0x2E000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |client()
          |    sleep(100)
          |    var name: [5]i8
          |    name[0] = 116  // t
          |    name[1] = 101  // e
          |    name[2] = 115  // s
          |    name[3] = 116  // t
          |    name[4] = 0
          |    val ino = fs_create(1, &name[0], 1, 0x1B6)
          |
          |    var data: [4]i8
          |    data[0] = 65
          |    data[1] = 66
          |    data[2] = 67
          |    data[3] = 68
          |    fs_write(ino, &data[0], 0, 4)
          |
          |    var buf: [4]i8
          |    val nr = fs_read(ino, &buf[0], 0, 4)
          |    if nr == 4
          |        putc(buf[0])
          |        putc(buf[1])
          |        putc(buf[2])
          |        putc(buf[3])
          |""".stripMargin
    ), prefill = "\n")
    output should include("ABCD")
  }

  "FS: stat returns file size after write" in {
    val (_, output) = runFS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(tfs_server, 0x20000, 0x1E000, "tfs")
          |    create_thread(client, 0x30000, 0x2E000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |client()
          |    sleep(100)
          |    var name: [2]i8
          |    name[0] = 102  // f
          |    name[1] = 0
          |    val ino = fs_create(1, &name[0], 1, 0x1B6)
          |
          |    var data: [10]i8
          |    var i = 0
          |    while i < 10
          |        data[i] = 65 + i
          |        i += 1
          |    fs_write(ino, &data[0], 0, 10)
          |
          |    var st: [7]int
          |    fs_stat(ino, &st[0])
          |    if st[4] == 10
          |        putc('Y')
          |    else
          |        putc('N')
          |""".stripMargin
    ), prefill = "\n")
    output should include("Y")
  }

  "FS: mkdir and readdir" in {
    val (_, output) = runFS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(tfs_server, 0x20000, 0x1E000, "tfs")
          |    create_thread(client, 0x30000, 0x2E000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |client()
          |    sleep(100)
          |    var dname: [4]i8
          |    dname[0] = 115  // s
          |    dname[1] = 117  // u
          |    dname[2] = 98   // b
          |    dname[3] = 0
          |    val dir_ino = fs_mkdir(1, &dname[0], 0x1FF)
          |    if dir_ino > 1
          |        putc('D')
          |
          |    var entry_ino: int
          |    var entry_name: [14]i8
          |    fs_readdir(1, 0, &entry_ino, &entry_name[0])
          |    if entry_name[0] == 46
          |        putc('.')
          |    fs_readdir(1, 1, &entry_ino, &entry_name[0])
          |    if entry_name[0] == 46
          |        if entry_name[1] == 46
          |            putc(':')
          |    fs_readdir(1, 2, &entry_ino, &entry_name[0])
          |    if entry_name[0] == 115
          |        putc('S')
          |""".stripMargin
    ), prefill = "\n")
    output should include("D.:S")
  }

  "FS: unlink removes file" in {
    val (_, output) = runFS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(tfs_server, 0x20000, 0x1E000, "tfs")
          |    create_thread(client, 0x30000, 0x2E000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |client()
          |    sleep(100)
          |    var name: [2]i8
          |    name[0] = 120  // x
          |    name[1] = 0
          |    val ino = fs_create(1, &name[0], 1, 0x1B6)
          |    if ino > 1
          |        putc('C')
          |    val r = fs_unlink(1, &name[0])
          |    if r == 0
          |        putc('U')
          |    var path: [3]i8
          |    path[0] = 47   // /
          |    path[1] = 120  // x
          |    path[2] = 0
          |    val ino2 = fs_open(&path[0])
          |    if ino2 == -1
          |        putc('G')
          |""".stripMargin
    ), prefill = "\n")
    output should include("CUG")
  }

  "FS: open file from prefilled filesystem" in {
    val (_, output) = runFS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(tfs_server, 0x20000, 0x1E000, "tfs")
          |    create_thread(client, 0x30000, 0x2E000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |client()
          |    sleep(100)
          |    var path: [7]i8
          |    path[0] = 47   // /
          |    path[1] = 104  // h
          |    path[2] = 101  // e
          |    path[3] = 108  // l
          |    path[4] = 108  // l
          |    path[5] = 111  // o
          |    path[6] = 0
          |    val ino = fs_open(&path[0])
          |    if ino > 0
          |        putc('F')
          |        var buf: [16]i8
          |        val nr = fs_read(ino, &buf[0], 0, 16)
          |        var i = 0
          |        while i < nr
          |            putc(buf[i])
          |            i += 1
          |""".stripMargin
    ), prefill = """/hello file "Hello!"""")
    output should include("FHello!")
  }

  "FS: read file from prefilled nested path" in {
    val (_, output) = runFS(Map(
      "app" ->
        """import oskit.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(disk_server, 0x10000, 0xE000, "disk")
          |    create_thread(tfs_server, 0x20000, 0x1E000, "tfs")
          |    create_thread(client, 0x30000, 0x2E000, "cli")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |client()
          |    sleep(100)
          |    var path: [10]i8
          |    path[0] = 47   // /
          |    path[1] = 100  // d
          |    path[2] = 105  // i
          |    path[3] = 114  // r
          |    path[4] = 47   // /
          |    path[5] = 102  // f
          |    path[6] = 105  // i
          |    path[7] = 108  // l
          |    path[8] = 101  // e
          |    path[9] = 0
          |    val ino = fs_open(&path[0])
          |    if ino > 0
          |        var buf: [16]i8
          |        val nr = fs_read(ino, &buf[0], 0, 16)
          |        var i = 0
          |        while i < nr
          |            putc(buf[i])
          |            i += 1
          |    else
          |        putc('?')
          |""".stripMargin
    ), prefill =
      """/dir dir
        |/dir/file file "OK"""".stripMargin)
    output should include("OK")
  }
}
