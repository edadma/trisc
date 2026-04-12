package io.github.edadma.trisc

/**
 * Progressive reproduction tests for the string concat OS bug.
 *
 * The `cd dev` command in nsh produces "////" instead of "/dev".
 * Codegen tests pass. These tests progressively add OS-specific
 * context to isolate what triggers the corruption.
 */
class StringConcatBugTests extends OSKitTestHelpers {

  // Minimal boot that works with the linker script (8MB RAM, STDOUT at 0x800000)
  private val linkerCompatBoot: String =
    s"""STDOUT = ${Runtime.stdoutAddress}
      |
      |segment vectors
      |
      |  dl ${Runtime.stdoutAddress - 8}
      |  dl boot
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |
      |segment code
      |
      |extern main
      |
      |global boot, func
      |entry boot
      |
      |boot
      |  movi r4, main
      |  jalr r6, r4
      |  halt
      |
      |global putchar, func
      |
      |putchar
      |  movi r2, STDOUT
      |  stb r1, r2, r0
      |  jalr r0, r6
      |
      |; puts: write string to stdout
      |; r1 = address of {ptr, len} string struct (16 bytes)
      |global puts, func
      |
      |puts
      |  ldd r2, r1, r0       ; r2 = ptr
      |  addi r1, r1, 8
      |  ldd r3, r1, r0       ; r3 = len
      |  movi r1, STDOUT
      |.puts_loop
      |  beq r3, r0, .puts_done
      |  ldb r4, r2, r0
      |  stb r4, r1, r0
      |  addi r2, r2, 1
      |  addi r3, r3, -1
      |  bra .puts_loop
      |.puts_done
      |  ldi r1, 0
      |  jalr r0, r6
      |
      |global default_isr, func
      |
      |default_isr
      |  halt
      |""".stripMargin

  private def readSysl(path: String): String = scala.io.Source.fromFile(path).mkString

  private lazy val allocSource = readSysl("posix/stdlib/alloc.sysl")
  private lazy val stringSource = readSysl("posix/string/string.sysl")
  private lazy val ctypeSource = readSysl("posix/ctype/ctype.sysl")

  /**
   * Run sources with the linker script and real sbrk (linker-defined _heap_start).
   * Uses 1MB RAM + stdout device, same memory layout as the OS build.
   */
  private def runWithLinkerScript(
      sources: Map[String, String],
      maxCycles: Int = 200000,
  ): (CPU, String) =
    val bootTof = assemble(linkerCompatBoot, relocatable = true)
    val driver = new SyslDriver
    val result = driver.compile(sources)
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
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val intc = new InterruptController(Runtime.intcAddress)
    val dma = new DMA(Runtime.dmaAddress, null, intc, 4)
    val mem = new Memory("Memory", ram, stdout, intc, dma)
    dma.mem = mem
    linked.load(mem)
    val cpu = new CPU(mem) { this.limit = maxCycles; quiet = true }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  private def baseSources(mainSource: String): Map[String, String] = Map(
    "posix/unistd/sbrk" -> sbrkSysl,
    "posix/string/string" -> stringSource,
    "posix/ctype/ctype" -> ctypeSource,
    "posix/stdlib/alloc" -> allocSource,
    "main" -> mainSource,
  )

  // ===== Level 1: Linker script + real sbrk, single unit =====

  "L1: linker script concat to global" taggedAs Slow in {
    val (_, out) = runWithLinkerScript(baseSources(
      """import posix.stdlib.*
        |
        |var g: string
        |
        |main() -> int
        |    g = "/"
        |    g = g + "dev"
        |    puts(g)
        |    0
        |""".stripMargin))
    out shouldBe "/dev"
  }

  "L1: linker script concat via function" taggedAs Slow in {
    val (_, out) = runWithLinkerScript(baseSources(
      """import posix.stdlib.*
        |
        |var cwd: string
        |
        |do_cd(arg: string)
        |    if len(cwd) > 1
        |        cwd = cwd + "/" + arg
        |    else
        |        cwd = cwd + arg
        |
        |main() -> int
        |    cwd = "/"
        |    do_cd("dev")
        |    puts(cwd)
        |    0
        |""".stripMargin))
    out shouldBe "/dev"
  }

  "L1: linker script concat via argv" taggedAs Slow in {
    val (_, out) = runWithLinkerScript(baseSources(
      """import posix.stdlib.*
        |
        |var cwd: string
        |
        |do_cd(argc: int, argv: *string)
        |    val arg = argv[1]
        |    if len(cwd) > 1
        |        cwd = cwd + "/" + arg
        |    else
        |        cwd = cwd + arg
        |
        |main() -> int
        |    cwd = "/"
        |    var args: [4]string
        |    args[0] = "cd"
        |    args[1] = "dev"
        |    do_cd(2, args)
        |    puts(cwd)
        |    0
        |""".stripMargin))
    out shouldBe "/dev"
  }

  // ===== Level 2: Cross-module — cd function in separate module =====

  "L2: cross-module concat" taggedAs Slow in {
    val (_, out) = runWithLinkerScript(Map(
      "posix/unistd/sbrk" -> sbrkSysl,
      "posix/string/string" -> stringSource,
      "posix/ctype/ctype" -> ctypeSource,
      "posix/stdlib/alloc" -> allocSource,
      "myshell/shell" ->
        """module myshell
          |
          |import posix.stdlib.*
          |
          |var cwd: string
          |
          |shell_init()
          |    cwd = "/"
          |
          |shell_cd(argc: int, argv: *string)
          |    val arg = argv[1]
          |    if len(cwd) > 1
          |        cwd = cwd + "/" + arg
          |    else
          |        cwd = cwd + arg
          |
          |shell_cwd() -> string
          |    cwd
          |""".stripMargin,
      "main" ->
        """import myshell.*
          |
          |main() -> int
          |    shell_init()
          |    var args: [4]string
          |    args[0] = "cd"
          |    args[1] = "dev"
          |    shell_cd(2, args)
          |    puts(shell_cwd())
          |    0
          |""".stripMargin,
    ))
    out shouldBe "/dev"
  }

  // ===== Level 3: Cross-module + tokenizer (like nsh) =====

  "L3: cross-module with tokenizer" taggedAs Slow in {
    val (_, out) = runWithLinkerScript(Map(
      "posix/unistd/sbrk" -> sbrkSysl,
      "posix/string/string" -> stringSource,
      "posix/ctype/ctype" -> ctypeSource,
      "posix/stdlib/alloc" -> allocSource,
      "myshell/shell" ->
        """module myshell
          |
          |import posix.stdlib.*
          |
          |var cwd: string
          |
          |val MAX_ARGS = 16
          |
          |tokenize(buf: *byte, buflen: int, argv: *string) -> int
          |    var argc = 0
          |    var pos = 0
          |    while pos < buflen
          |        while pos < buflen
          |            if buf[pos] != ' '
          |                break
          |            pos += 1
          |        if pos >= buflen
          |            break
          |        val start = pos
          |        while pos < buflen
          |            if buf[pos] == ' '
          |                break
          |            pos += 1
          |        if argc < MAX_ARGS
          |            argv[argc] = string(buf + start, pos - start)
          |            argc += 1
          |    argc
          |
          |shell_init()
          |    cwd = "/"
          |
          |cmd_cd(argc: int, argv: *string)
          |    if argc < 2
          |        cwd = "/"
          |        return
          |    val arg = argv[1]
          |    if arg[0] == '/'
          |        cwd = arg
          |    else
          |        if len(cwd) > 1
          |            cwd = cwd + "/" + arg
          |        else
          |            cwd = cwd + arg
          |
          |dispatch(argc: int, argv: *string)
          |    if argc == 0
          |        return
          |    val cmd = argv[0]
          |    if cmd == "cd"
          |        cmd_cd(argc, argv)
          |
          |shell_run(line: *byte, linelen: int)
          |    var argv: [16]string
          |    val argc = tokenize(line, linelen, argv)
          |    dispatch(argc, argv)
          |
          |shell_cwd() -> string
          |    cwd
          |""".stripMargin,
      "main" ->
        """import myshell.*
          |
          |main() -> int
          |    shell_init()
          |    var line: [64]byte
          |    // "cd dev"
          |    line[0] = 99   // c
          |    line[1] = 100  // d
          |    line[2] = 32   // space
          |    line[3] = 100  // d
          |    line[4] = 101  // e
          |    line[5] = 118  // v
          |    shell_run(line, 6)
          |    puts(shell_cwd())
          |    0
          |""".stripMargin,
    ))
    out shouldBe "/dev"
  }

  // ===== Level 4: Cross-module + many globals (bigger BSS) =====

  "L4: cross-module with large BSS" taggedAs Slow in {
    val (_, out) = runWithLinkerScript(Map(
      "posix/unistd/sbrk" -> sbrkSysl,
      "posix/string/string" -> stringSource,
      "posix/ctype/ctype" -> ctypeSource,
      "posix/stdlib/alloc" -> allocSource,
      "myshell/shell" ->
        """module myshell
          |
          |import posix.stdlib.*
          |
          |var cwd: string
          |var cwd_ino = 1
          |
          |// Buffers to increase BSS, mimicking nsh
          |var nsh_out: [140]byte
          |var nsh_outpos = 0
          |
          |val MAX_ARGS = 16
          |
          |nsh_print(s: string)
          |    val p = *byte(s)
          |    val n = len(s)
          |    for var i = 0; i < n; i++
          |        putchar(p[i])
          |
          |tokenize(buf: *byte, buflen: int, argv: *string) -> int
          |    var argc = 0
          |    var pos = 0
          |    while pos < buflen
          |        while pos < buflen
          |            if buf[pos] != ' '
          |                break
          |            pos += 1
          |        if pos >= buflen
          |            break
          |        val start = pos
          |        while pos < buflen
          |            if buf[pos] == ' '
          |                break
          |            pos += 1
          |        if argc < MAX_ARGS
          |            argv[argc] = string(buf + start, pos - start)
          |            argc += 1
          |    argc
          |
          |shell_init()
          |    cwd = "/"
          |
          |cmd_cd(argc: int, argv: *string)
          |    if argc < 2
          |        cwd_ino = 1
          |        cwd = "/"
          |        return
          |    val arg = argv[1]
          |    if arg[0] == '/'
          |        cwd = arg
          |    else
          |        if len(cwd) > 1
          |            cwd = cwd + "/" + arg
          |        else
          |            cwd = cwd + arg
          |
          |cmd_pwd(argc: int, argv: *string)
          |    nsh_print(cwd)
          |    putchar(10)
          |
          |dispatch(argc: int, argv: *string)
          |    if argc == 0
          |        return
          |    val cmd = argv[0]
          |    if cmd == "cd"
          |        cmd_cd(argc, argv)
          |    else if cmd == "pwd"
          |        cmd_pwd(argc, argv)
          |    else
          |        nsh_print(cmd)
          |        nsh_print(": not found\n")
          |
          |shell_run(line: *byte, linelen: int)
          |    var argv: [16]string
          |    val argc = tokenize(line, linelen, argv)
          |    dispatch(argc, argv)
          |
          |shell_cwd() -> string
          |    cwd
          |""".stripMargin,
      "main" ->
        """import myshell.*
          |
          |main() -> int
          |    shell_init()
          |    var line: [64]byte
          |    // "cd dev"
          |    line[0] = 99   // c
          |    line[1] = 100  // d
          |    line[2] = 32   // space
          |    line[3] = 100  // d
          |    line[4] = 101  // e
          |    line[5] = 118  // v
          |    shell_run(line, 6)
          |    puts(shell_cwd())
          |    0
          |""".stripMargin,
    ))
    out shouldBe "/dev"
  }

  // ===== Level 4b: Function-concat-then-concat (no OS, no interrupts) =====

  "L4b: function concat then caller concat" taggedAs Slow in {
    val (_, out) = runWithLinkerScript(baseSources(
      """import posix.stdlib.*
        |
        |var cwd: string
        |
        |do_concat_len(a: string, b: string) -> int
        |    val result = a + b
        |    len(result)
        |
        |main() -> int
        |    cwd = "/"
        |    val n = do_concat_len(cwd, "dev")
        |    cwd = cwd + "dev"
        |    puts(cwd)
        |    0
        |""".stripMargin))
    out shouldBe "/dev"
  }

  // L4c: Cross-module with tokenized args
  "L4c: cross-module tokenized concat" taggedAs Slow in {
    val (_, out) = runWithLinkerScript(Map(
      "posix/unistd/sbrk" -> sbrkSysl,
      "posix/string/string" -> stringSource,
      "posix/ctype/ctype" -> ctypeSource,
      "posix/stdlib/alloc" -> allocSource,
      "mymod/lib" ->
        """module mymod
          |
          |import posix.stdlib.*
          |
          |var cwd: string
          |
          |val MAX_ARGS = 16
          |
          |tokenize(buf: *byte, buflen: int, argv: *string) -> int
          |    var argc = 0
          |    var pos = 0
          |    while pos < buflen
          |        while pos < buflen
          |            if buf[pos] != ' '
          |                break
          |            pos += 1
          |        if pos >= buflen
          |            break
          |        val start = pos
          |        while pos < buflen
          |            if buf[pos] == ' '
          |                break
          |            pos += 1
          |        if argc < MAX_ARGS
          |            argv[argc] = string(buf + start, pos - start)
          |            argc += 1
          |    argc
          |
          |do_concat_len(a: string, b: string) -> int
          |    val result = a + b
          |    len(result)
          |
          |cmd_cd(argc: int, argv: *string)
          |    if argc < 2
          |        cwd = "/"
          |        return
          |    val n = do_concat_len(cwd, argv[1])
          |    val arg = argv[1]
          |    if arg[0] == '/'
          |        cwd = arg
          |    else
          |        if len(cwd) > 1
          |            cwd = cwd + "/" + arg
          |        else
          |            cwd = cwd + arg
          |
          |do_test()
          |    cwd = "/"
          |    var line: [256]byte
          |    var argv: [16]string
          |    line[0] = 99
          |    line[1] = 100
          |    line[2] = 32
          |    line[3] = 100
          |    line[4] = 101
          |    line[5] = 118
          |    val argc = tokenize(line, 6, argv)
          |    if argc > 0
          |        val cmd = argv[0]
          |        if cmd == "cd"
          |            cmd_cd(argc, argv)
          |    puts(cwd)
          |""".stripMargin,
      "main" ->
        """import mymod.*
          |
          |main() -> int
          |    do_test()
          |    0
          |""".stripMargin,
    ))
    out shouldBe "/dev"
  }

  // L4d: Same as L6a's shell module but with linkerScript, no OS kernel
  "L4d: L6a shell code without OS" taggedAs Slow in {
    val (_, out) = runWithLinkerScript(Map(
      "posix/unistd/sbrk" -> sbrkSysl,
      "posix/string/string" -> stringSource,
      "posix/ctype/ctype" -> ctypeSource,
      "posix/stdlib/alloc" -> allocSource,
      "concatmod3/shell" ->
        """module concatmod3
          |
          |import posix.stdlib.*
          |
          |var cwd: string
          |var cwd_ino = 1
          |
          |var nsh_out: [140]byte
          |var nsh_outpos = 0
          |
          |do_concat_len(a: string, b: string) -> int
          |    val result = a + b
          |    len(result)
          |
          |cmd_cd_simple(argc: int, argv: *string)
          |    if argc < 2
          |        cwd_ino = 1
          |        cwd = "/"
          |        return
          |    val n = do_concat_len(cwd, argv[1])
          |    val arg = argv[1]
          |    if arg[0] == '/'
          |        cwd = arg
          |    else
          |        if len(cwd) > 1
          |            cwd = cwd + "/" + arg
          |        else
          |            cwd = cwd + arg
          |
          |val MAX_ARGS = 16
          |
          |tokenize(buf: *byte, buflen: int, argv: *string) -> int
          |    var argc = 0
          |    var pos = 0
          |    while pos < buflen
          |        while pos < buflen
          |            if buf[pos] != ' '
          |                break
          |            pos += 1
          |        if pos >= buflen
          |            break
          |        val start = pos
          |        while pos < buflen
          |            if buf[pos] == ' '
          |                break
          |            pos += 1
          |        if argc < MAX_ARGS
          |            argv[argc] = string(buf + start, pos - start)
          |            argc += 1
          |    argc
          |
          |concat_test3()
          |    cwd = "/"
          |    var line: [256]byte
          |    var argv: [16]string
          |    line[0] = 99
          |    line[1] = 100
          |    line[2] = 32
          |    line[3] = 100
          |    line[4] = 101
          |    line[5] = 118
          |    val argc = tokenize(line, 6, argv)
          |    if argc > 0
          |        val cmd = argv[0]
          |        if cmd == "cd"
          |            cmd_cd_simple(argc, argv)
          |    puts(cwd)
          |""".stripMargin,
      "main" ->
        """import concatmod3.*
          |
          |main() -> int
          |    concat_test3()
          |    0
          |""".stripMargin,
    ))
    out shouldBe "/dev"
  }

  // ===== Level 5: Full OS build, simple concat app (like nsh setup) =====

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
  private lazy val memSysl: String   = readLsysl("std/mem/mem.lsysl")
  private lazy val halMemSysl: String = readLsysl("oskit/hal/mem_dma.lsysl")
  private lazy val configSysl: String = scala.io.Source.fromFile("oskit/config/config.sysl").mkString
  private lazy val vfsSrvSysl: String = readLsysl("oskit/servers/vfs.lsysl")
  private lazy val debugSysl: String = readLsysl("std/debug/debug.lsysl")

  private lazy val concatAppLinked: TOF =
    val bootTof    = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel/kernel"         -> kernelSysl,
      "oskit/services/services"     -> servicesSysl,
      "oskit/kernel/timer"          -> timerSysl,
      "oskit/sync/semaphore"        -> semaphoreSysl,
      "oskit/sync/mutex"            -> mutexSysl,
      "oskit/ipc/ipc"               -> ipcSysl,
      "std/mem/mem"                 -> memSysl,
      "oskit/hal/mem"               -> halMemSysl,
      "oskit/config/config"         -> configSysl,
      "std/debug/debug"             -> debugSysl,
      "oskit/drivers/disk/disk"     -> diskSysl,
      "oskit/drivers/kbd/keyboard"  -> kbdSysl,
      "oskit/drivers/tty/tty"       -> ttySysl,
      "oskit/fs/tfs"                -> tfsSysl,
      "oskit/fs/client"             -> fsClientSysl,
      "oskit/servers/tfs"           -> tfsSrvSysl,
      "oskit/servers/vfs"           -> vfsSrvSysl,
      "posix/unistd/sbrk"          -> sbrkSysl,
      "posix/string/string"        -> stringSource,
      "posix/ctype/ctype"          -> ctypeSource,
      "posix/stdlib/alloc"         -> allocSource,
      "concatmod/shell" ->
        """module concatmod
          |
          |import posix.stdlib.*
          |import oskit.drivers.tty.{tty_putc, tty_puts}
          |
          |var cwd: string
          |var cwd_ino = 1
          |
          |// Same buffers as nsh to match BSS layout
          |var nsh_out: [140]byte
          |var nsh_outpos = 0
          |
          |val MAX_ARGS = 16
          |
          |nsh_flush()
          |    if nsh_outpos > 0
          |        tty_puts(nsh_out, nsh_outpos)
          |        nsh_outpos = 0
          |
          |nsh_putc(ch: char)
          |    if nsh_outpos >= 120
          |        nsh_flush()
          |    nsh_out[nsh_outpos] = byte(ch)
          |    nsh_outpos += 1
          |
          |nsh_print(s: string)
          |    val p = *byte(s)
          |    val n = len(s)
          |    for var i = 0; i < n; i++
          |        if nsh_outpos >= 120
          |            nsh_flush()
          |        nsh_out[nsh_outpos] = p[i]
          |        nsh_outpos += 1
          |
          |nsh_newline()
          |    nsh_putc('\n')
          |    nsh_flush()
          |
          |tokenize(buf: *byte, buflen: int, argv: *string) -> int
          |    var argc = 0
          |    var pos = 0
          |    while pos < buflen
          |        while pos < buflen
          |            if buf[pos] != ' '
          |                break
          |            pos += 1
          |        if pos >= buflen
          |            break
          |        val start = pos
          |        while pos < buflen
          |            if buf[pos] == ' '
          |                break
          |            pos += 1
          |        if argc < MAX_ARGS
          |            argv[argc] = string(buf + start, pos - start)
          |            argc += 1
          |    argc
          |
          |cmd_cd(argc: int, argv: *string)
          |    if argc < 2
          |        cwd_ino = 1
          |        cwd = "/"
          |        return
          |    val arg = argv[1]
          |    if arg[0] == '/'
          |        cwd = arg
          |    else
          |        if len(cwd) > 1
          |            cwd = cwd + "/" + arg
          |        else
          |            cwd = cwd + arg
          |
          |cmd_pwd(argc: int, argv: *string)
          |    nsh_print(cwd)
          |    nsh_newline()
          |
          |dispatch(argc: int, argv: *string)
          |    if argc == 0
          |        return
          |    val cmd = argv[0]
          |    if cmd == "cd"
          |        cmd_cd(argc, argv)
          |    else if cmd == "pwd"
          |        cmd_pwd(argc, argv)
          |    else
          |        nsh_print(cmd)
          |        nsh_print(": not found\n")
          |
          |concat_test()
          |    cwd = "/"
          |    var line: [256]byte
          |    var argv: [16]string
          |    // Execute "cd dev"
          |    line[0] = 99   // c
          |    line[1] = 100  // d
          |    line[2] = 32   // space
          |    line[3] = 100  // d
          |    line[4] = 101  // e
          |    line[5] = 118  // v
          |    val argc = tokenize(line, 6, argv)
          |    dispatch(argc, argv)
          |    // Print result
          |    cmd_pwd(0, argv)
          |    nsh_flush()
          |""".stripMargin,
      "app" ->
        """import oskit.kernel.*
          |import oskit.ipc.*
          |import oskit.drivers.disk.disk_server
          |import oskit.servers.{tfs_server, vfs_server}
          |import oskit.drivers.tty.tty_server
          |import concatmod.concat_test
          |import oskit.services.sleep
          |
          |init()
          |    create_thread(disk_server, 0x600000, 0x600000, "disk")
          |    sleep(5)
          |    create_thread(tfs_server, 0x90000, 0x90000, "tfs")
          |    create_thread(tty_server, 0xA0000, 0xA0000, "tty")
          |    create_thread(vfs_server, 0xB0000, 0xB0000, "vfs")
          |    sleep(5)
          |    create_thread(concat_test, 0xC0000, 0xC0000, "test")
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

  "L5: full OS concat test" taggedAs Slow in {
    val linked = concatAppLinked

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
      prefill = "\n",
      maxInodes = 32,
    )
    val dma = new DMA(Runtime.dmaAddress, null, intc, 4)
    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, ramdisk, dma)
    dma.mem = mem
    linked.load(mem)

    val cpu = new CPU(mem, Seq(timer, intc)) { this.limit = 5200000; quiet = true }
    cpu.reset()
    cpu.run()
    output.toString should include("/dev")
  }

  // ===== Level 6: Full OS + IPC calls before concat (like real cmd_cd) =====

  private lazy val ipcConcatLinked: TOF =
    val bootTof    = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel/kernel"         -> kernelSysl,
      "oskit/services/services"     -> servicesSysl,
      "oskit/kernel/timer"          -> timerSysl,
      "oskit/sync/semaphore"        -> semaphoreSysl,
      "oskit/sync/mutex"            -> mutexSysl,
      "oskit/ipc/ipc"               -> ipcSysl,
      "std/mem/mem"                 -> memSysl,
      "oskit/hal/mem"               -> halMemSysl,
      "oskit/config/config"         -> configSysl,
      "std/debug/debug"             -> debugSysl,
      "oskit/drivers/disk/disk"     -> diskSysl,
      "oskit/drivers/kbd/keyboard"  -> kbdSysl,
      "oskit/drivers/tty/tty"       -> ttySysl,
      "oskit/fs/tfs"                -> tfsSysl,
      "oskit/fs/client"             -> fsClientSysl,
      "oskit/servers/tfs"           -> tfsSrvSysl,
      "oskit/servers/vfs"           -> vfsSrvSysl,
      "posix/unistd/sbrk"          -> sbrkSysl,
      "posix/string/string"        -> stringSource,
      "posix/ctype/ctype"          -> ctypeSource,
      "posix/stdlib/alloc"         -> allocSource,
      "concatmod2/shell" ->
        """module concatmod2
          |
          |import posix.stdlib.*
          |import oskit.fs.ROOT_INODE
          |import oskit.drivers.tty.{tty_putc, tty_puts}
          |import oskit.fs.{fs_open, fs_stat}
          |
          |var cwd: string
          |var cwd_ino = 1
          |
          |var nsh_out: [140]byte
          |var nsh_outpos = 0
          |
          |val MAX_ARGS = 16
          |
          |nsh_flush()
          |    if nsh_outpos > 0
          |        tty_puts(nsh_out, nsh_outpos)
          |        nsh_outpos = 0
          |
          |nsh_putc(ch: char)
          |    if nsh_outpos >= 120
          |        nsh_flush()
          |    nsh_out[nsh_outpos] = byte(ch)
          |    nsh_outpos += 1
          |
          |nsh_print(s: string)
          |    val p = *byte(s)
          |    val n = len(s)
          |    for var i = 0; i < n; i++
          |        if nsh_outpos >= 120
          |            nsh_flush()
          |        nsh_out[nsh_outpos] = p[i]
          |        nsh_outpos += 1
          |
          |nsh_newline()
          |    nsh_putc('\n')
          |    nsh_flush()
          |
          |tokenize(buf: *byte, buflen: int, argv: *string) -> int
          |    var argc = 0
          |    var pos = 0
          |    while pos < buflen
          |        while pos < buflen
          |            if buf[pos] != ' '
          |                break
          |            pos += 1
          |        if pos >= buflen
          |            break
          |        val start = pos
          |        while pos < buflen
          |            if buf[pos] == ' '
          |                break
          |            pos += 1
          |        if argc < MAX_ARGS
          |            argv[argc] = string(buf + start, pos - start)
          |            argc += 1
          |    argc
          |
          |// Path resolution — same as nsh
          |nsh_resolve(path: string) -> int
          |    if len(path) == 0
          |        return cwd_ino
          |    if path[0] == '/'
          |        return fs_open(path)
          |    if len(cwd) > 1
          |        return fs_open(cwd + "/" + path)
          |    fs_open(cwd + path)
          |
          |cmd_cd(argc: int, argv: *string)
          |    if argc < 2
          |        cwd_ino = ROOT_INODE
          |        cwd = "/"
          |        return
          |    val ino = nsh_resolve(argv[1])
          |    if ino < 0
          |        nsh_print(argv[1])
          |        nsh_print(": not found\n")
          |        return
          |    // Verify it's a directory
          |    var st: [7]int
          |    fs_stat(ino, st)
          |    val ftype = (st[0] >> 12) & 0xF
          |    if ftype != 2
          |        nsh_print("not a directory\n")
          |        return
          |    cwd_ino = ino
          |    val arg = argv[1]
          |    if arg[0] == '/'
          |        cwd = arg
          |    else
          |        if len(cwd) > 1
          |            cwd = cwd + "/" + arg
          |        else
          |            cwd = cwd + arg
          |
          |cmd_pwd(argc: int, argv: *string)
          |    nsh_print(cwd)
          |    nsh_newline()
          |
          |dispatch(argc: int, argv: *string)
          |    if argc == 0
          |        return
          |    val cmd = argv[0]
          |    if cmd == "cd"
          |        cmd_cd(argc, argv)
          |    else if cmd == "pwd"
          |        cmd_pwd(argc, argv)
          |
          |concat_test2()
          |    cwd = "/"
          |    var line: [256]byte
          |    var argv: [16]string
          |    // Execute "cd dev"
          |    line[0] = 99   // c
          |    line[1] = 100  // d
          |    line[2] = 32   // space
          |    line[3] = 100  // d
          |    line[4] = 101  // e
          |    line[5] = 118  // v
          |    val argc = tokenize(line, 6, argv)
          |    dispatch(argc, argv)
          |    cmd_pwd(0, argv)
          |    nsh_flush()
          |""".stripMargin,
      "app" ->
        """import oskit.kernel.*
          |import oskit.ipc.*
          |import oskit.drivers.disk.disk_server
          |import oskit.servers.{tfs_server, vfs_server}
          |import oskit.drivers.tty.tty_server
          |import concatmod2.concat_test2
          |import oskit.services.sleep
          |
          |init()
          |    create_thread(disk_server, 0x600000, 0x600000, "disk")
          |    sleep(5)
          |    create_thread(tfs_server, 0x90000, 0x90000, "tfs")
          |    create_thread(tty_server, 0xA0000, 0xA0000, "tty")
          |    create_thread(vfs_server, 0xB0000, 0xB0000, "vfs")
          |    sleep(5)
          |    create_thread(concat_test2, 0xC0000, 0xC0000, "test")
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

  // L6a: Only nsh_resolve (fs_open IPC), no fs_stat
  "L6a: IPC fs_open only before concat" taggedAs Slow in {
    val bootTof    = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel/kernel"         -> kernelSysl,
      "oskit/services/services"     -> servicesSysl,
      "oskit/kernel/timer"          -> timerSysl,
      "oskit/sync/semaphore"        -> semaphoreSysl,
      "oskit/sync/mutex"            -> mutexSysl,
      "oskit/ipc/ipc"               -> ipcSysl,
      "std/mem/mem"                 -> memSysl,
      "oskit/hal/mem"               -> halMemSysl,
      "oskit/config/config"         -> configSysl,
      "std/debug/debug"             -> debugSysl,
      "oskit/drivers/disk/disk"     -> diskSysl,
      "oskit/drivers/kbd/keyboard"  -> kbdSysl,
      "oskit/drivers/tty/tty"       -> ttySysl,
      "oskit/fs/tfs"                -> tfsSysl,
      "oskit/fs/client"             -> fsClientSysl,
      "oskit/servers/tfs"           -> tfsSrvSysl,
      "oskit/servers/vfs"           -> vfsSrvSysl,
      "posix/unistd/sbrk"          -> sbrkSysl,
      "posix/string/string"        -> stringSource,
      "posix/ctype/ctype"          -> ctypeSource,
      "posix/stdlib/alloc"         -> allocSource,
      "concatmod3/shell" ->
        """module concatmod3
          |
          |import posix.stdlib.*
          |import oskit.fs.ROOT_INODE
          |import oskit.drivers.tty.{tty_putc, tty_puts}
          |import oskit.fs.fs_open
          |
          |var cwd: string
          |var cwd_ino = 1
          |
          |var nsh_out: [140]byte
          |var nsh_outpos = 0
          |
          |nsh_flush()
          |    if nsh_outpos > 0
          |        tty_puts(nsh_out, nsh_outpos)
          |        nsh_outpos = 0
          |
          |nsh_putc(ch: char)
          |    if nsh_outpos >= 120
          |        nsh_flush()
          |    nsh_out[nsh_outpos] = byte(ch)
          |    nsh_outpos += 1
          |
          |nsh_print(s: string)
          |    val p = *byte(s)
          |    val n = len(s)
          |    for var i = 0; i < n; i++
          |        if nsh_outpos >= 120
          |            nsh_flush()
          |        nsh_out[nsh_outpos] = p[i]
          |        nsh_outpos += 1
          |
          |nsh_newline()
          |    nsh_putc('\n')
          |    nsh_flush()
          |
          |// Function takes string args AND does concat
          |do_concat_len(a: string, b: string) -> int
          |    val result = a + b
          |    len(result)
          |
          |cmd_cd_simple(argc: int, argv: *string)
          |    if argc < 2
          |        cwd_ino = ROOT_INODE
          |        cwd = "/"
          |        return
          |    val n = do_concat_len(cwd, argv[1])
          |    val arg = argv[1]
          |    if arg[0] == '/'
          |        cwd = arg
          |    else
          |        if len(cwd) > 1
          |            cwd = cwd + "/" + arg
          |        else
          |            cwd = cwd + arg
          |
          |val MAX_ARGS = 16
          |
          |tokenize(buf: *byte, buflen: int, argv: *string) -> int
          |    var argc = 0
          |    var pos = 0
          |    while pos < buflen
          |        while pos < buflen
          |            if buf[pos] != ' '
          |                break
          |            pos += 1
          |        if pos >= buflen
          |            break
          |        val start = pos
          |        while pos < buflen
          |            if buf[pos] == ' '
          |                break
          |            pos += 1
          |        if argc < MAX_ARGS
          |            argv[argc] = string(buf + start, pos - start)
          |            argc += 1
          |    argc
          |
          |concat_test3()
          |    cwd = "/"
          |    var line: [256]byte
          |    var argv: [16]string
          |    line[0] = 99   // c
          |    line[1] = 100  // d
          |    line[2] = 32   // space
          |    line[3] = 100  // d
          |    line[4] = 101  // e
          |    line[5] = 118  // v
          |    val argc = tokenize(line, 6, argv)
          |    cmd_cd_simple(argc, argv)
          |    nsh_print(cwd)
          |    nsh_newline()
          |    nsh_flush()
          |""".stripMargin,
      "app" ->
        """import oskit.kernel.*
          |import oskit.ipc.*
          |import oskit.drivers.disk.disk_server
          |import oskit.servers.{tfs_server, vfs_server}
          |import oskit.drivers.tty.tty_server
          |import concatmod3.concat_test3
          |import oskit.services.sleep
          |
          |init()
          |    create_thread(disk_server, 0x600000, 0x600000, "disk")
          |    sleep(5)
          |    create_thread(tfs_server, 0x90000, 0x90000, "tfs")
          |    create_thread(tty_server, 0xA0000, 0xA0000, "tty")
          |    create_thread(vfs_server, 0xB0000, 0xB0000, "vfs")
          |    sleep(5)
          |    create_thread(concat_test3, 0xC0000, 0xC0000, "test")
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
    val linked = Linker.link(Seq(bootTof, syslTof), linkerScript, 0)

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
      Runtime.ramdiskAddress, ram, sectors = 64, sectorSize = 4096,
      intc, irq = 3, prefill = "/dev dir\n", maxInodes = 32,
    )
    val dma = new DMA(Runtime.dmaAddress, null, intc, 4)
    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, ramdisk, dma)
    dma.mem = mem
    linked.load(mem)
    val cpu = new CPU(mem, Seq(timer, intc)) { this.limit = 5200000; quiet = true }
    cpu.reset()
    cpu.run()
    output.toString should include("/dev")
  }

  "L6: full OS + IPC before concat" taggedAs Slow in {
    val linked = ipcConcatLinked

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
      prefill = "/dev dir\n",
      maxInodes = 32,
    )
    val dma = new DMA(Runtime.dmaAddress, null, intc, 4)
    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, ramdisk, dma)
    dma.mem = mem
    linked.load(mem)

    val cpu = new CPU(mem, Seq(timer, intc)) { this.limit = 5200000; quiet = true }
    cpu.reset()
    cpu.run()
    output.toString should include("/dev")
  }
}
