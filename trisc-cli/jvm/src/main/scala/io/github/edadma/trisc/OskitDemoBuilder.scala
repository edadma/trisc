package io.github.edadma.trisc

object OskitDemoBuilder:
  import OSKitTestData.*

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private def compileAndLink(allSources: Map[String, String]): TOF =
    val bootTof = assemble(bootAsm, relocatable = true)
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    Linker.link(Seq(bootTof, syslTof), linkerScript, 0)

  private lazy val ipcSysl: String       = readLsysl("oskit/ipc/ipc.lsysl")
  private lazy val diskSysl: String      = readLsysl("oskit/drivers/disk/disk.lsysl")
  private lazy val kbdSysl: String       = readLsysl("oskit/drivers/kbd/keyboard.lsysl")
  private lazy val ttySysl: String       = readLsysl("oskit/drivers/tty/tty.lsysl")
  private lazy val tfsSysl: String       = readLsysl("oskit/fs/tfs.lsysl")
  private lazy val fsClientSysl: String  = readLsysl("oskit/fs/client.lsysl")
  private lazy val tfsSrvSysl: String    = readLsysl("oskit/servers/tfs.lsysl")
  private lazy val initSysl: String      = readLsysl("oskit/apps/init.lsysl")
  private lazy val debugSysl: String     = readLsysl("std/debug/debug.lsysl")
  private lazy val memSysl: String       = readLsysl("std/mem/mem.lsysl")
  private lazy val binarySysl: String    = readLsysl("std/encoding/binary/binary.lsysl")
  private lazy val stringsSysl: String   = readLsysl("std/strings/strings.lsysl")
  private lazy val builderSysl: String   = readLsysl("std/builder/builder.lsysl")
  private lazy val strconvSysl: String   = readLsysl("std/strconv/strconv.lsysl")
  private lazy val resultSysl: String    = readLsysl("std/result/result.lsysl")
  private lazy val errorsSysl: String    = readLsysl("std/errors/errors.lsysl")
  private lazy val utf8Sysl: String      = readLsysl("std/utf8/utf8.lsysl")
  private lazy val loaderSysl: String    = readLsysl("oskit/loader/loader.lsysl")
  private lazy val stdAllocSysl: String  = readLsysl("std/alloc/alloc.lsysl")
  private lazy val sha256Sysl: String    = readLsysl("std/crypto/sha256/sha256.lsysl")
  private lazy val hmacSysl: String      = readLsysl("std/crypto/hmac/hmac.lsysl")
  private lazy val pbkdf2Sysl: String    = readLsysl("std/crypto/pbkdf2/pbkdf2.lsysl")
  private lazy val pmSrvSysl: String     = readLsysl("oskit/servers/pm.lsysl")
  private lazy val vfsSrvSysl: String    = readLsysl("oskit/servers/vfs.lsysl")
  private lazy val rsSrvSysl: String     = readLsysl("oskit/servers/rs.lsysl")
  private lazy val dsSrvSysl: String     = readLsysl("oskit/servers/ds.lsysl")
  private lazy val halMemSysl: String   = readLsysl("oskit/hal/mem_dma.lsysl")
  private lazy val archVmSysl: String   = readLsysl("oskit/arch/trisc/vm.lsysl")
  private lazy val archCpuSysl: String  = readLsysl("oskit/arch/trisc/cpu.lsysl")
  private lazy val archProgConfigSysl: String = scala.io.Source.fromFile("oskit/arch/trisc/prog_config.sysl").mkString
  private lazy val configSysl: String   = scala.io.Source.fromFile("oskit/config/config.sysl").mkString
  private lazy val ipcClientSysl: String = scala.io.Source.fromFile("oskit/ipc/ipc_client.sysl").mkString
  private lazy val mouseSysl: String     = readLsysl("oskit/drivers/mouse/mouse.lsysl")
  private lazy val displaySysl: String   = readLsysl("oskit/drivers/display/display.lsysl")
  private lazy val suitSysl: String      = readLsysl("suit/suit.lsysl")
  private lazy val desktopAppSysl: String =
    scala.io.Source.fromFile("examples/draw-hello/os-desktop.sysl").mkString

  def buildLoginTof(): TOF =
    compileAndLink(
      Map(
        "oskit/kernel/kernel"         -> kernelSysl,
        "oskit/services/services"     -> servicesSysl,
        "oskit/kernel/timer"          -> timerSysl,
        "oskit/sync/semaphore"        -> semaphoreSysl,
        "oskit/sync/mutex"            -> mutexSysl,
        "oskit/ipc/ipc"               -> ipcSysl,
        "oskit/drivers/kbd/keyboard"  -> kbdSysl,
        "posix/unistd/sbrk"           -> sbrkSysl,
        "posix/string/string"         -> posixStringSysl,
        "posix/ctype/ctype"           -> posixCtypeSysl,
        "posix/stdlib/alloc"          -> posixAllocSysl,
        "oskit/hal/mem"               -> halMemSysl,
        "oskit/arch/vm"               -> archVmSysl,
        "oskit/arch/cpu"              -> archCpuSysl,
        "oskit/arch/prog_config"      -> archProgConfigSysl,
        "oskit/config/config"         -> configSysl,
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
            |// Write little-endian i64 to byte pointer
            |k_write_i64(p: *byte, v: i64)
            |    p[0] = byte(v)
            |    p[1] = byte(v >> 8)
            |    p[2] = byte(v >> 16)
            |    p[3] = byte(v >> 24)
            |    p[4] = byte(v >> 32)
            |    p[5] = byte(v >> 40)
            |    p[6] = byte(v >> 48)
            |    p[7] = byte(v >> 56)
            |
            |// Load TRB v1 binary from memory into a target page table.
            |// Returns entry point or -1 on error.
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
            |    // Verify SLIX magic
            |    if bi[0] != byte('S')
            |        return -1
            |    if bi[1] != byte('L')
            |        return -1
            |    if bi[2] != byte('I')
            |        return -1
            |    if bi[3] != byte('X')
            |        return -1
            |
            |    // Find "rs" module in boot info
            |    val mod_count = int(k_read_u32(bi + 4))
            |    var rs_idx = -1
            |    var mi = 0
            |    while mi < mod_count
            |        val entry = bi + 8 + mi * 24
            |        if entry[0] == byte('r')
            |            if entry[1] == byte('s')
            |                if entry[2] == 0
            |                    rs_idx = mi
            |        mi += 1
            |    if rs_idx < 0
            |        return -1
            |
            |    // Get RS module address and size
            |    val rs_entry = bi + 8 + rs_idx * 24
            |    val rs_addr = int(k_read_i64(rs_entry + 8))
            |    val rs_size = int(k_read_i64(rs_entry + 16))
            |
            |    // Create page table for RS (no MMIO needed)
            |    val rs_ptbr = vm_create_server_pt(0)
            |    if rs_ptbr == 0
            |        return -1
            |
            |    // Load RS TRB binary into its page table
            |    val rs_entry_pt = k_load_trb(*byte(rs_addr), rs_size, rs_ptbr)
            |    if rs_entry_pt < 0
            |        return -1
            |
            |    // Write info page at 0xBF000 in RS's address space:
            |    //   +0: RS TID placeholder (i64) — RS sets its own
            |    //   +8: ramdisk base (i64) — 0 on TRISC (MMIO device)
            |    //  +16: ramdisk size (i64) — 0 on TRISC
            |    //  +24: boot info header copy
            |    var info_page: [256]byte
            |    memset(&info_page[0], 0, 256)
            |    // ramdisk base/size = 0 (TRISC disk uses MMIO)
            |    // Copy boot info header at offset 24
            |    val bi_copy_len = 8 + mod_count * 24
            |    var ci = 0
            |    while ci < bi_copy_len
            |        if ci + 24 >= 256
            |            break
            |        info_page[ci + 24] = bi[ci]
            |        ci += 1
            |    vm_copy_to(rs_ptbr, 0xBF000, &info_page[0], 256)
            |
            |    // Create RS process (suspended) and resume it
            |    val rs_pid = create_process_suspended(rs_entry_pt, 0xD0000, 0xCF000, "rs", rs_ptbr)
            |    if rs_pid < 0
            |        return -1
            |    resume_process(rs_pid)
            |
            |    timer_init(1000)
            |    first_thread_ssp()
            |""".stripMargin,
      ),
    )

  def buildDesktopTof(): TOF =
    compileAndLink(
      Map(
        "oskit/kernel/kernel"        -> kernelSysl,
        "oskit/services/services"    -> servicesSysl,
        "oskit/kernel/timer"         -> timerSysl,
        "oskit/sync/semaphore"       -> semaphoreSysl,
        "oskit/sync/mutex"           -> mutexSysl,
        "oskit/ipc/ipc"              -> ipcSysl,
        "std/mem/mem"                -> memSysl,
        "std/debug/debug"            -> debugSysl,
        "oskit/drivers/kbd/keyboard" -> kbdSysl,
        "oskit/drivers/mouse/mouse"  -> mouseSysl,
        "oskit/drivers/display/display" -> displaySysl,
        "suit/suit"                  -> suitSysl,
        "app"                        -> desktopAppSysl,
        "posix/unistd/sbrk"          -> sbrkSysl,
        "posix/stdlib/alloc"         -> posixAllocSysl,
        "posix/string/string"        -> posixStringSysl,
        "posix/ctype/ctype"          -> posixCtypeSysl,
        "oskit/hal/mem"              -> halMemSysl,
        "oskit/arch/vm"        -> archVmSysl,
        "oskit/arch/cpu"       -> archCpuSysl,
        "oskit/arch/prog_config" -> archProgConfigSysl,
        "oskit/config/config"        -> configSysl,
      ),
    )

  // --- Boot module compilation (standalone server .trb binaries) ---

  private lazy val serverProgScript: LinkerScript =
    LinkerScriptParser.parse(
      """SECTIONS
        |    code: 0xD0000
        |    rodata
        |    data
        |    bss
        |SYMBOL _heap_start = AFTER bss
        |SYMBOL _heap_end = 0x100000
        |ENTRY main
        |""".stripMargin,
    ) match
      case Right(s) => s
      case Left(e)  => throw new RuntimeException(s"server linker script: $e")

  private lazy val userSbrkSysl: String =
    scala.io.Source.fromFile("oskit/ulib/sbrk.sysl").mkString

  /** Compile a server as a standalone .trb binary suitable for loading as a
    * boot module. The server gets its own copy of syscall.asm, the IPC client
    * module (not the kernel-side handlers), and the services module.
    *
    * @param serverUnitPath
    *   compilation unit path for the server source (e.g. "oskit/drivers/disk/disk")
    * @param serverModulePath
    *   module declaration path (e.g. "oskit.drivers.disk") — used for the import in the entry wrapper
    * @param serverSource
    *   tangled sysl source for the server
    * @param entryFn
    *   the server's entry function name (e.g. "disk_server")
    * @return
    *   serialized TRB v1 binary
    */
  private def compileServerTrb(
      serverUnitPath: String,
      serverModulePath: String,
      serverSource: String,
      entryFn: String,
      extraSources: Map[String, String] = Map.empty,
  ): Array[Byte] =
    val syscallAsm =
      scala.io.Source.fromFile("oskit/ulib/syscall.asm").mkString
    val syscallTof = assemble(syscallAsm, relocatable = true)

    val wrapperSource =
      s"""import $serverModulePath.{$entryFn}
         |import oskit.services.{rs_set_tid}
         |
         |main()
         |    // RS writes its TID at 0xBF000 before resuming us
         |    val rs_tid_ptr = *i64(0xBF000)
         |    rs_set_tid(int(*rs_tid_ptr))
         |    $entryFn()
         |""".stripMargin

    val allSources = Map(
      serverUnitPath       -> serverSource,
      "oskit/services/services" -> servicesSysl,
      "oskit/ipc/ipc"      -> ipcClientSysl,
      "oskit/arch/prog_config" -> archProgConfigSysl,
      "posix/unistd/sbrk"  -> userSbrkSysl,
      "posix/stdlib/alloc"  -> posixAllocSysl,
      "posix/string/string" -> posixStringSysl,
      "posix/ctype/ctype"   -> posixCtypeSysl,
      "app"                -> wrapperSource,
    ) ++ extraSources

    val driver  = new SyslDriver
    val result  = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    val linked  = Linker.link(Seq(syscallTof, syslTof), serverProgScript, 0)
    TriscBinary.serialize(linked)

  /** Compile all boot module servers as standalone .trb binaries.
    * Returns a list of (name, bytes) pairs in boot order.
    */
  def compileBootModules(): Seq[(String, Array[Byte])] =
    Seq(
      "rs"   -> compileServerTrb("oskit/servers/rs", "oskit.servers", rsSrvSysl, "rs_main",
        extraSources = Map(
          "oskit/hal/mem" -> halMemSysl,
        )),
      "disk" -> compileServerTrb("oskit/drivers/disk/disk", "oskit.drivers.disk", diskSysl, "disk_server",
        extraSources = Map(
          "oskit/hal/mem" -> halMemSysl,
        )),
      "tty"  -> compileServerTrb("oskit/drivers/tty/tty", "oskit.drivers.tty", ttySysl, "tty_server"),
      "tfs"  -> compileServerTrb("oskit/servers/tfs", "oskit.servers", tfsSrvSysl, "tfs_server",
        extraSources = Map(
          "oskit/fs/tfs"           -> tfsSysl,
          "oskit/drivers/disk/disk" -> diskSysl,
          "oskit/hal/mem"          -> halMemSysl,
        )),
      "vfs"  -> compileServerTrb("oskit/servers/vfs", "oskit.servers", vfsSrvSysl, "vfs_server",
        extraSources = Map(
          "oskit/config/config" -> configSysl,
        )),
      "pm"   -> compileServerTrb("oskit/servers/pm", "oskit.servers", pmSrvSysl, "pm_server",
        extraSources = Map(
          "oskit/config/config"  -> configSysl,
          "oskit/loader/loader"  -> readLsysl("oskit/loader/loader.lsysl"),
          "std/alloc/alloc"      -> stdAllocSysl,
          "oskit/fs/client"      -> fsClientSysl,
          "oskit/hal/mem"        -> halMemSysl,
        )),
      "ds"   -> compileServerTrb("oskit/servers/ds", "oskit.servers", dsSrvSysl, "ds_server"),
      "init"  -> compileServerTrb("oskit/apps/init/init", "oskit.apps.init", initSysl, "init",
        extraSources = Map(
          "oskit/fs/client"      -> fsClientSysl,
          "oskit/servers/pm"     -> pmSrvSysl,
          "oskit/config/config"  -> configSysl,
          "oskit/loader/loader"  -> loaderSysl,
          "std/alloc/alloc"      -> stdAllocSysl,
          "oskit/hal/mem"        -> halMemSysl,
        )),
    )

end OskitDemoBuilder
