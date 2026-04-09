package io.github.edadma.trisc

/** Compile the OS with login → nsh and write a TOF file.
  * Build: sbt "triscCliJVM/testOnly *BuildOSLogin*"
  * Run:   sbt "triscCliJVM/run run /tmp/os-login.tof"
  */
class BuildOSLogin extends OSKitTestHelpers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val ipcSysl: String    = readLsysl("oskit/ipc/ipc.lsysl")
  private lazy val diskSysl: String   = readLsysl("oskit/drivers/disk/disk.lsysl")
  private lazy val kbdSysl: String    = readLsysl("oskit/drivers/kbd/keyboard.lsysl")
  private lazy val ttySysl: String    = readLsysl("oskit/drivers/tty/tty.lsysl")
  private lazy val tfsSysl: String    = readLsysl("oskit/fs/tfs.lsysl")
  private lazy val tfsSrvSysl: String = readLsysl("oskit/servers/tfs.lsysl")
  private lazy val nshSysl: String    = readLsysl("oskit/apps/nsh.lsysl")
  private lazy val initSysl: String   = readLsysl("oskit/apps/init.lsysl")
  private lazy val loginSysl: String  = readLsysl("oskit/apps/login.lsysl")
  private lazy val debugSysl: String   = readLsysl("std/debug/debug.lsysl")
  private lazy val memSysl: String     = readLsysl("std/mem/mem.lsysl")
  private lazy val binarySysl: String  = readLsysl("std/encoding/binary/binary.lsysl")
  private lazy val sha256Sysl: String  = readLsysl("std/crypto/sha256/sha256.lsysl")
  private lazy val hmacSysl: String   = readLsysl("std/crypto/hmac/hmac.lsysl")
  private lazy val pbkdf2Sysl: String = readLsysl("std/crypto/pbkdf2/pbkdf2.lsysl")

  "build OS login TOF" in {
    val bootTof = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel/kernel"         -> kernelSysl,
      "oskit/services/services"     -> servicesSysl,
      "oskit/kernel/timer"          -> timerSysl,
      "oskit/sync/semaphore"        -> semaphoreSysl,
      "oskit/sync/mutex"            -> mutexSysl,
      "oskit/ipc/ipc"               -> ipcSysl,
      "oskit/drivers/disk/disk"     -> diskSysl,
      "oskit/drivers/kbd/keyboard"  -> kbdSysl,
      "oskit/drivers/tty/tty"       -> ttySysl,
      "oskit/fs/tfs"                -> tfsSysl,
      "oskit/servers/tfs"           -> tfsSrvSysl,
      "posix/unistd/sbrk"          -> sbrkSysl,
      "posix/string/string"        -> posixStringSysl,
      "posix/ctype/ctype"          -> posixCtypeSysl,
      "posix/stdlib/alloc"         -> posixAllocSysl,
      "std/debug/debug"             -> debugSysl,
      "std/mem/mem"                 -> memSysl,
      "std/encoding/binary/binary" -> binarySysl,
      "std/crypto/sha256/sha256"   -> sha256Sysl,
      "std/crypto/hmac/hmac"       -> hmacSysl,
      "std/crypto/pbkdf2/pbkdf2"   -> pbkdf2Sysl,
      "oskit/apps/nsh/nsh"           -> nshSysl,
      "oskit/apps/init/init"        -> initSysl,
      "oskit/apps/login/login"      -> loginSysl,
      "app" ->
        """import oskit.kernel.*
import oskit.ipc.*
import oskit.apps.init.{init}
          |
          |kernel_main() -> int
          |    ipc_init()
          |    create_thread(init, 0xC0000, 0xC0000, "init")
          |    timer_init(1000)
          |    first_thread_ssp()
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(bootTof, syslTof), linkerScript, 0)

    val tofStr = linked.serialize
    val pw = new java.io.PrintWriter("/tmp/os-login.tof")
    pw.print(tofStr)
    pw.close()
    println(s"Wrote /tmp/os-login.tof (${tofStr.length} bytes)")

    linked.tofType shouldBe TOFType.Executable
  }
}
