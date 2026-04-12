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
  private lazy val nshSysl: String       = readLsysl("oskit/apps/nsh.lsysl")
  private lazy val initSysl: String      = readLsysl("oskit/apps/init.lsysl")
  private lazy val loginSysl: String     = readLsysl("oskit/apps/login.lsysl")
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
  private lazy val sha256Sysl: String    = readLsysl("std/crypto/sha256/sha256.lsysl")
  private lazy val hmacSysl: String      = readLsysl("std/crypto/hmac/hmac.lsysl")
  private lazy val pbkdf2Sysl: String    = readLsysl("std/crypto/pbkdf2/pbkdf2.lsysl")
  private lazy val pmSrvSysl: String     = readLsysl("oskit/servers/pm.lsysl")
  private lazy val halMemSysl: String   = readLsysl("oskit/hal/mem_dma.lsysl")
  private lazy val configSysl: String   = scala.io.Source.fromFile("oskit/config/config.sysl").mkString
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
        "oskit/drivers/disk/disk"     -> diskSysl,
        "oskit/drivers/kbd/keyboard"  -> kbdSysl,
        "oskit/drivers/tty/tty"       -> ttySysl,
        "oskit/fs/tfs"                -> tfsSysl,
        "oskit/fs/client"             -> fsClientSysl,
        "oskit/servers/tfs"           -> tfsSrvSysl,
        "oskit/servers/pm"            -> pmSrvSysl,
        "posix/unistd/sbrk"           -> sbrkSysl,
        "posix/string/string"         -> posixStringSysl,
        "posix/ctype/ctype"           -> posixCtypeSysl,
        "posix/stdlib/alloc"          -> posixAllocSysl,
        "std/debug/debug"             -> debugSysl,
        "std/mem/mem"                 -> memSysl,
        "oskit/hal/mem"               -> halMemSysl,
        "oskit/config/config"         -> configSysl,
        "std/encoding/binary/binary"  -> binarySysl,
        "std/strings/strings"         -> stringsSysl,
        "std/builder/builder"         -> builderSysl,
        "std/strconv/strconv"         -> strconvSysl,
        "std/result/result"           -> resultSysl,
        "std/errors/errors"           -> errorsSysl,
        "std/utf8/utf8"               -> utf8Sysl,
        "oskit/loader/loader"         -> loaderSysl,
        "std/crypto/sha256/sha256"    -> sha256Sysl,
        "std/crypto/hmac/hmac"        -> hmacSysl,
        "std/crypto/pbkdf2/pbkdf2"    -> pbkdf2Sysl,
        "oskit/apps/nsh/nsh"          -> nshSysl,
        "oskit/apps/init/init"        -> initSysl,
        "oskit/apps/login/login"      -> loginSysl,
        "app" ->
          """import oskit.kernel.*
import oskit.ipc.*
import oskit.apps.init.{init}
            |
            |kernel_main() -> int
            |    ipc_init()
            |    create_thread(init, kernel_stack_usp(0), kernel_stack_ssp(0), "init")
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
        "oskit/config/config"        -> configSysl,
      ),
    )

end OskitDemoBuilder
