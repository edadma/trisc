package io.github.edadma.trisc

/** Compile the SLIX shell demo and write a TOF file for the GUI emulator.
  * Run with: sbt "triscCliJVM/testOnly *BuildSLIXShell*"
  * Then: sbt "triscCliJVM/run run --gui /tmp/slix-shell.tof"
  */
class BuildSLIXShell extends OSKitTestHelpers {

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
  private lazy val stringSysl: String = readLsysl("oskit/lib/string.lsysl")
  private lazy val shSysl: String     = readLsysl("oskit/apps/sh.lsysl")
  private lazy val initSysl: String   = readLsysl("oskit/apps/init.lsysl")
  private lazy val appSysl: String    = readLsysl("examples/slix-shell/main.lsysl")

  "build SLIX shell TOF" in {
    val bootTof = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel"     -> kernelSysl,
      "oskit/services"   -> servicesSysl,
      "oskit/timer"      -> timerSysl,
      "oskit/semaphore"  -> semaphoreSysl,
      "oskit/mutex"      -> mutexSysl,
      "oskit/ipc"        -> ipcSysl,
      "oskit/disk"       -> diskSysl,
      "oskit/kbd"        -> kbdSysl,
      "oskit/tty"        -> ttySysl,
      "oskit/fs/tfs"     -> tfsSysl,
      "oskit/tfs_srv"    -> tfsSrvSysl,
      "oskit/lib/string" -> stringSysl,
      "oskit/sh"         -> shSysl,
      "oskit/init"       -> initSysl,
      "oskit/app"        -> appSysl,
    )
    val driver  = new SyslDriver
    val result  = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs =
      for unit <- result.units yield
        val asm = codegen.generate(unit.typed)
        assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    val linked  = Linker.link(Seq(bootTof, syslTof), linkerScript, 0)

    val tofStr = linked.serialize
    val pw = new java.io.PrintWriter("/tmp/slix-shell.tof")
    pw.print(tofStr)
    pw.close()
    println(s"Wrote /tmp/slix-shell.tof (${tofStr.length} bytes)")

    linked.tofType shouldBe TOFType.Executable
  }
}
