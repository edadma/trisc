package io.github.edadma.trisc

/** Compile the OS desktop demo and write a TOF file for the GUI emulator.
  * Run with: sbt "triscCliJVM/testOnly *BuildOSDesktop*"
  * Then: sbt "triscCliJVM/run run --gui /tmp/os-desktop.tof"
  */
class BuildOSDesktop extends OSKitTestHelpers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val ipcSysl: String = readLsysl("oskit/ipc/ipc.lsysl")
  private lazy val kbdSysl: String = readLsysl("oskit/drivers/kbd/keyboard.lsysl")
  private lazy val mouseSysl: String = readLsysl("oskit/drivers/mouse/mouse.lsysl")
  private lazy val displaySysl: String = readLsysl("oskit/drivers/display/display.lsysl")
  private lazy val memSysl: String = readLsysl("std/mem/mem.lsysl")
  private lazy val debugSysl: String = readLsysl("std/debug/debug.lsysl")
  private lazy val suitSysl: String = readLsysl("suit/suit.lsysl")
  private lazy val appSysl: String = scala.io.Source.fromFile("examples/draw-hello/os-desktop.sysl").mkString

  "build OS desktop TOF" in {
    val bootTof = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel/kernel" -> kernelSysl, "oskit/services/services" -> servicesSysl, "oskit/kernel/timer" -> timerSysl,
      "oskit/sync/semaphore" -> semaphoreSysl, "oskit/sync/mutex" -> mutexSysl,
      "oskit/ipc/ipc" -> ipcSysl, "std/mem/mem" -> memSysl, "std/debug/debug" -> debugSysl, "oskit/drivers/kbd/keyboard" -> kbdSysl,
      "oskit/drivers/mouse/mouse" -> mouseSysl, "oskit/drivers/display/display" -> displaySysl,
      "suit/suit" -> suitSysl, "app" -> appSysl,
      "posix/unistd/sbrk" -> sbrkSysl, "posix/stdlib/alloc" -> posixAllocSysl, "posix/string/string" -> posixStringSysl, "posix/ctype/ctype" -> posixCtypeSysl,
    )
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(bootTof, syslTof), linkerScript, 0)

    // Write the TOF to disk
    val tofStr = linked.serialize
    val pw = new java.io.PrintWriter("/tmp/os-desktop.tof")
    pw.print(tofStr)
    pw.close()
    println(s"Wrote /tmp/os-desktop.tof (${tofStr.length} bytes)")

    // Sanity check: it should be a valid executable
    linked.tofType shouldBe TOFType.Executable
  }
}
