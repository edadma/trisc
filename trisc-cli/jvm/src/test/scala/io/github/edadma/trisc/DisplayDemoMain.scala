package io.github.edadma.trisc

/** Headless display demo runners.
  * Run: sbt "triscCliJVM/runMain io.github.edadma.trisc.DisplayDemoMain"
  * Optional arg: tof | compile | all (default: all)
  *
  * "tof" loads /tmp/os-desktop.tof (run RegenOskitDemoMain desktop first).
  * "compile" compiles examples/draw-hello/os-desktop.sysl from source.
  */
object DisplayDemoMain:
  import OSKitTestData.*

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private def runDisplay(userSources: Map[String, String], maxCycles: Int = 20000000): (CPU, String) =
    val bootTof = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel/kernel" -> kernelSysl, "oskit/services/services" -> servicesSysl,
      "oskit/kernel/timer" -> timerSysl, "oskit/sync/semaphore" -> semaphoreSysl,
      "oskit/sync/mutex" -> mutexSysl, "oskit/ipc/ipc" -> readLsysl("oskit/ipc/ipc.lsysl"),
      "std/mem/mem" -> readLsysl("std/mem/mem.lsysl"), "oskit/hal/mem" -> readLsysl("oskit/hal/mem_dma.lsysl"), "oskit/config/config" -> scala.io.Source.fromFile("oskit/config/config.sysl").mkString, "std/debug/debug" -> readLsysl("std/debug/debug.lsysl"),
      "oskit/drivers/kbd/keyboard" -> readLsysl("oskit/drivers/kbd/keyboard.lsysl"),
      "oskit/drivers/mouse/mouse" -> readLsysl("oskit/drivers/mouse/mouse.lsysl"),
      "oskit/drivers/display/display" -> readLsysl("oskit/drivers/display/display.lsysl"),
      "suit/suit" -> readLsysl("suit/suit.lsysl"),
      "posix/unistd/sbrk" -> sbrkSysl, "posix/stdlib/alloc" -> posixAllocSysl,
      "posix/string/string" -> posixStringSysl, "posix/ctype/ctype" -> posixCtypeSysl,
    ) ++ userSources
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(bootTof, syslTof), linkerScript, 0)

    runLinked(linked, maxCycles)

  private def runLinked(linked: TOF, maxCycles: Int): (CPU, String) =
    val output = new StringBuilder
    val stdout = new Stdout(Runtime.stdoutAddress, s => output ++= s)
    val intc = new InterruptController(Runtime.intcAddress)
    val timer = new Timer(Runtime.timerAddress, intc, irq = 0)
    val kbd = new KeyboardDevice(Runtime.keyboardAddress, intc, irq = 1)
    val mouse = new MouseDevice(Runtime.mouseAddress, intc, irq = 2)
    val fb = new FramebufferImage(Runtime.framebufferAddress, Runtime.framebufferMaxSize)
    val displayCtrl = new HeadlessDisplayController(Runtime.displayCtrlAddress, fb)
    var memRef: Addressable = null
    val memProxy: Addressable = new Addressable {
      val name = "memProxy"; val base = 0L; val size = 0L
      def readByte(addr: Long): Int = memRef.readByte(addr)
      def writeByte(addr: Long, data: Long): Unit = ()
      def loadByte(addr: Long, data: Long): Unit = ()
      override def readInt(addr: Long): Int = memRef.readInt(addr)
    }
    val drawEngine = new DrawEngine(
      Runtime.drawEngineAddress, memProxy, fb,
      () => displayCtrl.currentFBWidth, () => displayCtrl.currentFBHeight,
    )
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val ramdisk = new Ramdisk(Runtime.ramdiskAddress, ram, sectors = 256, sectorSize = 4096, intc, irq = 3,
      prefill = "/dev/tty0 char 0 0\n/dev/disk0 block 1 0\n/dev/null char 0 1\n")
    val blitter = new Blitter(Runtime.blitterAddress, memProxy, fb,
      () => displayCtrl.currentFBWidth, () => displayCtrl.currentFBHeight)
    val dma = new DMA(Runtime.dmaAddress, null, intc, 4)
    val mem = new Memory("Memory", ram, stdout, intc, timer, kbd, mouse, displayCtrl, fb, drawEngine, ramdisk, blitter, dma)
    dma.mem = mem
    memRef = mem
    linked.load(mem)
    val cpu = new CPU(mem, Seq(timer, intc)) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  private def runFromTof(): Unit =
    println("Loading /tmp/os-desktop.tof...")
    val tofStr = scala.io.Source.fromFile("/tmp/os-desktop.tof").mkString
    val linked = TOF.deserialize(tofStr)
    val (cpu, output) = runLinked(linked, 20000000)
    val pass = if output.contains("!") then "PASS" else "FAIL"
    println(s"$pass  Output markers: ${output.take(40)}")
    println(s"  CPU state: ${cpu.state}, cycles: ${cpu.cycles}")

  private def runFromSource(): Unit =
    println("Compiling examples/draw-hello/os-desktop.sysl...")
    val appSysl = scala.io.Source.fromFile("examples/draw-hello/os-desktop.sysl").mkString
    val (cpu, output) = runDisplay(Map("app" -> appSysl))
    val markers = Seq("S" -> "server started", "A" -> "app started", "P" -> "port found", "!" -> "completed")
    for (m, desc) <- markers do
      val status = if output.contains(m) then "PASS" else "FAIL"
      println(s"  $status  $m ($desc)")
    println(s"  CPU state: ${cpu.state}, cycles: ${cpu.cycles}")

  def main(args: Array[String]): Unit =
    val mode = args.headOption.getOrElse("all")
    if mode == "tof" || mode == "all" then runFromTof()
    if mode == "compile" || mode == "all" then runFromSource()
