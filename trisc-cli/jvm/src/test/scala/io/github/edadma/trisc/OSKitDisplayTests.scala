package io.github.edadma.trisc

/** Headless display controller for testing — no Swing dependencies */
class HeadlessDisplayController(val base: Long, fb: FramebufferImage) extends Device:
  val name = "DisplayController"
  val size = 6
  private var mode = 0
  private var widthHi = 0
  private var widthLo = 80
  private var heightHi = 0
  private var heightLo = 24
  var currentFBWidth = 640
  var currentFBHeight = 480

  def readByte(addr: Long): Int =
    (addr - base).toInt match
      case 0 => mode
      case 2 => widthHi
      case 3 => widthLo
      case 4 => heightHi
      case 5 => heightLo
      case _ => 0

  def writeByte(addr: Long, data: Long): Unit =
    val v = (data & 0xff).toInt
    (addr - base).toInt match
      case 0 => mode = v & 1
      case 1 => // commit
        if mode == 1 then
          val w = (widthHi << 8) | widthLo
          val h = (heightHi << 8) | heightLo
          fb.setResolution(w, h)
          fb.clear()
          currentFBWidth = fb.width
          currentFBHeight = fb.height
      case 2 => widthHi = v
      case 3 => widthLo = v
      case 4 => heightHi = v
      case 5 => heightLo = v
      case _ =>

  def currentMode: Int = mode

class OSKitDisplayTests extends OSKitTestHelpers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val ipcSysl: String = readLsysl("oskit/ipc/ipc.lsysl")
  private lazy val kbdSysl: String = readLsysl("oskit/drivers/kbd/keyboard.lsysl")
  private lazy val mouseSysl: String = readLsysl("oskit/drivers/mouse/mouse.lsysl")
  private lazy val displaySysl: String = readLsysl("oskit/drivers/display/display.lsysl")
  private lazy val memSysl: String = readLsysl("std/mem/mem.lsysl")
  private lazy val halMemSysl: String = readLsysl("oskit/hal/mem_dma.lsysl")
  private lazy val debugSysl: String = readLsysl("std/debug/debug.lsysl")

  def runDisplay(userSources: Map[String, String], maxCycles: Int = 10000000): (CPU, String) =
    val bootTof = assemble(bootAsm, relocatable = true)
    val allSources = Map(
      "oskit/kernel/kernel" -> kernelSysl, "oskit/services/services" -> servicesSysl, "oskit/kernel/timer" -> timerSysl,
      "oskit/sync/semaphore" -> semaphoreSysl, "oskit/sync/mutex" -> mutexSysl,
      "oskit/ipc/ipc" -> ipcSysl, "std/mem/mem" -> memSysl, "oskit/hal/mem" -> halMemSysl, "std/debug/debug" -> debugSysl, "oskit/drivers/kbd/keyboard" -> kbdSysl,
      "oskit/drivers/mouse/mouse" -> mouseSysl, "oskit/drivers/display/display" -> displaySysl,
      "posix/unistd/sbrk" -> sbrkSysl, "posix/stdlib/alloc" -> posixAllocSysl, "posix/string/string" -> posixStringSysl, "posix/ctype/ctype" -> posixCtypeSysl,
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
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val intc = new InterruptController(Runtime.intcAddress)
    val timer = new Timer(Runtime.timerAddress, intc, irq = 0)
    val kbd = new KeyboardDevice(Runtime.keyboardAddress, intc, irq = 1)
    val mouse = new MouseDevice(Runtime.mouseAddress, intc, irq = 2)
    val fb = new FramebufferImage(Runtime.framebufferAddress, Runtime.framebufferMaxSize)
    // Headless display controller — just tracks resolution, no Swing
    val displayCtrl = new HeadlessDisplayController(Runtime.displayCtrlAddress, fb)
    // memProxy for DrawEngine (needs to read RAM for text strings)
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

    val dma = new DMA(Runtime.dmaAddress, null, intc, 4)
    val mem = new Memory("Memory",
      new RAM(0, Runtime.stdoutAddress.toInt), stdout, intc, timer, kbd, mouse,
      displayCtrl, fb, drawEngine, dma)
    dma.mem = mem
    memRef = mem
    linked.load(mem)
    val testMmu = new SimpleMMU(mem); testMmu.setIdentityRange(0x7FE000L, 0xC00000L)
    dma.mmu = Some(testMmu)
    val cpu = new CPU(mem, Seq(timer, intc), mmu = Some(testMmu)) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  "Display server: client creates a window" in {
    val (cpu, output) = runDisplay(Map(
      "app" ->
        """import oskit.kernel.*
import oskit.services.*
import oskit.ipc.*
import oskit.drivers.display.*
import oskit.drivers.kbd.*
import oskit.drivers.mouse.*
          |
          |kernel_main() -> int
          |    ipc_init()
          |    keyboard_init()
          |    mouse_init()
          |    create_thread(server, 0x20000, 0x1F000, "disp")
          |    create_thread(client, 0x30000, 0x2F000, "app")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |server()
          |    display_server(640, 480)
          |
          |client()
          |    sleep(20)
          |    var title: [5]byte
          |    title[0] = byte(72)  // H
          |    title[1] = byte(101) // e
          |    title[2] = byte(108) // l
          |    title[3] = byte(108) // l
          |    title[4] = byte(111) // o
          |    val surf = ds_create_window(50, 50, 200, 150, 3, &title[0], 5)
          |    if surf > 0
          |        putc('W')
          |    // Draw into the window
          |    ds_set_color(0, 200, 255, 255)
          |    ds_clear(surf)
          |    putc('D')
          |    ds_composite()
          |    putc('!')
          |""".stripMargin
    ))
    output should include("W")
    output should include("D")
    output should include("!")
  }
}
