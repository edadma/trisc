package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

trait TFSTestHelpers extends AnyFreeSpec with Matchers {

  private lazy val tfsSource: String =
    val raw = scala.io.Source.fromFile("tos/tfs/tfs.lsysl").mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private val tfsBoot: String =
    s"""STDOUT = 0x100000
       |
       |segment vectors
       |
       |  dl 0xFFF8
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
       |global default_isr, func
       |
       |default_isr
       |  halt
       |  align 8
       |""".stripMargin

  def runTFS(
      mainSource: String,
      prefill: String = "",
      maxCycles: Int = 500000,
  ): (CPU, String) =
    runTFS(Map("main" -> mainSource), prefill, maxCycles)

  def runTFS(
      sources: Map[String, String],
      prefill: String,
      maxCycles: Int,
  ): (CPU, String) =
    val bootTof = assemble(tfsBoot, relocatable = true)
    val allSources = sources + ("tfs" -> tfsSource)
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val progTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(bootTof, progTof))

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
    intc.addTickable(() => timer.tick())
    val ramSize = Runtime.stdoutAddress.toInt
    val ram = new RAM(0, ramSize)
    val ramdisk = new Ramdisk(
      Runtime.ramdiskAddress,
      ram,
      sectors = 2048,
      sectorSize = 512,
      intc,
      irq = 3,
      prefill = prefill,
    )
    val mem = new Memory("Memory", ram, stdout, intc, timer, ramdisk)
    linked.load(mem)
    val cpu = new CPU(mem, intc) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  /** Generate SYSL code to declare a null-terminated byte array from a string.
    * Usage: `syslBytes("path", "/dev/tty0")` produces:
    *   var path: [11]i8
    *   path[0] = 47
    *   path[1] = 100
    *   ...
    *   path[10] = 0
    */
  def syslBytes(name: String, s: String): String =
    val bytes = s.getBytes("UTF-8") :+ 0.toByte
    val decl = s"    var $name: [${bytes.length}]i8"
    val assigns = bytes.zipWithIndex.map { (b, i) => s"    $name[$i] = ${b & 0xff}" }.mkString("\n")
    s"$decl\n$assigns"
}
