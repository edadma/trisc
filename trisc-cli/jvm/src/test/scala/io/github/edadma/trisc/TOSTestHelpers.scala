package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

object TOSTestData {
  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  lazy val bootAsm: String = scala.io.Source.fromFile("tos/boot.asm").mkString
  lazy val kernelSysl: String = readLsysl("tos/kernel.lsysl")
  lazy val servicesSysl: String = readLsysl("tos/services.lsysl")
  lazy val semaphoreSysl: String = readLsysl("tos/semaphore.lsysl")
  lazy val mutexSysl: String = readLsysl("tos/mutex.lsysl")
  lazy val condvarSysl: String = readLsysl("tos/condvar.lsysl")
  lazy val barrierSysl: String = readLsysl("tos/barrier.lsysl")
  lazy val rwlockSysl: String = readLsysl("tos/rwlock.lsysl")
  lazy val channelSysl: String = readLsysl("tos/channel.lsysl")
  lazy val mailboxSysl: String = readLsysl("tos/mailbox.lsysl")
  lazy val rbtreeSysl: String = readLsysl("tos/rbtree.lsysl")
  lazy val tasksSysl: String = readLsysl("examples/tos-demo/tasks.lsysl")
  lazy val mainSysl: String = readLsysl("examples/tos-demo/main.lsysl")

  val minimalBoot: String =
    """STDOUT = 0x10000
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
      |""".stripMargin
}

trait TOSTestHelpers extends AnyFreeSpec with Matchers {
  export TOSTestData.*

  def compileSysl(source: String): TOF =
    val driver = new SyslDriver
    val result = driver.compile(Map("main" -> source))
    val unit = result.units.head
    val codegen = new SyslTriscCodegen()
    val asm = codegen.generate(unit.typed)
    assemble(asm, relocatable = true)

  def runWithBoot(syslSource: String): (CPU, String) = runWithBoot(Map("main" -> syslSource))

  def runWithBoot(sources: Map[String, String]): (CPU, String) =
    val bootTof = assemble(minimalBoot, relocatable = true)
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val progTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(bootTof, progTof))

    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = 0x10000
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val mem = new Memory("Memory", new RAM(0, 0x10000), stdout)
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 100000 }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  def runTOS(userSources: Map[String, String], maxCycles: Int = 50000000): (CPU, String) =
    val bootTof = assemble(bootAsm, relocatable = true)

    val allSources = Map(
      "kernel" -> kernelSysl, "services" -> servicesSysl, "semaphore" -> semaphoreSysl,
      "mutex" -> mutexSysl, "condvar" -> condvarSysl, "barrier" -> barrierSysl,
      "rwlock" -> rwlockSysl, "channel" -> channelSysl, "mailbox" -> mailboxSysl,
    ) ++ userSources
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)

    val linked = Linker.link(Seq(bootTof, syslTof))

    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = 0x100000
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val intc = new InterruptController(0x100026L)
    val timer = new Timer(0x100020L, intc, irq = 0)
    intc.addTickable(() => timer.tick())
    val mem = new Memory("Memory", new RAM(0, 0x100000), stdout, intc, timer)
    linked.load(mem)
    val cpu = new CPU(mem, intc) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  def runRBTest(appSource: String): (CPU, String) =
    runWithBoot(Map("rbtree" -> rbtreeSysl, "main" -> appSource))
}
