package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

trait TOSTestHelpers extends AnyFreeSpec with Matchers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  val bootAsm = scala.io.Source.fromFile("tos/boot.asm").mkString
  val kernelSysl = readLsysl("tos/kernel.lsysl")
  val servicesSysl = readLsysl("tos/services.lsysl")
  val semaphoreSysl = readLsysl("tos/semaphore.lsysl")
  val mutexSysl = readLsysl("tos/mutex.lsysl")
  val condvarSysl = readLsysl("tos/condvar.lsysl")
  val barrierSysl = readLsysl("tos/barrier.lsysl")
  val rwlockSysl = readLsysl("tos/rwlock.lsysl")
  val channelSysl = readLsysl("tos/channel.lsysl")
  val mailboxSysl = readLsysl("tos/mailbox.lsysl")
  val rbtreeSysl = readLsysl("tos/rbtree.lsysl")
  val tasksSysl = readLsysl("examples/tos-demo/tasks.lsysl")
  val mainSysl = readLsysl("examples/tos-demo/main.lsysl")

  // Minimal boot stub for end-to-end tests.
  // Uses test memory layout (64KB RAM, stdout at 0x10000).
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

  /** Compile TOS kernel + user tasks, link with boot.asm, run on CPU with timer. */
  def runTOS(userSources: Map[String, String], maxCycles: Int = 500000000): (CPU, String) =
    // Assemble boot.asm
    val bootTof = assemble(bootAsm, relocatable = true)

    // Compile kernel + user sources together
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

    // Link all
    val linked = Linker.link(Seq(bootTof, syslTof))

    // Set up CPU with stdout + timer
    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = 0x100000
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val timer = new Timer(0x100020L)
    val mem = new Memory("Memory", new RAM(0, 0x100000), stdout, timer)
    linked.load(mem)
    val cpu = new CPU(mem, timer) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  def runRBTest(appSource: String): (CPU, String) =
    runWithBoot(Map("rbtree" -> rbtreeSysl, "main" -> appSource))
}
