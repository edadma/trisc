package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.logger._

trait OSKitTestHelpers extends AnyFreeSpec with Matchers {
  export OSKitTestData.*

  def compileSysl(source: String): TOF =
    val driver = new SyslDriver
    val result = driver.compile(Map("main" -> source))
    val unit = result.units.head
    val codegen = new SyslTriscCodegen()
    val asm = codegen.generate(unit.typed)
    assemble(asm, relocatable = true)

  def runWithBoot(syslSource: String): (CPU, String) = runWithBoot(Map("main" -> syslSource))

  def runWithBoot(sources: Map[String, String], maxCycles: Int = 100000): (CPU, String) =
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
    val cpu = new CPU(mem) { limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  def runTOS(userSources: Map[String, String], maxCycles: Int = 2000000): (CPU, String) =
    val bootTof = assemble(bootAsm, relocatable = true)

    val allSources = Map(
      "oskit/kernel/kernel" -> kernelSysl, "oskit/services/services" -> servicesSysl, "oskit/kernel/timer" -> timerSysl, "oskit/sync/semaphore" -> semaphoreSysl,
      "oskit/sync/mutex" -> mutexSysl, "oskit/sync/condvar" -> condvarSysl, "oskit/sync/barrier" -> barrierSysl,
      "oskit/sync/rwlock" -> rwlockSysl, "oskit/sync/channel" -> channelSysl, "oskit/sync/mailbox" -> mailboxSysl,
      "oskit/sync/rmutex" -> rmutexSysl, "oskit/sync/qset" -> qsetSysl, "oskit/sync/pimutex" -> pimutexSysl,
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
    val mem = new Memory("Memory", new RAM(0, Runtime.stdoutAddress.toInt), stdout, intc, timer)
    linked.load(mem)
    val cpu = new CPU(mem, Seq(timer, intc)) { this.limit = maxCycles }
    if maxCycles <= 1000 then
      cpu.log.setLogLevel(LogLevel.TRACE)
      cpu.log.setHandler(new FileHandler("/tmp/trisc_debug.log"))
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  def runRBTest(appSource: String): (CPU, String) =
    runWithBoot(Map("oskit/kernel/rbtree" -> rbtreeSysl, "main" -> appSource))
}
