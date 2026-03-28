package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TOSTests extends AnyFreeSpec with Matchers {

  val bootAsm = scala.io.Source.fromFile("tos/boot.asm").mkString
  val kernelSysl = scala.io.Source.fromFile("tos/kernel.sysl").mkString
  val servicesSysl = scala.io.Source.fromFile("tos/services.sysl").mkString
  val tasksSysl = scala.io.Source.fromFile("examples/tos-demo/tasks.sysl").mkString
  val mainSysl = scala.io.Source.fromFile("examples/tos-demo/main.sysl").mkString

  "boot.asm assembles" in {
    val tof = assemble(bootAsm, relocatable = true)
    tof.segments should not be empty
  }

  "kernel.sysl parses" in {
    val parser = new SyslParser
    val result = parser.parseProgram(kernelSysl)
    result shouldBe a[Right[?, ?]]
  }

  "tasks.sysl parses" in {
    val parser = new SyslParser
    val result = parser.parseProgram(tasksSysl)
    result shouldBe a[Right[?, ?]]
  }

  "TOS compiles and links" in {
    // Step 1: Assemble boot.asm
    val bootTof = assemble(bootAsm, relocatable = true)

    // Step 2: Compile Sysl files together (kernel + demo)
    val driver = new SyslDriver
    val result = driver.compile(Map(
      "kernel" -> kernelSysl,
      "services" -> servicesSysl,
      "tasks" -> tasksSysl,
      "main" -> mainSysl,
    ))
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)

    // Step 3: Link with boot
    val linked = Linker.link(Seq(bootTof, syslTof))
    linked.segments should not be empty
    linked.entryAddress shouldBe defined
  }

  // ===== End-to-end: boot stub + Sysl program =====

  // Minimal boot stub for end-to-end tests.
  // Uses test memory layout (0x1000 total, stdout at 0xFF8).
  val minimalBoot: String =
    """STDOUT = 0xFF8
      |
      |segment vectors
      |
      |  dl 0xFF0
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

  def runWithBoot(syslSource: String): (CPU, String) =
    val bootTof = assemble(minimalBoot, relocatable = true)
    val progTof = compileSysl(syslSource)
    val linked = Linker.link(Seq(bootTof, progTof))

    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = 0xFF8
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val mem = new Memory("Memory", new RAM(0, 0xFF8), stdout)
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  "boot + sysl hello: prints Hello and halts" in {
    val (cpu, output) = runWithBoot(
      """main() -> int
        |    putchar(72)
        |    putchar(101)
        |    putchar(108)
        |    putchar(108)
        |    putchar(111)
        |    putchar(10)
        |    0
        |""".stripMargin)

    output shouldBe "Hello\n"
    cpu.state shouldBe State.Halt
  }

  "boot + sysl program: returns value in r1" in {
    val (cpu, _) = runWithBoot(
      """main() -> int
        |    42
        |""".stripMargin)

    cpu.state shouldBe State.Halt
    cpu.r(1).read shouldBe 42
  }

  "boot + sysl program: computation and output" in {
    val (cpu, output) = runWithBoot(
      """main() -> int
        |    var i = 0
        |    while i < 5
        |        putchar(65 + i)
        |        i += 1
        |    putchar(10)
        |    0
        |""".stripMargin)

    output shouldBe "ABCDE\n"
    cpu.state shouldBe State.Halt
  }

  // ===== TOS integration tests =====

  /** Compile TOS kernel + user tasks, link with boot.asm, run on CPU with timer. */
  def runTOS(userSources: Map[String, String], maxCycles: Int = 500000000): (CPU, String) =
    // Assemble boot.asm
    val bootTof = assemble(bootAsm, relocatable = true)

    // Compile kernel + user sources together
    val allSources = Map("kernel" -> kernelSysl, "services" -> servicesSysl) ++ userSources
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
    val cpu = new CPU(mem, Seq(timer)) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  "TOS: putc syscall prints character" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |task()
          |    putc(72)
          |    putc(105)
          |    putc(10)
          |""".stripMargin
    ), maxCycles = 100000)

    output should startWith("Hi\n")
  }

  "TOS: thread exit works cleanly" in {
    val (cpu, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(task1, 0x6000, 0x5000, "t1")
          |    create_thread(task2, 0x8000, 0x7000, "t2")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |task1()
          |    putc(65)
          |
          |task2()
          |    putc(66)
          |""".stripMargin
    ), maxCycles = 100000)

    // Both tasks print and exit — output should contain both A and B
    output should include("A")
    output should include("B")
  }

  "TOS: sleep syscall delays output" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |
          |kernel_main() -> int
          |    create_thread(task, 0x6000, 0x5000, "task")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |
          |task()
          |    putc(65)
          |    sleep(5)
          |    putc(66)
          |    sleep(5)
          |    putc(67)
          |""".stripMargin
    ))

    output should startWith("ABC")
  }

  "TOS: two tasks interleave with sleep" in {
    val (_, output) = runTOS(Map(
      "tasks" ->
        """import "services"
          |
          |task_a()
          |    var i = 0
          |    while i < 3
          |        putc(65)
          |        sleep(10)
          |        i += 1
          |
          |task_b()
          |    var i = 0
          |    while i < 3
          |        putc(66)
          |        sleep(20)
          |        i += 1
          |""".stripMargin,
      "app" ->
        """import "kernel"
          |import "tasks"
          |
          |kernel_main() -> int
          |    create_thread(task_a, 0x6000, 0x5000, "a")
          |    create_thread(task_b, 0x8000, 0x7000, "b")
          |    val period: *i32 = 0x100020
          |    *period = 10
          |    val control: *i8 = 0x100024
          |    *control = 1
          |    first_thread_ssp()
          |""".stripMargin
    ))

    // A prints at ticks 0,10,20 — B prints at ticks 0,20,40
    // Expected pattern: AB A AB A B (roughly 2:1)
    output.count(_ == 'A') shouldBe 3
    output.count(_ == 'B') shouldBe 3
  }
}
