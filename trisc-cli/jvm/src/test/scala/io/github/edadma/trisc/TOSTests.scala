package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TOSTests extends AnyFreeSpec with Matchers {

  val bootAsm = scala.io.Source.fromFile("boot.asm").mkString
  val kernelSysl = scala.io.Source.fromFile("tos/kernel.sysl").mkString
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
}
