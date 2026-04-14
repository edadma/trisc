package io.github.edadma.trisc

class OSKitBootTests extends OSKitTestHelpers {

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
    val memSource = LiterateRenderer.tangle(new LiterateParser().parse(scala.io.Source.fromFile("std/mem/mem.lsysl").mkString))
    val halMemSource = LiterateRenderer.tangle(new LiterateParser().parse(scala.io.Source.fromFile("oskit/hal/mem_dma.lsysl").mkString))
    val result = driver.compile(Map(
      "oskit/kernel/kernel" -> kernelSysl,
      "oskit/services/services" -> servicesSysl,
      "std/mem/mem" -> memSource,
      "oskit/hal/mem" -> halMemSource,
      "oskit/arch/vm" -> LiterateRenderer.tangle(new LiterateParser().parse(scala.io.Source.fromFile("oskit/arch/trisc/vm.lsysl").mkString)),
      "oskit/arch/cpu" -> LiterateRenderer.tangle(new LiterateParser().parse(scala.io.Source.fromFile("oskit/arch/trisc/cpu.lsysl").mkString)),
      "oskit/config/config" -> scala.io.Source.fromFile("oskit/config/config.sysl").mkString,
      "posix/unistd/sbrk" -> sbrkSysl,
      "posix/stdlib/alloc" -> posixAllocSysl,
      "posix/string/string" -> posixStringSysl,
      "posix/ctype/ctype" -> posixCtypeSysl,
      "tasks" -> tasksSysl,
      "main" -> mainSysl,
    ))
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)

    // Step 3: Link with boot (using linker script for _heap_start etc.)
    val linked = Linker.link(Seq(bootTof, syslTof), linkerScript, 0)
    linked.segments should not be empty
    linked.entryAddress shouldBe defined
  }

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
