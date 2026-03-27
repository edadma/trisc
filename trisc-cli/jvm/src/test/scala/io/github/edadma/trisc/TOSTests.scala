package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TOSTests extends AnyFreeSpec with Matchers {

  val bootAsm = scala.io.Source.fromFile("tos/boot.asm").mkString
  val kernelSysl = scala.io.Source.fromFile("tos/kernel.sysl").mkString
  val tasksSysl = scala.io.Source.fromFile("tos/tasks.sysl").mkString

  "boot.asm assembles" in {
    val tof = assemble(bootAsm, relocatable = true)
    tof.segments should not be empty
  }

  "kernel.sysl parses" in {
    val parser = new SyslParser
    val result = parser.parseProgram(kernelSysl)
    result shouldBe a[Right[_, _]]
  }

  "tasks.sysl parses" in {
    val parser = new SyslParser
    val result = parser.parseProgram(tasksSysl)
    result shouldBe a[Right[_, _]]
  }

  "kernel.sysl analyzes" in {
    val parser = new SyslParser
    // Parse both Sysl files together (same module)
    val combined = kernelSysl + "\n" + tasksSysl
    val Right(ast) = parser.parseProgram(combined): @unchecked
    val analyzer = new SyslAnalyzer
    // Register assembly symbols as external functions
    // (these are defined in boot.asm, called from kernel.sysl)
    val bootMeta = ModuleMeta.fromSmeta(
      """SMETA v1
        |FUNC start_first_thread 1 int void
        |""".stripMargin)
    analyzer.registerImport(bootMeta)
    val typed = analyzer.analyze(ast)
    typed.decls should not be empty
  }

  "TOS compiles and links" in {
    // Step 1: Assemble boot.asm
    val bootTof = assemble(bootAsm, relocatable = true)

    // Step 2: Compile Sysl files together
    val parser = new SyslParser
    val combined = kernelSysl + "\n" + tasksSysl
    val Right(ast) = parser.parseProgram(combined): @unchecked
    val analyzer = new SyslAnalyzer
    // Register start_first_thread as external
    val bootMeta = ModuleMeta.fromSmeta(
      """SMETA v1
        |FUNC start_first_thread 1 int void
        |""".stripMargin)
    analyzer.registerImport(bootMeta)
    val typed = analyzer.analyze(ast)
    val asmCode = (new SyslTriscCodegen).generate(typed)
    val syslTof = assemble(asmCode, relocatable = true)

    // Step 3: Link
    val linked = Linker.link(Seq(bootTof, syslTof))
    linked.segments should not be empty
    linked.entryAddress shouldBe defined
  }
}
