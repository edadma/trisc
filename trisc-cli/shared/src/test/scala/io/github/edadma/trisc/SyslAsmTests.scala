package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslAsmTests extends AnyFreeSpec with Matchers {

  // ===== Parser =====

  "parse single asm statement" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    asm("nop")
        |    0
        |""".stripMargin): @unchecked
    ast.decls.length shouldBe 1
  }

  "parse asm with escape for multiline" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    asm("nop\nnop")
        |    0
        |""".stripMargin): @unchecked
    ast.decls.length shouldBe 1
  }

  // ===== Interpreter (no-op) =====

  "asm is no-op in interpreter" in {
    val buf = new StringBuilder
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    asm("nop")
        |    42
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val interp = new SyslInterpreter(s => buf ++= s)
    interp.run(typed) shouldBe 42
  }

  // ===== TRISC codegen =====

  "asm emits nop in codegen" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    asm("nop")
        |    42
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val asm = (new SyslTriscCodegen).generate(typed)
    asm should include("nop")
  }

  "asm emits multiple lines" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    asm("nop\nnop\nnop")
        |    42
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val asm = (new SyslTriscCodegen).generate(typed)
    // Should contain three nop instructions
    "nop".r.findAllIn(asm).length should be >= 3
  }

  "asm end-to-end with nop" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    asm("nop")
        |    42
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val asmCode = (new SyslTriscCodegen).generate(typed)
    val tof = assemble(asmCode, relocatable = true)
    val linked = Linker.link(Seq(tof))
    val mem = new Memory("test", new RAM(0, 0x1000))
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 100000 }
    cpu.pc = linked.entryAddress.get
    cpu.state = State.Run
    cpu.r(7).write(0x1000 - 8)
    cpu.run()
    cpu.r(1).read shouldBe 42
  }
}
