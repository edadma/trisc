package io.github.edadma.trisc

class DisassemblerTests extends TestHelpers {

  "disassemble simple instruction" in {
    val tof = assemble("ldi r1, 42\nhalt\n")
    val mem = new Memory("test", new RAM(0, 0x100))
    tof.load(mem)
    val dis = new Disassembler(mem)
    val (line, size) = dis.disassembleAt(0)
    size shouldBe 2
    line should include("ldi")
    line should include("r1")
    line should include("42")
  }

  "disassemble range" in {
    val tof = assemble("ldi r1, 10\nldi r2, 20\nadd r3, r1, r2\nhalt\n")
    val mem = new Memory("test", new RAM(0, 0x100))
    tof.load(mem)
    val dis = new Disassembler(mem)
    val output = dis.disassembleRange(0, 8)
    output should include("ldi")
    output should include("add")
  }

  "disassemble with symbols from TOF" in {
    val tof = assemble(
      """global main, func
        |main
        |  ldi r1, 42
        |  halt
        |""".stripMargin)
    val mem = new Memory("test", new RAM(0, 0x100))
    tof.load(mem)
    val dis = Disassembler.fromTOF(mem, tof)
    val (line, _) = dis.disassembleAt(0)
    line should include("main:")
  }

  "disassemble resolves branch target to symbol" in {
    val tof = assemble(
      """entry main
        |global main, func
        |global helper, func
        |helper
        |  ldi r1, 99
        |  jalr r0, r6
        |main
        |  movi r4, helper
        |  jalr r6, r4
        |  halt
        |""".stripMargin, relocatable = true)
    val linked = Linker.link(Seq(tof))
    val mem = new Memory("test", new RAM(0, 0x100))
    linked.load(mem)
    val dis = Disassembler.fromTOF(mem, linked)
    // The helper label should appear in disassembly
    val output = dis.disassembleRange(0, 12)
    output should include("helper:")
    output should include("main:")
  }

  "disassemble function stops at halt" in {
    val tof = assemble(
      """global main, func
        |main
        |  ldi r1, 42
        |  halt
        |""".stripMargin)
    val mem = new Memory("test", new RAM(0, 0x100))
    tof.load(mem)
    val dis = Disassembler.fromTOF(mem, tof)
    val result = dis.disassembleFunction("main")
    result shouldBe defined
    result.get should include("ldi")
    result.get should include("halt")
  }

  "disassemble function stops at return" in {
    val tof = assemble(
      """global helper, func
        |helper
        |  ldi r1, 10
        |  jalr r0, r6
        |""".stripMargin)
    val mem = new Memory("test", new RAM(0, 0x100))
    tof.load(mem)
    val dis = Disassembler.fromTOF(mem, tof)
    val result = dis.disassembleFunction("helper")
    result shouldBe defined
    result.get should include("ldi")
    result.get should include("jalr")
  }

  "disassemble function returns None for unknown name" in {
    val tof = assemble("halt\n")
    val mem = new Memory("test", new RAM(0, 0x100))
    tof.load(mem)
    val dis = Disassembler.fromTOF(mem, tof)
    dis.disassembleFunction("nonexistent") shouldBe None
  }

  "disassemble end-to-end from Sysl" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    x = 5
        |    x + 1
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val asm = (new SyslTriscCodegen).generate(typed)
    val tof = assemble(asm, relocatable = true)
    val linked = Linker.link(Seq(tof))
    val mem = new Memory("test", new RAM(0, 0x1000))
    linked.load(mem)
    val dis = Disassembler.fromTOF(mem, linked)
    val result = dis.disassembleFunction("main")
    result shouldBe defined
    result.get should include("main:")
  }
}
