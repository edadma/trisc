package io.github.edadma.trisc

class NewExceptionTests extends TestHelpers {

  // ===== Unimplemented Opcode (vector 6) =====

  "unimplemented opcode dispatches to vector 6" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd handler
        |dd 0
        |dd 0
        |resb 88
        |reset
        |  ldi r1, 42
        |  halt
        |handler
        |  ldi r2, 99
        |  halt
        |""".stripMargin)
    // reset runs, sets r1=42, halts — no illegal instruction here
    cpu.r(1).read shouldBe 42
  }

  "illegal instruction object sets UnimplementedOpcode" in {
    val cpu = new CPU(new RAM(0, 256))
    IllegalInstruction(cpu)
    cpu.state shouldBe State.UnimplementedOpcode
  }

  // ===== Illegal Integer Divide (vector 8) =====

  "div by zero triggers illegal divide exception" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd handler
        |resb 88
        |reset
        |  ldi r1, 42
        |  div r2, r1, r0
        |  ldi r3, 99
        |  halt
        |handler
        |  ldi r3, 77
        |  halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 77
  }

  "divu by zero triggers illegal divide exception" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd handler
        |resb 88
        |reset
        |  ldi r1, 42
        |  divu r2, r1, r0
        |  ldi r3, 99
        |  halt
        |handler
        |  ldi r3, 77
        |  halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 77
  }

  "div by nonzero does not trigger exception" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 42
        |ldi r2, 7
        |div r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 6
  }

  "divide exception preserves registers when handler does not clobber" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd handler
        |resb 88
        |reset
        |  ldi r1, 2
        |  spsr r1
        |  ldi r1, 42
        |  div r2, r1, r0
        |  ldi r3, 88
        |  halt
        |handler
        |  rte
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
    cpu.r(3).read shouldBe 88
  }

  // ===== Data Access (vector 4, slot 4) =====

  "load from unmapped address triggers data access exception" in {
    val mem = new Memory("Memory", new RAM(0, 256))
    val tof = assemble(
      """dd 0xF0
        |dd reset
        |dd 0
        |dd 0
        |dd handler
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |resb 88
        |reset
        |  movi r1, 0x1000
        |  ldb r2, r1, r0
        |  ldi r3, 99
        |  halt
        |handler
        |  ldi r3, 77
        |  halt
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(3).read shouldBe 77
  }

  "store to unmapped address triggers data access exception" in {
    val mem = new Memory("Memory", new RAM(0, 256))
    val tof = assemble(
      """dd 0xF0
        |dd reset
        |dd 0
        |dd 0
        |dd handler
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |resb 88
        |reset
        |  movi r1, 0x1000
        |  stb r2, r1, r0
        |  ldi r3, 99
        |  halt
        |handler
        |  ldi r3, 77
        |  halt
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(3).read shouldBe 77
  }

  // ===== Instruction Access (vector 3, slot 3) =====

  "fetch from unmapped address triggers instruction access exception" in {
    val mem = new Memory("Memory", new RAM(0, 256))
    val tof = assemble(
      """dd 0xF0
        |dd reset
        |dd 0
        |dd handler
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |resb 88
        |reset
        |  movi r1, 0x1000
        |  jalr r0, r1
        |  halt
        |handler
        |  ldi r3, 77
        |  halt
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(3).read shouldBe 77
  }
}
