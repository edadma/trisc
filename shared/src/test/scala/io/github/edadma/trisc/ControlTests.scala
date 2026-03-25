package io.github.edadma.trisc

class ControlTests extends TestHelpers {

  // ===== HALT =====

  "halt stops execution" in {
    val cpu = runCPU(VECTORS + "ldi r1, 1\nhalt\nldi r1, 2\n")
    cpu.r(1).read shouldBe 1
    cpu.state shouldBe State.Halt
  }

  // ===== NOP =====

  "nop does nothing" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nnop\nnop\nnop\nhalt\n")
    cpu.r(1).read shouldBe 42
  }

  // ===== MOV =====

  "mov copies register" in {
    val cpu = runCPU(VECTORS + "ldi r1, 77\nmov r2, r1\nhalt\n")
    cpu.r(2).read shouldBe 77
  }

  "mov does not affect source" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nmov r2, r1\nldi r1, 0\nhalt\n")
    cpu.r(1).read shouldBe 0
    cpu.r(2).read shouldBe 42
  }

  // ===== r0 =====

  "r0 is hardwired to zero" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nadd r1, r1, r0\nhalt\n")
    cpu.r(0).read shouldBe 0
    cpu.r(1).read shouldBe 42
  }

  "r0 ignores writes via add" in {
    val cpu = runCPU(VECTORS + "ldi r1, 99\nadd r0, r1, r1\nhalt\n")
    cpu.r(0).read shouldBe 0
  }

  // ===== JALR =====

  "jalr saves return address and jumps" in {
    val cpu = runCPU(VECTORS +
      """movi r1, target
        |jalr r7, r1
        |halt
        |target
        |  ldi r2, 42
        |  halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 42
    cpu.r(7).read should be > 0L
  }

  "jalr subroutine call and return" in {
    val output = runProgram(
      """STDOUT = 0xFF8
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r2, 'A'
        |movi r1, printChar
        |jalr r7, r1
        |ldi r2, 'B'
        |movi r1, printChar
        |jalr r7, r1
        |halt
        |printChar
        |  movi r4, STDOUT
        |  stb r2, r4, r0
        |  jalr r0, r7
        |""".stripMargin)
    output shouldBe "AB"
  }

  "jalr r0 as link discards return address" in {
    val cpu = runCPU(VECTORS +
      """movi r1, target
        |jalr r0, r1
        |halt
        |target
        |  ldi r2, 42
        |  halt
        |""".stripMargin)
    cpu.r(0).read shouldBe 0
    cpu.r(2).read shouldBe 42
  }

  // ===== SPSR / GPSR =====

  "spsr sets processor status register" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x0F\nspsr r1\ngpsr r2\nhalt\n")
    cpu.r(2).read shouldBe 0x0F
  }

  "gpsr reads processor status register" in {
    val cpu = runCPU(VECTORS + "ldi r1, 5\nspsr r1\ngpsr r2\nhalt\n")
    cpu.r(2).read shouldBe 5
  }

  "spsr then gpsr round-trips" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x0A\nspsr r1\ngpsr r3\nhalt\n")
    cpu.r(3).read shouldBe 0x0A
  }
}
