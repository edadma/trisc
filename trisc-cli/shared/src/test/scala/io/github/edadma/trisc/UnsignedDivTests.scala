package io.github.edadma.trisc

class UnsignedDivTests extends TestHelpers {

  // ===== DIVU (quotient + remainder in register pair) =====

  "divu basic (10 / 3 = 3 rem 1)" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\nldi r2, 3\ndivu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 3
    cpu.r(4).read shouldBe 1
  }

  "divu exact division (10 / 5 = 2 rem 0)" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\nldi r2, 5\ndivu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 2
    cpu.r(4).read shouldBe 0
  }

  "divu by 1 is identity with 0 remainder" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nldi r2, 1\ndivu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 42
    cpu.r(4).read shouldBe 0
  }

  "divu 0 / N = 0 rem 0" in {
    val cpu = runCPU(VECTORS + "ldi r2, 7\ndivu r1, r0, r2\nhalt\n")
    cpu.r(1).read shouldBe 0
    cpu.r(2).read shouldBe 0
  }

  "divu large unsigned value (-1 as unsigned / 2)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 2
        |divu r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0x7FFFFFFFFFFFFFFFL
    cpu.r(4).read shouldBe 1 // 0xFFFFFFFFFFFFFFFF % 2 = 1
  }

  "divu where signed div would give different result" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 2
        |divu r3, r1, r2
        |div r5, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0x7FFFFFFFFFFFFFFFL // unsigned quotient
    cpu.r(4).read shouldBe 1                    // unsigned remainder
    cpu.r(5).read shouldBe 0L                   // signed: -1 / 2 = 0
    cpu.r(6).read shouldBe -1L                  // signed: -1 % 2 = -1
  }

  "divu self / self = 1 rem 0" in {
    val cpu = runCPU(VECTORS + "ldi r1, 99\ndivu r2, r1, r1\nhalt\n")
    cpu.r(2).read shouldBe 1
    cpu.r(3).read shouldBe 0
  }

  "divu large unsigned self / self = 1 rem 0" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |divu r2, r1, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 1
    cpu.r(3).read shouldBe 0
  }

  "divu small / large = 0 rem small" in {
    val cpu = runCPU(VECTORS + "ldi r1, 3\nldi r2, 100\ndivu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 0
    cpu.r(4).read shouldBe 3
  }

  // ===== DIVU consistency: a = (a / b) * b + (a % b) =====

  "divu consistency: a = (a / b) * b + (a % b)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 47
        |ldi r2, 7
        |divu r3, r1, r2
        |; r3 = quotient, r4 = remainder
        |mul r5, r3, r2
        |add r5, r5, r4
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 6  // 47 / 7 = 6
    cpu.r(4).read shouldBe 5  // 47 % 7 = 5
    cpu.r(5).read shouldBe 47 // 6 * 7 + 5 = 47
  }

  "divu consistency with large unsigned value" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 3
        |divu r3, r1, r2
        |; r3 = quotient, r4 = remainder
        |mul r5, r3, r2
        |add r5, r5, r4
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0x5555555555555555L
    cpu.r(4).read shouldBe 0
    cpu.r(5).read shouldBe -1L
  }
}
