package io.github.edadma.trisc

class UnsignedDivTests extends TestHelpers {

  // ===== DIVU + REMU =====

  "divu basic (10 / 3 = 3)" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\nldi r2, 3\ndivu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 3
  }

  "remu 10 % 3 = 1" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\nldi r2, 3\nremu r1, r2\nhalt\n")
    cpu.r(1).read shouldBe 1
  }

  "divu exact division (10 / 5 = 2)" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\nldi r2, 5\ndivu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 2
  }

  "divu by 1 is identity" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nldi r2, 1\ndivu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 42
  }

  "divu 0 / N = 0" in {
    val cpu = runCPU(VECTORS + "ldi r2, 7\ndivu r1, r0, r2\nhalt\n")
    cpu.r(1).read shouldBe 0
  }

  "divu large unsigned value (max / 2)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 2
        |divu r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0x7FFFFFFFFFFFFFFFL
  }

  "remu large unsigned value (max % 2 = 1)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 2
        |remu r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
  }

  "divu vs div on negative-looking input" in {
    // -1 as unsigned = max; (max / 2) vs (-1 / 2)
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 2
        |divu r3, r1, r2
        |div  r4, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0x7FFFFFFFFFFFFFFFL // unsigned quotient
    cpu.r(4).read shouldBe 0L                   // signed: -1 / 2 = 0
  }

  "remu vs rem on negative-looking input" in {
    // -1 as unsigned = max; (max % 2) vs (-1 % 2)
    val cpuUnsigned = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 2
        |remu r1, r2
        |halt
        |""".stripMargin)
    cpuUnsigned.r(1).read shouldBe 1L

    val cpuSigned = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 2
        |rem r1, r2
        |halt
        |""".stripMargin)
    cpuSigned.r(1).read shouldBe -1L
  }

  "divu self / self = 1" in {
    val cpu = runCPU(VECTORS + "ldi r1, 99\ndivu r2, r1, r1\nhalt\n")
    cpu.r(2).read shouldBe 1
  }

  "divu small / large = 0" in {
    val cpu = runCPU(VECTORS + "ldi r1, 3\nldi r2, 100\ndivu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 0
  }

  "remu small % large = small" in {
    val cpu = runCPU(VECTORS + "ldi r1, 3\nldi r2, 100\nremu r1, r2\nhalt\n")
    cpu.r(1).read shouldBe 3
  }

  // ===== Consistency: a = (a / b) * b + (a % b) =====

  "divu/remu consistency: a = (a / b) * b + (a % b)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 47
        |ldi r2, 7
        |divu r3, r1, r2
        |remu r1, r2
        |mul r5, r3, r2
        |add r5, r5, r1
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 6  // 47 / 7 = 6
    cpu.r(1).read shouldBe 5  // 47 % 7 = 5
    cpu.r(5).read shouldBe 47 // 6 * 7 + 5
  }

  "divu/remu consistency with large unsigned value" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 3
        |divu r3, r1, r2
        |remu r1, r2
        |mul r5, r3, r2
        |add r5, r5, r1
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0x5555555555555555L
    cpu.r(1).read shouldBe 0
    cpu.r(5).read shouldBe -1L
  }
}
