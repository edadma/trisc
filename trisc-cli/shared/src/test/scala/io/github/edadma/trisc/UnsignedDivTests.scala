package io.github.edadma.trisc

class UnsignedDivTests extends TestHelpers {

  // ===== DIVU =====

  "divu basic (10 / 3 = 3)" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\nldi r2, 3\ndivu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 3
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

  "divu large unsigned value (-1 as unsigned / 2 = 0x7FFFFFFFFFFFFFFF)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 2
        |divu r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0x7FFFFFFFFFFFFFFFL
  }

  "divu where signed div would give different result (-1 signed / 2 = 0, but unsigned max / 2 = large)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 2
        |divu r3, r1, r2
        |div r4, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0x7FFFFFFFFFFFFFFFL // unsigned
    cpu.r(4).read shouldBe 0L                   // signed: -1 / 2 = 0
  }

  "divu self / self = 1" in {
    val cpu = runCPU(VECTORS + "ldi r1, 99\ndivu r2, r1, r1\nhalt\n")
    cpu.r(2).read shouldBe 1
  }

  "divu large unsigned self / self = 1" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |divu r2, r1, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 1
  }

  "divu small / large = 0" in {
    val cpu = runCPU(VECTORS + "ldi r1, 3\nldi r2, 100\ndivu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 0
  }

  // ===== REMU =====

  "remu basic (10 % 3 = 1)" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\nldi r2, 3\nremu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 1
  }

  "remu exact division gives 0 (10 % 5 = 0)" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\nldi r2, 5\nremu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 0
  }

  "remu by 1 = 0" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nldi r2, 1\nremu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 0
  }

  "remu 0 % N = 0" in {
    val cpu = runCPU(VECTORS + "ldi r2, 7\nremu r1, r0, r2\nhalt\n")
    cpu.r(1).read shouldBe 0
  }

  "remu large unsigned value (-1 as unsigned % 2 = 1)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 2
        |remu r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1
  }

  "remu where signed rem would give different result" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 2
        |remu r3, r1, r2
        |rem r4, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1   // unsigned: 0xFFFFFFFFFFFFFFFF % 2 = 1
    cpu.r(4).read shouldBe -1  // signed: -1 % 2 = -1
  }

  "remu N % N = 0" in {
    val cpu = runCPU(VECTORS + "ldi r1, 99\nremu r2, r1, r1\nhalt\n")
    cpu.r(2).read shouldBe 0
  }

  "remu large unsigned N % N = 0" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |remu r2, r1, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 0
  }

  "remu small % large = small" in {
    val cpu = runCPU(VECTORS + "ldi r1, 3\nldi r2, 100\nremu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 3
  }

  // ===== DIVU + REMU consistency =====

  "divu and remu consistency: a = (a / b) * b + (a % b)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 47
        |ldi r2, 7
        |divu r3, r1, r2
        |remu r4, r1, r2
        |mul r5, r3, r2
        |add r5, r5, r4
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 6  // 47 / 7 = 6
    cpu.r(4).read shouldBe 5  // 47 % 7 = 5
    cpu.r(5).read shouldBe 47 // 6 * 7 + 5 = 47
  }

  "divu and remu consistency with large unsigned value" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 3
        |divu r3, r1, r2
        |remu r4, r1, r2
        |mul r5, r3, r2
        |add r5, r5, r4
        |halt
        |""".stripMargin)
    // 0xFFFFFFFFFFFFFFFF / 3 = 0x5555555555555555
    cpu.r(3).read shouldBe 0x5555555555555555L
    // 0xFFFFFFFFFFFFFFFF % 3 = 0
    cpu.r(4).read shouldBe 0
    // quotient * divisor + remainder = original
    cpu.r(5).read shouldBe -1L
  }
}
