package io.github.edadma.trisc

class BitCountTests extends TestHelpers {

  // ===== CLZ (Count Leading Zeros) =====

  "clz of 0 is 64" in {
    val cpu = runCPU(VECTORS +
      """clz r1, r0
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 64
  }

  "clz of 1 is 63" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |clz r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 63
  }

  "clz of 2 is 62" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 2
        |clz r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 62
  }

  "clz of 0x80 is 56" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0x80
        |clz r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 56
  }

  "clz of -1 (all ones) is 0" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |clz r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 0
  }

  "clz of value with high bit set is 0" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |ldi r2, 63
        |lsl r1, r1, r2
        |clz r3, r1
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
  }

  "clz of 0xFF is 56" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0xFF
        |clz r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 56
  }

  // ===== CTZ (Count Trailing Zeros) =====

  "ctz of 0 is 64" in {
    val cpu = runCPU(VECTORS +
      """ctz r1, r0
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 64
  }

  "ctz of 1 is 0" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |ctz r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 0
  }

  "ctz of 2 is 1" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 2
        |ctz r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 1
  }

  "ctz of 4 is 2" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 4
        |ctz r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 2
  }

  "ctz of 0x80 is 7" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0x80
        |ctz r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 7
  }

  "ctz of -1 (all ones) is 0" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ctz r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 0
  }

  "ctz of value with only high bit set is 63" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |ldi r2, 63
        |lsl r1, r1, r2
        |ctz r3, r1
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 63
  }

  "ctz of even number (6) is 1" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 6
        |ctz r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 1
  }
}
