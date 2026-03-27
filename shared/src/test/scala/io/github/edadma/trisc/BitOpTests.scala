package io.github.edadma.trisc

class BitOpTests extends TestHelpers {

  // ===== BTST =====

  "btst bit 0 of 1 is 1" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |ldi r2, 0
        |btst r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
  }

  "btst bit 1 of 1 is 0" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |ldi r2, 1
        |btst r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0
  }

  "btst bit 3 of 0xFF is 1" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0xFF
        |ldi r2, 3
        |btst r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
  }

  "btst bit 8 of 0xFF is 0" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0xFF
        |ldi r2, 8
        |btst r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0
  }

  // ===== BSET =====

  "bset bit 0 of 0 gives 1" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0
        |ldi r2, 0
        |bset r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
  }

  "bset bit 3 of 0 gives 8" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0
        |ldi r2, 3
        |bset r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 8
  }

  "bset bit 0 of 0xFF stays 0xFF" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0xFF
        |ldi r2, 0
        |bset r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0xFF
  }

  "bset bit 8 of 0xFF gives 0x1FF" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0xFF
        |ldi r2, 8
        |bset r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0x1FF
  }

  // ===== BCLR =====

  "bclr bit 0 of 1 gives 0" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |ldi r2, 0
        |bclr r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0
  }

  "bclr bit 3 of 0xFF gives 0xF7" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0xFF
        |ldi r2, 3
        |bclr r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0xF7
  }

  "bclr bit 0 of 0 stays 0" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0
        |ldi r2, 0
        |bclr r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0
  }

  // ===== ROL =====

  "rol by 1" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |ldi r2, 1
        |rol r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 2
  }

  "rol by 4" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0xFF
        |ldi r2, 4
        |rol r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0xFF0
  }

  "rol wraps high bits to low" in {
    // Set bit 63 (sign bit), rotate left by 1 — should wrap to bit 0
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |ldi r3, 63
        |lsl r1, r1, r3
        |ldi r2, 1
        |rol r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
  }

  "rol by 0 is identity" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 42
        |ldi r2, 0
        |rol r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
  }

  // ===== ROR =====

  "ror by 1" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 2
        |ldi r2, 1
        |ror r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
  }

  "ror wraps low bits to high" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |ldi r2, 1
        |ror r1, r2
        |halt
        |""".stripMargin)
    // bit 0 wraps to bit 63
    cpu.r(1).read shouldBe Long.MinValue // 0x8000000000000000
  }

  "ror by 0 is identity" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 42
        |ldi r2, 0
        |ror r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
  }

  "rol then ror is identity" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0xAB
        |ldi r2, 7
        |rol r1, r2
        |ror r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0xAB
  }
}
