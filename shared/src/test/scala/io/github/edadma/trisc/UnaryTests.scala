package io.github.edadma.trisc

class UnaryTests extends TestHelpers {

  // ===== NEG =====

  "neg basic" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nneg r2, r1\nhalt\n")
    cpu.r(2).read shouldBe -42
  }

  "neg zero" in {
    val cpu = runCPU(VECTORS + "neg r1, r0\nhalt\n")
    cpu.r(1).read shouldBe 0
  }

  "neg double negation" in {
    val cpu = runCPU(VECTORS + "ldi r1, 7\nneg r2, r1\nneg r3, r2\nhalt\n")
    cpu.r(3).read shouldBe 7
  }

  // ===== NOT =====

  "not basic" in {
    val cpu = runCPU(VECTORS + "not r1, r0\nhalt\n")
    cpu.r(1).read shouldBe -1 // ~0 = all 1s = -1
  }

  "not double inversion" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nnot r2, r1\nnot r3, r2\nhalt\n")
    cpu.r(3).read shouldBe 42
  }

  "not of 0xFF" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0xFF\nnot r2, r1\nhalt\n")
    cpu.r(2).read shouldBe ~0xFFL
  }

  // ===== ZEB (zero extend byte) =====

  "zeb clears upper bits" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |zeb r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 0xFF // -1 → 0xFF
  }

  "zeb preserves low byte" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x42\nzeb r2, r1\nhalt\n")
    cpu.r(2).read shouldBe 0x42
  }

  // ===== ZES (zero extend short) =====

  "zes clears upper bits" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |zes r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 0xFFFF
  }

  "zes preserves low short" in {
    val cpu = runCPU(VECTORS + "movi r1, 0x1234\nzes r2, r1\nhalt\n")
    cpu.r(2).read shouldBe 0x1234
  }

  // ===== ZEW (zero extend word) =====

  "zew clears upper bits" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |zew r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 0xFFFFFFFFL
  }

  // ===== SEB (sign extend byte) =====

  "seb extends negative byte" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x80\nseb r2, r1\nhalt\n")
    cpu.r(2).read shouldBe -128
  }

  "seb preserves positive byte" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x42\nseb r2, r1\nhalt\n")
    cpu.r(2).read shouldBe 0x42
  }

  // ===== SES (sign extend short) =====

  "ses extends negative short" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0x80
        |sli r1, 0x00
        |ses r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe -32768
  }

  "ses preserves positive short" in {
    val cpu = runCPU(VECTORS + "movi r1, 0x1234\nses r2, r1\nhalt\n")
    cpu.r(2).read shouldBe 0x1234
  }

  // ===== SEW (sign extend word) =====

  "sew extends negative word" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0x80
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |sew r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe -2147483648L // 0x80000000 sign-extended to 64 bits
  }

  "sew preserves positive word" in {
    val cpu = runCPU(VECTORS + "movi r1, 0x1234\nsew r2, r1\nhalt\n")
    cpu.r(2).read shouldBe 0x1234
  }

  // ===== REV (byte-reverse) =====

  "rev of 0 is 0" in {
    val cpu = runCPU(VECTORS +
      """rev r1, r0
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0
  }

  "rev of 0x0102 swaps bytes" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sli r1, 2
        |rev r2, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 0x0201000000000000L
  }

  "rev twice is identity" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0xAB
        |rev r2, r1
        |rev r3, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0xAB
  }

  // ===== SEXT (sign-extend from bit width) =====

  "sext from 8 bits, positive" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0x7F
        |ldi r2, 8
        |sext r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0x7F
  }

  "sext from 8 bits, negative" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0x80
        |ldi r2, 8
        |sext r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe -128L
  }

  "sext from 16 bits, negative" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0xFF
        |sli r1, 0xFE
        |ldi r2, 16
        |sext r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe -2L
  }

  "sext from 1 bit" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |ldi r2, 1
        |sext r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe -1L
  }

  "sext with width 0 is identity" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 42
        |ldi r2, 0
        |sext r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
  }
}
