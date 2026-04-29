package io.github.edadma.trisc

class MultiplyTests extends TestHelpers {

  // ===== MUL: basic signed multiply (low 64 bits, signed/unsigned identical) =====

  "mul small positive values" in {
    val cpu = runCPU(VECTORS + "ldi r1, 6\nldi r2, 7\nmul r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 42
  }

  "mul 0 * anything = 0" in {
    val cpu = runCPU(VECTORS + "ldi r3, 99\nmul r1, r0, r3\nhalt\n")
    cpu.r(1).read shouldBe 0
  }

  "mul anything * 0 = 0" in {
    val cpu = runCPU(VECTORS + "ldi r1, 200\nmul r3, r1, r0\nhalt\n")
    cpu.r(3).read shouldBe 0
  }

  "mul 0 * 0 = 0" in {
    val cpu = runCPU(VECTORS + "mul r1, r0, r0\nhalt\n")
    cpu.r(1).read shouldBe 0
  }

  "mul 1 * N = N" in {
    val cpu = runCPU(VECTORS + "ldi r1, 1\nldi r2, 42\nmul r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 42
  }

  "mul N * 1 = N" in {
    val cpu = runCPU(VECTORS + "ldi r1, 137\nldi r2, 1\nmul r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 137
  }

  // ===== MUL: negative values (low 64 bits) =====

  "mul negative * positive gives negative result" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |sub r1, r0, r1
        |ldi r2, 3
        |mul r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe -15
  }

  "mul negative * negative gives positive result" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 6
        |sub r1, r0, r1
        |ldi r2, 7
        |sub r2, r0, r2
        |mul r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 42
  }

  "mul -1 * -1 = 1" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 1
        |sub r2, r0, r2
        |mul r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1
  }

  // ===== MUL: low half wraps for large values =====

  "mul 2^32 * 2^32 wraps to 0 in low 64" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |ldi r2, 1
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |mul r3, r1, r2
        |halt
        |""".stripMargin)
    // 2^32 * 2^32 = 2^64, low 64 bits = 0
    cpu.r(3).read shouldBe 0
  }

  "mul 2^32 * 3 fits in 64 bits" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |ldi r2, 3
        |mul r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0x300000000L
  }

  "mul same register source and dest" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\nmul r1, r1, r1\nhalt\n")
    cpu.r(1).read shouldBe 100
  }

  // ===== MUL: writes only rd; r((d+1)&7) is preserved =====

  "mul r3, r1, r2 leaves r4 untouched" in {
    val cpu = runCPU(VECTORS +
      """ldi r4, 99
        |ldi r1, 1
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |ldi r2, 1
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |mul r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0      // low 64 bits of 2^64
    cpu.r(4).read shouldBe 99     // r((d+1)&7) = r4 — unchanged
  }

  // ===== MULH: signed high 64 bits (destructive: rd = high(rd * rb)) =====

  "mulh small product gives 0 high" in {
    // 6 * 7 = 42 — high is 0
    val cpu = runCPU(VECTORS + "ldi r1, 6\nldi r2, 7\nmulh r1, r2\nhalt\n")
    cpu.r(1).read shouldBe 0
  }

  "mulh small negative product sign-extends" in {
    // -1 * 3 = -3 — signed high is -1
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 3
        |mulh r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe -1
  }

  "mulh 2^32 * 2^32 gives 1 in high" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |ldi r2, 1
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |mulh r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
  }

  // ===== MULHU: unsigned high 64 bits =====

  "mulhu small unsigned product gives 0 high" in {
    val cpu = runCPU(VECTORS + "ldi r1, 6\nldi r2, 7\nmulhu r1, r2\nhalt\n")
    cpu.r(1).read shouldBe 0
  }

  "mulhu max unsigned * 2 gives 1 in high" in {
    // 0xFFFFFFFFFFFFFFFF * 2 = 0x1FFFFFFFFFFFFFFFE; high = 1
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 2
        |mulhu r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
  }

  "mulhu max * max gives 0xFFFFFFFFFFFFFFFE high" in {
    // (2^64 - 1)^2 = 2^128 - 2^65 + 1
    // high 64 bits = 2^64 - 2 = 0xFFFFFFFFFFFFFFFE (signed: -2)
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 1
        |sub r2, r0, r2
        |mulhu r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe -2L
  }

  "mulhu vs mulh differ for negative-looking operands" in {
    // -1 (= max as unsigned) * 3
    // signed high  : -1
    // unsigned high: 2
    val signed = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 3
        |mulh r1, r2
        |halt
        |""".stripMargin)
    signed.r(1).read shouldBe -1

    val unsigned = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 3
        |mulhu r1, r2
        |halt
        |""".stripMargin)
    unsigned.r(1).read shouldBe 2
  }

  // ===== MULHSU: signed × unsigned high =====

  "mulhsu negative_signed * unsigned" in {
    // -1 (signed) * 3 (unsigned) = -3 ; high = -1
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 3
        |mulhsu r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe -1
  }

  "mulhsu small_signed * unsigned_max" in {
    // 3 (signed) * 0xFFFFFFFFFFFFFFFF (unsigned, = 2^64 - 1) = 3 * (2^64 - 1)
    // high 64 = 2 (since 3 * (2^64 - 1) = 3 * 2^64 - 3 → high = 2, low = -3)
    val cpu = runCPU(VECTORS +
      """ldi r1, 3
        |ldi r2, 1
        |sub r2, r0, r2
        |mulhsu r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 2
  }

  // ===== Bignum primitive: full 128-bit product =====

  "mul + mulhu produce both halves of 64x64 unsigned" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |ldi r2, 1
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |mul  r3, r1, r2
        |mov  r4, r1
        |mulhu r4, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0   // low half of 2^64
    cpu.r(4).read shouldBe 1   // high half
  }

  // ===== REM / REMU =====

  "rem 7 % 3 = 1" in {
    val cpu = runCPU(VECTORS + "ldi r1, 7\nldi r2, 3\nrem r1, r2\nhalt\n")
    cpu.r(1).read shouldBe 1
  }

  "rem -7 % 3 = -1 (signed)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 7
        |sub r1, r0, r1
        |ldi r2, 3
        |rem r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe -1
  }

  "rem 7 % -3 = 1" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 7
        |ldi r2, 3
        |sub r2, r0, r2
        |rem r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
  }

  "remu 7 % 3 = 1" in {
    val cpu = runCPU(VECTORS + "ldi r1, 7\nldi r2, 3\nremu r1, r2\nhalt\n")
    cpu.r(1).read shouldBe 1
  }

  "remu -1 (= max) % 3" in {
    // 0xFFFFFFFFFFFFFFFF % 3 = 0  (since 2^64 - 1 = 3 * k)
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 3
        |remu r1, r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0
  }
}
