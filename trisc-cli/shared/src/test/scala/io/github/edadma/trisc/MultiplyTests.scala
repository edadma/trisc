package io.github.edadma.trisc

class MultiplyTests extends TestHelpers {

  // ===== MUL: basic signed multiply =====

  "mul small positive values" in {
    val cpu = runCPU(VECTORS + "ldi r1, 6\nldi r2, 7\nmul r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 42
    cpu.r(4).read shouldBe 0 // high bits zero for small result
  }

  "mul 0 * anything = 0 with high = 0" in {
    val cpu = runCPU(VECTORS + "ldi r3, 99\nmul r1, r0, r3\nhalt\n")
    cpu.r(1).read shouldBe 0
    cpu.r(2).read shouldBe 0 // high bits
    cpu.r(3).read shouldBe 99 // r3 unchanged
  }

  "mul anything * 0 = 0" in {
    val cpu = runCPU(VECTORS + "ldi r1, 200\nmul r3, r1, r0\nhalt\n")
    cpu.r(3).read shouldBe 0
    cpu.r(4).read shouldBe 0
  }

  "mul 0 * 0 = 0" in {
    val cpu = runCPU(VECTORS + "mul r1, r0, r0\nhalt\n")
    cpu.r(1).read shouldBe 0
    cpu.r(2).read shouldBe 0
  }

  "mul 1 * N = N" in {
    val cpu = runCPU(VECTORS + "ldi r1, 1\nldi r2, 42\nmul r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 42
    cpu.r(4).read shouldBe 0
  }

  "mul N * 1 = N" in {
    val cpu = runCPU(VECTORS + "ldi r1, 137\nldi r2, 1\nmul r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 137
    cpu.r(4).read shouldBe 0
  }

  // ===== MUL: negative values =====

  "mul negative * positive gives negative result, high = -1" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |sub r1, r0, r1
        |ldi r2, 3
        |mul r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe -15
    cpu.r(4).read shouldBe -1 // sign extension for small negative result
  }

  "mul positive * negative gives negative result, high = -1" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 7
        |ldi r2, 4
        |sub r2, r0, r2
        |mul r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe -28
    cpu.r(4).read shouldBe -1
  }

  "mul negative * negative gives positive result, high = 0" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 6
        |sub r1, r0, r1
        |ldi r2, 7
        |sub r2, r0, r2
        |mul r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 42
    cpu.r(4).read shouldBe 0
  }

  "mul -1 * -1 = 1, high = 0" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 1
        |sub r2, r0, r2
        |mul r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1
    cpu.r(4).read shouldBe 0
  }

  // ===== MUL: large values producing nonzero high =====

  "mul large positive values producing nonzero high" in {
    // r1 = 0x100000000 (2^32)
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
    // 2^32 * 2^32 = 2^64, low 64 bits = 0, high 64 bits = 1
    cpu.r(3).read shouldBe 0
    cpu.r(4).read shouldBe 1
  }

  "mul 2^32 * 3 fits in 64 bits, high = 0" in {
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
    cpu.r(4).read shouldBe 0
  }

  // ===== MUL: same register as source and dest =====

  "mul same register source and dest" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\nmul r1, r1, r1\nhalt\n")
    cpu.r(1).read shouldBe 100
    cpu.r(2).read shouldBe 0
  }

  // ===== MUL: rd=r7, high goes to r0 (lost) =====

  "mul with rd=r7: high goes to r0 which is hardwired zero" in {
    // r1 = 2^32, r2 = 2^32, product = 2^64 => low = 0, high = 1
    // but high -> r0, which ignores writes
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
        |mul r7, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(7).read shouldBe 0 // low bits
    cpu.r(0).read shouldBe 0 // high would be 1, but r0 is hardwired zero
  }

  // ===== MUL: register pair mechanics =====

  "mul r1, r2, r3: low in r1, high in r2" in {
    val cpu = runCPU(VECTORS +
      """ldi r2, 1
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |ldi r3, 1
        |sli r3, 0
        |sli r3, 0
        |sli r3, 0
        |sli r3, 0
        |mul r1, r2, r3
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0 // low 64 bits of 2^64
    cpu.r(2).read shouldBe 1 // high 64 bits of 2^64
  }

  "mul r3, r1, r2: low in r3, high in r4" in {
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
    cpu.r(3).read shouldBe 0
    cpu.r(4).read shouldBe 1
  }

  "mul r6, r1, r2: low in r6, high in r7" in {
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
        |mul r6, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(6).read shouldBe 0
    cpu.r(7).read shouldBe 1
  }

  // ===== MULU: basic unsigned multiply =====

  "mulu small positive values" in {
    val cpu = runCPU(VECTORS + "ldi r1, 6\nldi r2, 7\nmulu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 42
    cpu.r(4).read shouldBe 0
  }

  "mulu 0 * anything = 0" in {
    val cpu = runCPU(VECTORS + "ldi r2, 200\nmulu r3, r0, r2\nhalt\n")
    cpu.r(3).read shouldBe 0
    cpu.r(4).read shouldBe 0
  }

  "mulu 1 * N = N" in {
    val cpu = runCPU(VECTORS + "ldi r1, 1\nldi r2, 255\nmulu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 255
    cpu.r(4).read shouldBe 0
  }

  // ===== MULU: large unsigned values =====

  "mulu large values producing nonzero high" in {
    // 2^32 * 2^32 = 2^64, low = 0, high = 1
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
        |mulu r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
    cpu.r(4).read shouldBe 1
  }

  "mulu max unsigned * 2: low = 0xFFFFFFFFFFFFFFFE, high = 1" in {
    // r1 = -1 (0xFFFFFFFFFFFFFFFF as unsigned = max), r2 = 2
    // unsigned: max * 2 = 2^65 - 2, low = 0xFFFFFFFFFFFFFFFE, high = 1
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 2
        |mulu r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe -2L // 0xFFFFFFFFFFFFFFFE as signed Long
    cpu.r(4).read shouldBe 1
  }

  "mulu max unsigned * max unsigned" in {
    // (2^64 - 1) * (2^64 - 1) = 2^128 - 2^65 + 1
    // low 64 bits = 1, high 64 bits = 2^64 - 2 = 0xFFFFFFFFFFFFFFFE
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 1
        |sub r2, r0, r2
        |mulu r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1
    cpu.r(4).read shouldBe -2L // 0xFFFFFFFFFFFFFFFE as signed Long
  }

  // ===== MULU vs MUL: high bits differ for "negative-looking" values =====

  "mulu differs from mul for negative-looking operands" in {
    // -1 (unsigned: max) * 3
    // signed mul:   -1 * 3 = -3, high = -1
    // unsigned mulu: max * 3, low = max - 2 = 0xFFFFFFFFFFFFFFFD, high = 2
    val cpuMul = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 3
        |mul r3, r1, r2
        |halt
        |""".stripMargin)
    cpuMul.r(3).read shouldBe -3
    cpuMul.r(4).read shouldBe -1

    val cpuMulu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 3
        |mulu r3, r1, r2
        |halt
        |""".stripMargin)
    cpuMulu.r(3).read shouldBe -3L // low bits are the same for signed and unsigned
    cpuMulu.r(4).read shouldBe 2   // unsigned high differs from signed high
  }

  "low 64 bits are identical for mul and mulu" in {
    // Verify that low bits match for both instructions with various operands
    // -5 * 7 = -35 (same low bits signed and unsigned)
    val cpuMul = runCPU(VECTORS +
      """ldi r1, 5
        |sub r1, r0, r1
        |ldi r2, 7
        |mul r3, r1, r2
        |halt
        |""".stripMargin)

    val cpuMulu = runCPU(VECTORS +
      """ldi r1, 5
        |sub r1, r0, r1
        |ldi r2, 7
        |mulu r3, r1, r2
        |halt
        |""".stripMargin)

    cpuMul.r(3).read shouldBe cpuMulu.r(3).read // low bits always match
    cpuMul.r(4).read should not be cpuMulu.r(4).read // high bits differ
  }

  // ===== MULU: register pair mechanics =====

  "mulu r1, r2, r3: low in r1, high in r2" in {
    val cpu = runCPU(VECTORS +
      """ldi r2, 1
        |sub r2, r0, r2
        |ldi r3, 2
        |mulu r1, r2, r3
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe -2L // 0xFFFFFFFFFFFFFFFE
    cpu.r(2).read shouldBe 1
  }

  "mulu with rd=r7: high goes to r0 (lost)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 2
        |mulu r7, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(7).read shouldBe -2L // low bits
    cpu.r(0).read shouldBe 0   // high would be 1, but r0 is hardwired zero
  }

  // ===== MUL: edge cases with larger constructed values =====

  "mul 255 * 255 = 65025, high = 0" in {
    val cpu = runCPU(VECTORS + "ldi r1, 255\nldi r2, 255\nmul r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 65025
    cpu.r(4).read shouldBe 0
  }

  "mul large positive * small positive, no overflow" in {
    // r1 = 0x0100 (256)
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sli r1, 0
        |ldi r2, 100
        |mul r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 25600
    cpu.r(4).read shouldBe 0
  }

  "mul -1 * 0 = 0, high = 0" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |mul r3, r1, r0
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
    cpu.r(4).read shouldBe 0
  }

  "mulu same register source and dest" in {
    val cpu = runCPU(VECTORS + "ldi r1, 15\nmulu r1, r1, r1\nhalt\n")
    cpu.r(1).read shouldBe 225
    cpu.r(2).read shouldBe 0
  }

  // ===== MUL: constructed large negative * large positive =====

  "mul large negative * large positive produces nonzero high" in {
    // r1 = -(2^32), r2 = 2^32
    // signed: -(2^32) * 2^32 = -(2^64), low = 0, high = -1
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |sub r1, r0, r1
        |ldi r2, 1
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |mul r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
    cpu.r(4).read shouldBe -1
  }

  // ===== MULU: 2^48 * 2^48 =====

  "mulu 2^48 * 2^48 = 2^96" in {
    // r1 = 2^48 = 0x0001000000000000
    // 2^96: low = 0, high = 2^32 = 0x100000000
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |sli r1, 0
        |ldi r2, 1
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |sli r2, 0
        |mulu r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
    cpu.r(4).read shouldBe 0x100000000L
  }
}
