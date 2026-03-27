package io.github.edadma.trisc

class CarryTests extends TestHelpers {

  // ===== ADD carry flag =====

  "add small values, no carry" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 10
        |ldi r2, 20
        |add r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 30
    cpu.test(Status.C) shouldBe false
  }

  "add 0 + 0, no carry" in {
    val cpu = runCPU(VECTORS +
      """add r1, r0, r0
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0
    cpu.test(Status.C) shouldBe false
  }

  "add causes unsigned overflow, carry set" in {
    // r1 = 0xFFFFFFFFFFFFFFFF (-1 signed, max unsigned)
    // r1 + 1 overflows unsigned, result = 0, carry = 1
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 1
        |add r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
    cpu.test(Status.C) shouldBe true
  }

  "add max + max, carry set" in {
    // max + max = 0xFFFFFFFFFFFFFFFF + 0xFFFFFFFFFFFFFFFF
    // result = 0xFFFFFFFFFFFFFFFE, carry = 1
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |add r2, r1, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe -2L
    cpu.test(Status.C) shouldBe true
  }

  "carry flag cleared by subsequent add with no overflow" in {
    // First add overflows and sets carry
    // Second add does not overflow and should clear carry
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 1
        |add r3, r1, r2
        |ldi r4, 5
        |ldi r5, 10
        |add r6, r4, r5
        |halt
        |""".stripMargin)
    cpu.r(6).read shouldBe 15
    cpu.test(Status.C) shouldBe false
  }

  // ===== SUB carry/borrow flag =====

  "sub a >= b, no borrow" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 50
        |ldi r2, 20
        |sub r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 30
    cpu.test(Status.C) shouldBe false
  }

  "sub a < b unsigned, borrow set" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |ldi r2, 10
        |sub r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe -5L
    cpu.test(Status.C) shouldBe true
  }

  "sub 0 - 0, no borrow" in {
    val cpu = runCPU(VECTORS +
      """sub r1, r0, r0
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0
    cpu.test(Status.C) shouldBe false
  }

  "sub 0 - 1, borrow set" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r2, r0, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe -1L
    cpu.test(Status.C) shouldBe true
  }

  "borrow flag cleared by subsequent sub with no underflow" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r2, r0, r1
        |ldi r3, 20
        |ldi r4, 5
        |sub r5, r3, r4
        |halt
        |""".stripMargin)
    cpu.r(5).read shouldBe 15
    cpu.test(Status.C) shouldBe false
  }

  // ===== ADC =====

  "adc basic, no prior carry" in {
    // Ensure carry is clear first (add 0+0), then adc
    val cpu = runCPU(VECTORS +
      """add r7, r0, r0
        |ldi r1, 10
        |ldi r2, 20
        |adc r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 30
    cpu.test(Status.C) shouldBe false
  }

  "adc with carry set adds 1 extra" in {
    // Set carry via overflowing add, then adc should include +1
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 1
        |add r3, r1, r2
        |ldi r4, 10
        |ldi r5, 20
        |adc r6, r4, r5
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
    cpu.r(6).read shouldBe 31
    cpu.test(Status.C) shouldBe false
  }

  "adc 0 + 0 + carry = 1" in {
    // Set carry, then adc r0, r0 should give 1
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 1
        |add r3, r1, r2
        |adc r4, r0, r0
        |halt
        |""".stripMargin)
    cpu.r(4).read shouldBe 1
    cpu.test(Status.C) shouldBe false
  }

  "adc overflow sets carry for next adc" in {
    // Clear carry with add, then adc max + 1 should overflow and set carry
    // Use add to clear carry right before the adc (sub would set it)
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 1
        |add r7, r0, r0
        |adc r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
    cpu.test(Status.C) shouldBe true
  }

  "adc chain: 128-bit addition via add then adc" in {
    // Compute (r2:r1) + (r4:r3) where each pair is low:high 64-bit limbs
    // (0x0000000000000001 : 0xFFFFFFFFFFFFFFFF) + (0x0000000000000000 : 0x0000000000000002)
    // = 1:max + 0:2 => low: max+2 overflows to 1, carry=1, high: 1+0+1 = 2
    // Result: (2 : 1)
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 1
        |ldi r3, 2
        |ldi r4, 0
        |add r5, r1, r3
        |adc r6, r2, r4
        |halt
        |""".stripMargin)
    cpu.r(5).read shouldBe 1L
    cpu.r(6).read shouldBe 2L
  }

  // ===== SBC =====

  "sbc basic, no prior borrow" in {
    val cpu = runCPU(VECTORS +
      """add r7, r0, r0
        |ldi r1, 50
        |ldi r2, 20
        |sbc r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 30
    cpu.test(Status.C) shouldBe false
  }

  "sbc with borrow set subtracts 1 extra" in {
    // Set borrow: sub 0 - 1
    // Then sbc 20 - 5 - 1 = 14
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r2, r0, r1
        |ldi r3, 20
        |ldi r4, 5
        |sbc r5, r3, r4
        |halt
        |""".stripMargin)
    cpu.r(5).read shouldBe 14
    cpu.test(Status.C) shouldBe false
  }

  "sbc 0 - 0 - borrow = -1 with borrow set" in {
    // Set borrow first, then sbc 0,0 => 0 - 0 - 1 = -1 (underflow, borrow set)
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r2, r0, r1
        |sbc r3, r0, r0
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe -1L
    cpu.test(Status.C) shouldBe true
  }

  "sbc underflow sets borrow" in {
    // No prior borrow, sbc 0 - 1 => underflow
    val cpu = runCPU(VECTORS +
      """add r7, r0, r0
        |ldi r1, 1
        |sbc r2, r0, r1
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe -1L
    cpu.test(Status.C) shouldBe true
  }

  "sbc chain: 128-bit subtraction via sub then sbc" in {
    // (r2:r1) - (r4:r3) where pair is high:low
    // (2 : 1) - (1 : 3)
    // low: 1 - 3 underflows => 0xFFFFFFFFFFFFFFFF, borrow=1
    // high: 2 - 1 - 1(borrow) = 0
    // Result: (0 : 0xFFFFFFFFFFFFFFFE)
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |ldi r2, 2
        |ldi r3, 3
        |ldi r4, 1
        |sub r5, r1, r3
        |sbc r6, r2, r4
        |halt
        |""".stripMargin)
    cpu.r(5).read shouldBe -2L
    cpu.r(6).read shouldBe 0L
  }

  // ===== GPSR carry bit verification =====

  "gpsr reads carry bit after overflow" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 1
        |add r3, r1, r2
        |gpsr r4
        |halt
        |""".stripMargin)
    (cpu.r(4).read & 4) shouldBe 4
  }

  "gpsr reads no carry bit after normal add" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |ldi r2, 10
        |add r3, r1, r2
        |gpsr r4
        |halt
        |""".stripMargin)
    (cpu.r(4).read & 4) shouldBe 0
  }

  // ===== Multi-limb comprehensive =====

  "128-bit add: full multi-limb with carry propagation" in {
    // Add two 128-bit numbers:
    // A = (0x0000000000000003 : 0xFFFFFFFFFFFFFFFE)  (high : low)
    // B = (0x0000000000000001 : 0x0000000000000005)
    // low:  0xFFFFFFFFFFFFFFFE + 5 = 3 with carry
    // high: 3 + 1 + 1(carry) = 5
    // Result: (5 : 3)
    val cpu = runCPU(VECTORS +
      """ldi r1, 2
        |sub r1, r0, r1
        |ldi r2, 3
        |ldi r3, 5
        |ldi r4, 1
        |add r5, r1, r3
        |adc r6, r2, r4
        |halt
        |""".stripMargin)
    cpu.r(5).read shouldBe 3L
    cpu.r(6).read shouldBe 5L
    cpu.test(Status.C) shouldBe false
  }

  "128-bit sub: full multi-limb with borrow propagation" in {
    // Subtract two 128-bit numbers:
    // A = (5 : 3)  (high : low)
    // B = (1 : 5)
    // low:  3 - 5 underflows => 0xFFFFFFFFFFFFFFFE, borrow=1
    // high: 5 - 1 - 1(borrow) = 3
    // Result: (3 : 0xFFFFFFFFFFFFFFFE)
    val cpu = runCPU(VECTORS +
      """ldi r1, 3
        |ldi r2, 5
        |ldi r3, 5
        |ldi r4, 1
        |sub r5, r1, r3
        |sbc r6, r2, r4
        |halt
        |""".stripMargin)
    cpu.r(5).read shouldBe -2L
    cpu.r(6).read shouldBe 3L
    cpu.test(Status.C) shouldBe false
  }
}
