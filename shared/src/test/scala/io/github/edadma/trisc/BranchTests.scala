package io.github.edadma.trisc

class BranchTests extends TestHelpers {

  // ===== BEQ =====

  "beq taken when equal" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |ldi r2, 5
        |beq r1, r2, skip
        |ldi r3, 1
        |skip
        |ldi r4, 99
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
    cpu.r(4).read shouldBe 99
  }

  "beq not taken when unequal" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |ldi r2, 6
        |beq r1, r2, skip
        |ldi r3, 1
        |skip
        |ldi r4, 99
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1
    cpu.r(4).read shouldBe 99
  }

  "beq backward branch (loop)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0
        |ldi r2, 3
        |loop
        |  addi r1, r1, 1
        |  beq r1, r2, done
        |  bra loop
        |done
        |  halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 3
  }

  "beq with r0,r0 is always taken" in {
    val cpu = runCPU(VECTORS + "beq r0, r0, skip\nldi r1, 1\nskip\nhalt\n")
    cpu.r(1).read shouldBe 0
  }

  // ===== BLS =====

  "bls taken when less" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 3
        |ldi r2, 5
        |bls r1, r2, skip
        |ldi r3, 1
        |skip
        |ldi r4, 99
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
    cpu.r(4).read shouldBe 99
  }

  "bls not taken when equal" in {
    val cpu = runCPU(VECTORS + "ldi r1, 5\nldi r2, 5\nbls r1, r2, skip\nldi r3, 1\nskip\nhalt\n")
    cpu.r(3).read shouldBe 1
  }

  "bls not taken when greater" in {
    val cpu = runCPU(VECTORS + "ldi r1, 7\nldi r2, 3\nbls r1, r2, skip\nldi r3, 1\nskip\nhalt\n")
    cpu.r(3).read shouldBe 1
  }

  // ===== BLU =====

  "blu taken when less (unsigned)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 3
        |ldi r2, 5
        |blu r1, r2, skip
        |ldi r3, 1
        |skip
        |ldi r4, 99
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
    cpu.r(4).read shouldBe 99
  }

  "blu not taken when equal" in {
    val cpu = runCPU(VECTORS + "ldi r1, 5\nldi r2, 5\nblu r1, r2, skip\nldi r3, 1\nskip\nhalt\n")
    cpu.r(3).read shouldBe 1
  }

  "blu not taken when greater" in {
    val cpu = runCPU(VECTORS + "ldi r1, 7\nldi r2, 3\nblu r1, r2, skip\nldi r3, 1\nskip\nhalt\n")
    cpu.r(3).read shouldBe 1
  }

  "blu treats negative as large unsigned" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |sub r2, r0, r1
        |blu r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    // 5 < (unsigned -5), so branch taken
    cpu.r(3).read shouldBe 0
  }

  "blu negative not less than positive (unsigned)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |sub r2, r0, r1
        |blu r2, r1, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    // (unsigned -5) is NOT < 5, so branch not taken
    cpu.r(3).read shouldBe 1
  }

  // ===== BRA =====

  "bra forward branch" in {
    val cpu = runCPU(VECTORS + "bra skip\nldi r1, 1\nskip\nldi r2, 99\nhalt\n")
    cpu.r(1).read shouldBe 0
    cpu.r(2).read shouldBe 99
  }

  "bra backward branch (loop)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0
        |ldi r2, 5
        |loop
        |  addi r1, r1, 1
        |  bls r1, r2, loop
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 5
  }
}
