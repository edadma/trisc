package io.github.edadma.trisc

class PseudoTests extends TestHelpers {

  // ===== RET =====

  "ret returns from subroutine" in {
    val cpu = runCPU(VECTORS +
      """movi r1, func
        |jalr r7, r1
        |ldi r2, 99
        |halt
        |func
        |  ret
        |""".stripMargin)
    cpu.r(2).read shouldBe 99
  }

  // ===== BNE =====

  "bne taken when not equal" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 3
        |ldi r2, 5
        |bne r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
  }

  "bne not taken when equal" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |ldi r2, 5
        |bne r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1
  }

  // ===== BGT =====

  "bgt taken when greater" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 7
        |ldi r2, 3
        |bgt r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
  }

  "bgt not taken when equal" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |ldi r2, 5
        |bgt r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1
  }

  "bgt not taken when less" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 3
        |ldi r2, 7
        |bgt r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1
  }

  // ===== BGE =====

  "bge taken when greater" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 7
        |ldi r2, 3
        |bge r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
  }

  "bge taken when equal" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |ldi r2, 5
        |bge r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
  }

  "bge not taken when less" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 3
        |ldi r2, 7
        |bge r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1
  }

  // ===== BGU =====

  "bgu taken when greater (unsigned)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 7
        |ldi r2, 3
        |bgu r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
  }

  "bgu not taken when equal" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |ldi r2, 5
        |bgu r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1
  }

  // ===== BGEU =====

  "bgeu taken when greater (unsigned)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 7
        |ldi r2, 3
        |bgeu r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
  }

  "bgeu taken when equal" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |ldi r2, 5
        |bgeu r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
  }

  "bgeu not taken when less (unsigned)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 3
        |ldi r2, 7
        |bgeu r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1
  }

  // ===== BLE =====

  "ble taken when less" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 3
        |ldi r2, 7
        |ble r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
  }

  "ble taken when equal" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |ldi r2, 5
        |ble r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
  }

  "ble not taken when greater" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 7
        |ldi r2, 3
        |ble r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1
  }

  // ===== BLEU =====

  "bleu taken when less (unsigned)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 3
        |ldi r2, 7
        |bleu r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
  }

  "bleu taken when equal" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |ldi r2, 5
        |bleu r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 0
  }

  "bleu not taken when greater (unsigned)" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 7
        |ldi r2, 3
        |bleu r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1
  }
}
