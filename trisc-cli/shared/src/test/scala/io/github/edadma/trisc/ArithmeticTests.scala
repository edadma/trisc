package io.github.edadma.trisc

class ArithmeticTests extends TestHelpers {

  // ===== ADD =====

  "add basic" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\nldi r2, 20\nadd r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 30
  }

  "add with r0 is identity" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nadd r2, r1, r0\nhalt\n")
    cpu.r(2).read shouldBe 42
  }

  "add same register as source and dest" in {
    val cpu = runCPU(VECTORS + "ldi r1, 5\nadd r1, r1, r1\nhalt\n")
    cpu.r(1).read shouldBe 10
  }

  "add zero plus zero" in {
    val cpu = runCPU(VECTORS + "add r1, r0, r0\nhalt\n")
    cpu.r(1).read shouldBe 0
  }

  // ===== SUB =====

  "sub basic" in {
    val cpu = runCPU(VECTORS + "ldi r1, 50\nldi r2, 20\nsub r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 30
  }

  "sub same register yields zero" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nsub r2, r1, r1\nhalt\n")
    cpu.r(2).read shouldBe 0
  }

  "sub produces negative result" in {
    val cpu = runCPU(VECTORS + "ldi r1, 5\nldi r2, 10\nsub r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe -5
  }

  // ===== MUL =====

  "mul basic" in {
    val cpu = runCPU(VECTORS + "ldi r1, 6\nldi r2, 7\nmul r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 42
  }

  "mul by zero" in {
    val cpu = runCPU(VECTORS + "ldi r1, 99\nmul r2, r1, r0\nhalt\n")
    cpu.r(2).read shouldBe 0
  }

  "mul by one" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nldi r2, 1\nmul r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 42
  }

  // ===== DIV =====

  "div basic" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nldi r2, 6\ndiv r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 7
  }

  "div truncates toward zero" in {
    val cpu = runCPU(VECTORS + "ldi r1, 7\nldi r2, 2\ndiv r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 3
  }

  "div zero by nonzero" in {
    val cpu = runCPU(VECTORS + "ldi r2, 5\ndiv r1, r0, r2\nhalt\n")
    cpu.r(1).read shouldBe 0
  }

  // ===== REM =====

  "rem basic" in {
    val cpu = runCPU(VECTORS + "ldi r1, 17\nldi r2, 5\nrem r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 2
  }

  "rem with no remainder" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\nldi r2, 5\nrem r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 0
  }

  "rem zero by nonzero" in {
    val cpu = runCPU(VECTORS + "ldi r2, 7\nrem r1, r0, r2\nhalt\n")
    cpu.r(1).read shouldBe 0
  }

  // ===== SLT =====

  "slt sets 1 when less" in {
    val cpu = runCPU(VECTORS + "ldi r1, 3\nldi r2, 5\nslt r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 1
  }

  "slt sets 0 when equal" in {
    val cpu = runCPU(VECTORS + "ldi r1, 5\nldi r2, 5\nslt r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 0
  }

  "slt sets 0 when greater" in {
    val cpu = runCPU(VECTORS + "ldi r1, 7\nldi r2, 3\nslt r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 0
  }

  "slt handles negative values" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |ldi r2, 10
        |sub r1, r0, r1
        |slt r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1 // -5 < 10
  }

  // ===== SLTU =====

  "sltu sets 1 when less (unsigned)" in {
    val cpu = runCPU(VECTORS + "ldi r1, 3\nldi r2, 5\nsltu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 1
  }

  "sltu sets 0 when equal" in {
    val cpu = runCPU(VECTORS + "ldi r1, 5\nldi r2, 5\nsltu r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 0
  }

  "sltu treats negative as large unsigned" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |sub r2, r0, r1
        |sltu r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 1 // 5 < (large unsigned value of -5)
  }
}
