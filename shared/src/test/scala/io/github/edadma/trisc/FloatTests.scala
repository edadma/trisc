package io.github.edadma.trisc

class FloatTests extends TestHelpers {

  // Helper: load a double from a dd constant into a register via ldd
  private def loadDouble(reg: String, label: String): String =
    s"movi $reg, $label\nldd $reg, $reg, r0\n"

  private def floatProg(body: String, consts: String): String =
    VECTORS + body + "halt\nalign 8\n" + consts

  // ===== FADD =====

  "fadd basic" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fadd r3, r1, r2\n",
      "a dd 1.5\nb dd 2.5\n"))
    cpu.r(3).readf shouldBe 4.0
  }

  "fadd zero plus value" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fadd r2, r1, r0\n",
      "a dd 3.14\n"))
    cpu.r(2).readf shouldBe 3.14
  }

  "fadd negative values" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fadd r3, r1, r2\n",
      "a dd -1.5\nb dd -2.5\n"))
    cpu.r(3).readf shouldBe -4.0
  }

  "fadd positive and negative" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fadd r3, r1, r2\n",
      "a dd 10.0\nb dd -3.0\n"))
    cpu.r(3).readf shouldBe 7.0
  }

  "fadd same register" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fadd r1, r1, r1\n",
      "a dd 2.25\n"))
    cpu.r(1).readf shouldBe 4.5
  }

  "fadd very small values" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fadd r3, r1, r2\n",
      "a dd 0.001\nb dd 0.002\n"))
    cpu.r(3).readf shouldBe (0.003 +- 1e-15)
  }

  // ===== FSUB =====

  "fsub basic" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fsub r3, r1, r2\n",
      "a dd 5.5\nb dd 2.5\n"))
    cpu.r(3).readf shouldBe 3.0
  }

  "fsub same value yields zero" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fsub r2, r1, r1\n",
      "a dd 42.0\n"))
    cpu.r(2).readf shouldBe 0.0
  }

  "fsub produces negative result" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fsub r3, r1, r2\n",
      "a dd 1.0\nb dd 3.0\n"))
    cpu.r(3).readf shouldBe -2.0
  }

  "fsub from zero" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fsub r2, r0, r1\n",
      "a dd 7.5\n"))
    cpu.r(2).readf shouldBe -7.5
  }

  // ===== FMUL =====

  "fmul basic" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fmul r3, r1, r2\n",
      "a dd 3.0\nb dd 4.0\n"))
    cpu.r(3).readf shouldBe 12.0
  }

  "fmul by zero" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fmul r2, r1, r0\n",
      "a dd 99.9\n"))
    cpu.r(2).readf shouldBe 0.0
  }

  "fmul by one" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fmul r3, r1, r2\n",
      "a dd 42.5\nb dd 1.0\n"))
    cpu.r(3).readf shouldBe 42.5
  }

  "fmul negative values" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fmul r3, r1, r2\n",
      "a dd -3.0\nb dd -4.0\n"))
    cpu.r(3).readf shouldBe 12.0
  }

  "fmul mixed sign" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fmul r3, r1, r2\n",
      "a dd -2.0\nb dd 5.0\n"))
    cpu.r(3).readf shouldBe -10.0
  }

  "fmul fractional" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fmul r3, r1, r2\n",
      "a dd 0.5\nb dd 0.5\n"))
    cpu.r(3).readf shouldBe 0.25
  }

  // ===== FDIV =====

  "fdiv basic" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fdiv r3, r1, r2\n",
      "a dd 10.0\nb dd 4.0\n"))
    cpu.r(3).readf shouldBe 2.5
  }

  "fdiv by one" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fdiv r3, r1, r2\n",
      "a dd 42.0\nb dd 1.0\n"))
    cpu.r(3).readf shouldBe 42.0
  }

  "fdiv zero by nonzero" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fdiv r2, r0, r1\n",
      "a dd 5.0\n"))
    cpu.r(2).readf shouldBe 0.0
  }

  "fdiv by zero produces infinity" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fdiv r2, r1, r0\n",
      "a dd 1.0\n"))
    cpu.r(2).readf.isInfinite shouldBe true
  }

  "fdiv negative by positive" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fdiv r3, r1, r2\n",
      "a dd -9.0\nb dd 3.0\n"))
    cpu.r(3).readf shouldBe -3.0
  }

  "fdiv fractional result" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fdiv r3, r1, r2\n",
      "a dd 1.0\nb dd 3.0\n"))
    cpu.r(3).readf shouldBe (1.0 / 3.0 +- 1e-15)
  }

  // ===== FNEG =====

  "fneg basic" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fneg r2, r1\n",
      "a dd 3.14\n"))
    cpu.r(2).readf shouldBe -3.14
  }

  "fneg of negative" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fneg r2, r1\n",
      "a dd -42.0\n"))
    cpu.r(2).readf shouldBe 42.0
  }

  "fneg of zero" in {
    val cpu = runCPU(floatProg(
      "fneg r1, r0\n", ""))
    // -0.0 == 0.0 in IEEE 754
    cpu.r(1).readf shouldBe 0.0
  }

  "fneg double negation" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fneg r2, r1\nfneg r3, r2\n",
      "a dd 7.5\n"))
    cpu.r(3).readf shouldBe 7.5
  }

  "fneg preserves magnitude" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fneg r2, r1\n",
      "a dd 1.23456789\n"))
    cpu.r(2).readf shouldBe -1.23456789
  }

  // ===== FINV =====

  "finv basic" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "finv r2, r1\n",
      "a dd 4.0\n"))
    cpu.r(2).readf shouldBe 0.25
  }

  "finv of one" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "finv r2, r1\n",
      "a dd 1.0\n"))
    cpu.r(2).readf shouldBe 1.0
  }

  "finv of negative" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "finv r2, r1\n",
      "a dd -2.0\n"))
    cpu.r(2).readf shouldBe -0.5
  }

  "finv of zero produces infinity" in {
    val cpu = runCPU(floatProg(
      "finv r1, r0\n", ""))
    cpu.r(1).readf.isInfinite shouldBe true
  }

  "finv double inversion" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "finv r2, r1\nfinv r3, r2\n",
      "a dd 5.0\n"))
    cpu.r(3).readf shouldBe 5.0
  }

  "finv of 0.5" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "finv r2, r1\n",
      "a dd 0.5\n"))
    cpu.r(2).readf shouldBe 2.0
  }

  // ===== Float integration =====

  "fadd then fmul compound expression" in {
    // (2.0 + 3.0) * 4.0 = 20.0
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + loadDouble("r3", "c") +
        "fadd r4, r1, r2\nfmul r5, r4, r3\n",
      "a dd 2.0\nb dd 3.0\nc dd 4.0\n"))
    cpu.r(5).readf shouldBe 20.0
  }

  "fdiv then fsub compound expression" in {
    // 10.0 / 2.0 - 1.5 = 3.5
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + loadDouble("r3", "c") +
        "fdiv r4, r1, r2\nfsub r5, r4, r3\n",
      "a dd 10.0\nb dd 2.0\nc dd 1.5\n"))
    cpu.r(5).readf shouldBe 3.5
  }
}
