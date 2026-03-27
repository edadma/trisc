package io.github.edadma.trisc

class NewFloatTests extends TestHelpers {

  private def loadDouble(reg: String, label: String): String =
    s"movi $reg, $label\nldd $reg, $reg, r0\n"

  private def floatProg(body: String, consts: String): String =
    VECTORS + body + "halt\nalign 8\n" + consts

  // ===== FSLT (float set less than) =====

  "fslt when a < b yields 1" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fslt r3, r1, r2\n",
      "a dd 1.0\nb dd 2.0\n"))
    cpu.r(3).read shouldBe 1
  }

  "fslt when a > b yields 0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fslt r3, r1, r2\n",
      "a dd 5.0\nb dd 3.0\n"))
    cpu.r(3).read shouldBe 0
  }

  "fslt when a == b yields 0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fslt r3, r1, r2\n",
      "a dd 4.0\nb dd 4.0\n"))
    cpu.r(3).read shouldBe 0
  }

  "fslt negative < positive yields 1" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fslt r3, r1, r2\n",
      "a dd -3.0\nb dd 1.0\n"))
    cpu.r(3).read shouldBe 1
  }

  "fslt positive < negative yields 0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fslt r3, r1, r2\n",
      "a dd 1.0\nb dd -3.0\n"))
    cpu.r(3).read shouldBe 0
  }

  "fslt value < zero yields 0 when value is positive" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fslt r2, r1, r0\n",
      "a dd 5.0\n"))
    cpu.r(2).read shouldBe 0
  }

  "fslt zero < positive yields 1" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fslt r2, r0, r1\n",
      "a dd 5.0\n"))
    cpu.r(2).read shouldBe 1
  }

  "fslt negative < zero yields 1" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fslt r2, r1, r0\n",
      "a dd -2.5\n"))
    cpu.r(2).read shouldBe 1
  }

  "fslt NaN < anything yields 0" in {
    // 0.0 / 0.0 = NaN
    val cpu = runCPU(floatProg(
      "fdiv r1, r0, r0\n" + loadDouble("r2", "a") + "fslt r3, r1, r2\n",
      "a dd 1.0\n"))
    cpu.r(3).read shouldBe 0
  }

  "fslt anything < NaN yields 0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fdiv r2, r0, r0\n" + "fslt r3, r1, r2\n",
      "a dd 1.0\n"))
    cpu.r(3).read shouldBe 0
  }

  // ===== FPOW (float power, destructive: ra = pow(ra, rb)) =====

  "fpow square 2.0 ^ 2.0 = 4.0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fpow r1, r2\n",
      "a dd 2.0\nb dd 2.0\n"))
    cpu.r(1).readf shouldBe 4.0
  }

  "fpow cube 3.0 ^ 3.0 = 27.0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fpow r1, r2\n",
      "a dd 3.0\nb dd 3.0\n"))
    cpu.r(1).readf shouldBe 27.0
  }

  "fpow to the 0 yields 1.0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fpow r1, r0\n",
      "a dd 99.0\n"))
    cpu.r(1).readf shouldBe 1.0
  }

  "fpow to the 1 yields identity" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fpow r1, r2\n",
      "a dd 7.5\nb dd 1.0\n"))
    cpu.r(1).readf shouldBe 7.5
  }

  "fpow fractional exponent 4.0 ^ 0.5 = 2.0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fpow r1, r2\n",
      "a dd 4.0\nb dd 0.5\n"))
    cpu.r(1).readf shouldBe 2.0
  }

  "fpow 8.0 ^ (1/3) = 2.0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fpow r1, r2\n",
      "a dd 8.0\nb dd 0.3333333333333333\n"))
    cpu.r(1).readf shouldBe (2.0 +- 1e-10)
  }

  "fpow negative base with integer exponent (-2.0) ^ 3.0 = -8.0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fpow r1, r2\n",
      "a dd -2.0\nb dd 3.0\n"))
    cpu.r(1).readf shouldBe -8.0
  }

  "fpow negative base with even exponent (-3.0) ^ 2.0 = 9.0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fpow r1, r2\n",
      "a dd -3.0\nb dd 2.0\n"))
    cpu.r(1).readf shouldBe 9.0
  }

  // ===== CVT (int to float) =====

  "cvt 0 to 0.0" in {
    val cpu = runCPU(floatProg(
      "cvt r1, r0\n", ""))
    cpu.r(1).readf shouldBe 0.0
  }

  "cvt 1 to 1.0" in {
    val cpu = runCPU(floatProg(
      "ldi r1, 1\ncvt r2, r1\n", ""))
    cpu.r(2).readf shouldBe 1.0
  }

  "cvt 42 to 42.0" in {
    val cpu = runCPU(floatProg(
      "ldi r1, 42\ncvt r2, r1\n", ""))
    cpu.r(2).readf shouldBe 42.0
  }

  "cvt negative -5 to -5.0" in {
    val cpu = runCPU(floatProg(
      "ldi r1, 5\nsub r1, r0, r1\ncvt r2, r1\n", ""))
    cpu.r(2).readf shouldBe -5.0
  }

  "cvt negative -1 to -1.0" in {
    val cpu = runCPU(floatProg(
      "ldi r1, 1\nsub r1, r0, r1\ncvt r2, r1\n", ""))
    cpu.r(2).readf shouldBe -1.0
  }

  "cvt large value 1000 to 1000.0" in {
    val cpu = runCPU(floatProg(
      "movi r1, 1000\ncvt r2, r1\n", ""))
    cpu.r(2).readf shouldBe 1000.0
  }

  "cvt 100 to 100.0" in {
    val cpu = runCPU(floatProg(
      "ldi r1, 100\ncvt r2, r1\n", ""))
    cpu.r(2).readf shouldBe 100.0
  }

  // ===== FINT (float to int, truncation toward zero) =====

  "fint 1.0 to 1" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fint r2, r1\n",
      "a dd 1.0\n"))
    cpu.r(2).read shouldBe 1
  }

  "fint 1.9 truncates to 1" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fint r2, r1\n",
      "a dd 1.9\n"))
    cpu.r(2).read shouldBe 1
  }

  "fint -1.9 truncates to -1" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fint r2, r1\n",
      "a dd -1.9\n"))
    cpu.r(2).read shouldBe -1
  }

  "fint 0.0 to 0" in {
    val cpu = runCPU(floatProg(
      "fint r1, r0\n", ""))
    cpu.r(1).read shouldBe 0
  }

  "fint large float 12345.6789 to 12345" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fint r2, r1\n",
      "a dd 12345.6789\n"))
    cpu.r(2).read shouldBe 12345
  }

  "fint -0.5 truncates to 0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fint r2, r1\n",
      "a dd -0.5\n"))
    cpu.r(2).read shouldBe 0
  }

  "fint 99.99 truncates to 99" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fint r2, r1\n",
      "a dd 99.99\n"))
    cpu.r(2).read shouldBe 99
  }

  // ===== FSQRT (float square root) =====

  "fsqrt 4.0 = 2.0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fsqrt r2, r1\n",
      "a dd 4.0\n"))
    cpu.r(2).readf shouldBe 2.0
  }

  "fsqrt 9.0 = 3.0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fsqrt r2, r1\n",
      "a dd 9.0\n"))
    cpu.r(2).readf shouldBe 3.0
  }

  "fsqrt 2.0 is approximately 1.41421356" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fsqrt r2, r1\n",
      "a dd 2.0\n"))
    cpu.r(2).readf shouldBe (1.41421356237 +- 1e-10)
  }

  "fsqrt 0.0 = 0.0" in {
    val cpu = runCPU(floatProg(
      "fsqrt r1, r0\n", ""))
    cpu.r(1).readf shouldBe 0.0
  }

  "fsqrt 1.0 = 1.0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fsqrt r2, r1\n",
      "a dd 1.0\n"))
    cpu.r(2).readf shouldBe 1.0
  }

  "fsqrt 25.0 = 5.0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fsqrt r2, r1\n",
      "a dd 25.0\n"))
    cpu.r(2).readf shouldBe 5.0
  }

  "fsqrt 0.25 = 0.5" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fsqrt r2, r1\n",
      "a dd 0.25\n"))
    cpu.r(2).readf shouldBe 0.5
  }

  "fsqrt negative yields NaN" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fsqrt r2, r1\n",
      "a dd -4.0\n"))
    cpu.r(2).readf.isNaN shouldBe true
  }

  // ===== FABS (float absolute value) =====

  "fabs positive remains positive" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fabs r2, r1\n",
      "a dd 3.14\n"))
    cpu.r(2).readf shouldBe 3.14
  }

  "fabs negative becomes positive" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fabs r2, r1\n",
      "a dd -7.5\n"))
    cpu.r(2).readf shouldBe 7.5
  }

  "fabs zero remains zero" in {
    val cpu = runCPU(floatProg(
      "fabs r1, r0\n", ""))
    cpu.r(1).readf shouldBe 0.0
  }

  "fabs -0.0 yields 0.0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fabs r2, r1\n",
      "a dd -0.0\n"))
    cpu.r(2).readf shouldBe 0.0
  }

  "fabs large negative" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fabs r2, r1\n",
      "a dd -99999.99\n"))
    cpu.r(2).readf shouldBe 99999.99
  }

  "fabs -1.0 yields 1.0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fabs r2, r1\n",
      "a dd -1.0\n"))
    cpu.r(2).readf shouldBe 1.0
  }

  "fabs already positive large value" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fabs r2, r1\n",
      "a dd 123456.789\n"))
    cpu.r(2).readf shouldBe 123456.789
  }

  // ===== Integration tests =====

  "cvt then fadd: int 3 + float 1.5 = 4.5" in {
    val cpu = runCPU(floatProg(
      "ldi r1, 3\ncvt r1, r1\n" + loadDouble("r2", "a") + "fadd r3, r1, r2\n",
      "a dd 1.5\n"))
    cpu.r(3).readf shouldBe 4.5
  }

  "fsqrt then fpow: sqrt(16) ^ 3 = 64" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fsqrt r2, r1\n" + loadDouble("r3", "b") + "fpow r2, r3\n",
      "a dd 16.0\nb dd 3.0\n"))
    cpu.r(2).readf shouldBe 64.0
  }

  "fabs then fint: abs(-7.9) truncated = 7" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fabs r2, r1\nfint r3, r2\n",
      "a dd -7.9\n"))
    cpu.r(3).read shouldBe 7
  }

  "fslt chained comparison: a < b and b < c" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + loadDouble("r3", "c") +
        "fslt r4, r1, r2\nfslt r5, r2, r3\n",
      "a dd 1.0\nb dd 2.0\nc dd 3.0\n"))
    cpu.r(4).read shouldBe 1
    cpu.r(5).read shouldBe 1
  }

  // ===== FSEQ (float set equal) =====

  "fseq equal values yields 1" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fseq r3, r1, r2\n",
      "a dd 3.14\nb dd 3.14\n"))
    cpu.r(3).read shouldBe 1
  }

  "fseq different values yields 0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fseq r3, r1, r2\n",
      "a dd 1.0\nb dd 2.0\n"))
    cpu.r(3).read shouldBe 0
  }

  "fseq zero and negative zero yields 1" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fseq r2, r0, r1\n",
      "a dd -0.0\n"))
    cpu.r(2).read shouldBe 1
  }

  "fseq NaN != NaN yields 0" in {
    val cpu = runCPU(floatProg(
      "fdiv r1, r0, r0\nfseq r2, r1, r1\n", ""))
    cpu.r(2).read shouldBe 0
  }

  "fseq NaN != value yields 0" in {
    val cpu = runCPU(floatProg(
      "fdiv r1, r0, r0\n" + loadDouble("r2", "a") + "fseq r3, r1, r2\n",
      "a dd 1.0\n"))
    cpu.r(3).read shouldBe 0
  }

  "fseq infinity == infinity yields 1" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + "fdiv r2, r1, r0\nfdiv r3, r1, r0\nfseq r4, r2, r3\n",
      "a dd 1.0\n"))
    cpu.r(4).read shouldBe 1
  }

  "fseq positive != negative yields 0" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fseq r3, r1, r2\n",
      "a dd 5.0\nb dd -5.0\n"))
    cpu.r(3).read shouldBe 0
  }

  "fseq very close values are not equal" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "a") + loadDouble("r2", "b") + "fseq r3, r1, r2\n",
      "a dd 1.0\nb dd 1.0000000000000002\n"))
    cpu.r(3).read shouldBe 0
  }
}
