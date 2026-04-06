package io.github.edadma.trisc

class SyslCodegenMathHwTests extends SyslCodegenHelpers {

  private val mathSource = scala.io.Source.fromFile("posix/math/math.sysl").mkString

  private def mathSources(mainSource: String): Map[String, String] =
    Map(
      "posix/math/math" -> mathSource,
      "main" -> mainSource,
    )

  // ===== fabs =====

  "fabs positive" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(fabs(5.0))
        |""".stripMargin)) shouldBe 5
  }

  "fabs negative" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(fabs(-5.0))
        |""".stripMargin)) shouldBe 5
  }

  "fabs zero" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(fabs(0.0))
        |""".stripMargin)) shouldBe 0
  }

  // ===== sqrt =====

  "sqrt 4" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(sqrt(4.0))
        |""".stripMargin)) shouldBe 2
  }

  "sqrt 9" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(sqrt(9.0))
        |""".stripMargin)) shouldBe 3
  }

  "sqrt 1" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(sqrt(1.0))
        |""".stripMargin)) shouldBe 1
  }

  // ===== sin / cos / tan =====

  "sin 0" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(sin(0.0))
        |""".stripMargin)) shouldBe 0
  }

  "cos 0" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(cos(0.0))
        |""".stripMargin)) shouldBe 1
  }

  "sin pi/2 is approximately 1" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(round(sin(PI / 2.0)))
        |""".stripMargin)) shouldBe 1
  }

  "cos pi is approximately -1" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(round(cos(PI)))
        |""".stripMargin)) shouldBe -1
  }

  "tan 0" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(tan(0.0))
        |""".stripMargin)) shouldBe 0
  }

  // ===== asin / acos / atan =====

  "asin 0" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(asin(0.0))
        |""".stripMargin)) shouldBe 0
  }

  "acos 1" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(acos(1.0))
        |""".stripMargin)) shouldBe 0
  }

  "atan 0" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(atan(0.0))
        |""".stripMargin)) shouldBe 0
  }

  "asin 1 is approximately pi/2" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int
        |    val result = asin(1.0)
        |    if result > 1.57 && result < 1.58 then 1
        |    else 0
        |""".stripMargin)) shouldBe 1
  }

  // ===== exp / log =====

  "exp 0" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(exp(0.0))
        |""".stripMargin)) shouldBe 1
  }

  "exp 1 is approximately e" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(round(exp(1.0)))
        |""".stripMargin)) shouldBe 3
  }

  "log 1" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(log(1.0))
        |""".stripMargin)) shouldBe 0
  }

  "log e is approximately 1" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(round(log(E)))
        |""".stripMargin)) shouldBe 1
  }

  // ===== fpow =====

  "fpow 2^3" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(fpow(2.0, 3.0))
        |""".stripMargin)) shouldBe 8
  }

  "fpow 3^2" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(fpow(3.0, 2.0))
        |""".stripMargin)) shouldBe 9
  }

  "fpow x^0" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(fpow(5.0, 0.0))
        |""".stripMargin)) shouldBe 1
  }

  "fpow x^1" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(fpow(7.0, 1.0))
        |""".stripMargin)) shouldBe 7
  }

  // ===== atan2 =====

  "atan2 0 1" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(atan2(0.0, 1.0))
        |""".stripMargin)) shouldBe 0
  }

  "atan2 1 0 is approximately pi/2" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int
        |    val result = atan2(1.0, 0.0)
        |    if result > 1.57 && result < 1.58 then 1
        |    else 0
        |""".stripMargin)) shouldBe 1
  }

  // ===== derived functions =====

  "exp2 3" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(round(exp2(3.0)))
        |""".stripMargin)) shouldBe 8
  }

  "log2 8" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(log2(8.0))
        |""".stripMargin)) shouldBe 3
  }

  "log10 100" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(log10(100.0))
        |""".stripMargin)) shouldBe 2
  }

  // ===== hyperbolic =====

  "sinh 0" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(sinh(0.0))
        |""".stripMargin)) shouldBe 0
  }

  "cosh 0" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(cosh(0.0))
        |""".stripMargin)) shouldBe 1
  }

  "tanh 0" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(tanh(0.0))
        |""".stripMargin)) shouldBe 0
  }

  // ===== floor / ceil / round / trunc =====

  "floor 2.7" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(floor(2.7))
        |""".stripMargin)) shouldBe 2
  }

  "floor -2.3" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(floor(-2.3))
        |""".stripMargin)) shouldBe -3
  }

  "ceil 2.3" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(ceil(2.3))
        |""".stripMargin)) shouldBe 3
  }

  "ceil -2.7" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(ceil(-2.7))
        |""".stripMargin)) shouldBe -2
  }

  "round 2.5" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(round(2.5))
        |""".stripMargin)) shouldBe 3
  }

  "round -2.5" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(round(-2.5))
        |""".stripMargin)) shouldBe -3
  }

  "trunc 2.9" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(trunc(2.9))
        |""".stripMargin)) shouldBe 2
  }

  "trunc -2.9" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(trunc(-2.9))
        |""".stripMargin)) shouldBe -2
  }

  // ===== fmod / copysign =====

  "fmod 7 3" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(fmod(7.0, 3.0))
        |""".stripMargin)) shouldBe 1
  }

  "copysign positive from negative" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(copysign(5.0, -1.0))
        |""".stripMargin)) shouldBe -5
  }

  "copysign negative from positive" in {
    compileMultiAndRun(mathSources(
      """import posix.math.*
        |main() -> int = int(copysign(-5.0, 1.0))
        |""".stripMargin)) shouldBe 5
  }
}
