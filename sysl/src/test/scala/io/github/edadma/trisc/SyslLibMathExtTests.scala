package io.github.edadma.trisc

class SyslLibMathExtTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map(
    "posix/math/math" -> readSysl("posix/math/math.sysl"),
  )

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import posix.math.*
       |$main
       |""".stripMargin)

  // ===== isnan =====

  "isnan on normal" in { evalWith("main() -> int = isnan(1.0)") shouldBe 0 }
  "isnan on zero" in { evalWith("main() -> int = isnan(0.0)") shouldBe 0 }
  "isnan on NaN" in { evalWith("main() -> int = isnan(0.0 / 0.0)") shouldBe 1 }

  // ===== isinf =====

  "isinf on normal" in { evalWith("main() -> int = isinf(1.0)") shouldBe 0 }
  "isinf on zero" in { evalWith("main() -> int = isinf(0.0)") shouldBe 0 }
  "isinf on positive inf" in { evalWith("main() -> int = isinf(1.0e308 * 10.0)") shouldBe 1 }
  "isinf on negative inf" in { evalWith("main() -> int = isinf(-1.0e308 * 10.0)") shouldBe 1 }

  // ===== isfinite =====

  "isfinite on normal" in { evalWith("main() -> int = isfinite(42.0)") shouldBe 1 }
  "isfinite on zero" in { evalWith("main() -> int = isfinite(0.0)") shouldBe 1 }
  "isfinite on inf" in { evalWith("main() -> int = isfinite(1.0e308 * 10.0)") shouldBe 0 }
  "isfinite on NaN" in { evalWith("main() -> int = isfinite(0.0 / 0.0)") shouldBe 0 }

  // ===== signbit =====

  "signbit positive" in { evalWith("main() -> int = signbit(1.0)") shouldBe 0 }
  "signbit negative" in { evalWith("main() -> int = signbit(-1.0)") shouldBe 1 }
  "signbit zero" in { evalWith("main() -> int = signbit(0.0)") shouldBe 0 }

  // ===== constants =====

  "E is approximately 2.718" in {
    evalWith(
      """main() -> int
        |    if E > 2.718 && E < 2.719 then 1
        |    else 0
        |""".stripMargin) shouldBe 1
  }

  "HUGE_VAL is large" in { evalWith("main() -> int = if HUGE_VAL > 1.0e307 then 1 else 0") shouldBe 1 }
}
