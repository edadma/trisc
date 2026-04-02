package io.github.edadma.trisc

class SyslLibMathTests extends SyslTestHelpers {

  val libs: Map[String, String] = Map("math" -> readSysl("posix/lib/math.sysl"))

  private def evalWith(main: String): Long = evalWithLibs(libs,
    s"""import math.*
       |$main
       |""".stripMargin)

  // ===== abs =====

  "abs positive" in { evalWith("main() -> int = abs(42)") shouldBe 42 }
  "abs negative" in { evalWith("main() -> int = abs(-7)") shouldBe 7 }
  "abs zero" in { evalWith("main() -> int = abs(0)") shouldBe 0 }

  // ===== min =====

  "min first smaller" in { evalWith("main() -> int = min(3, 7)") shouldBe 3 }
  "min second smaller" in { evalWith("main() -> int = min(10, 2)") shouldBe 2 }
  "min equal" in { evalWith("main() -> int = min(5, 5)") shouldBe 5 }
  "min with negatives" in { evalWith("main() -> int = min(-3, -7)") shouldBe -7 }

  // ===== max =====

  "max first larger" in { evalWith("main() -> int = max(10, 3)") shouldBe 10 }
  "max second larger" in { evalWith("main() -> int = max(2, 8)") shouldBe 8 }
  "max equal" in { evalWith("main() -> int = max(5, 5)") shouldBe 5 }

  // ===== clamp =====

  "clamp within range" in { evalWith("main() -> int = clamp(5, 0, 10)") shouldBe 5 }
  "clamp below" in { evalWith("main() -> int = clamp(-5, 0, 10)") shouldBe 0 }
  "clamp above" in { evalWith("main() -> int = clamp(15, 0, 10)") shouldBe 10 }

  // ===== sign =====

  "sign positive" in { evalWith("main() -> int = sign(42)") shouldBe 1 }
  "sign negative" in { evalWith("main() -> int = sign(-99)") shouldBe -1 }
  "sign zero" in { evalWith("main() -> int = sign(0)") shouldBe 0 }

  // ===== gcd =====

  "gcd basic" in { evalWith("main() -> int = gcd(12, 8)") shouldBe 4 }
  "gcd coprime" in { evalWith("main() -> int = gcd(7, 13)") shouldBe 1 }
  "gcd with zero" in { evalWith("main() -> int = gcd(15, 0)") shouldBe 15 }
  "gcd with negatives" in { evalWith("main() -> int = gcd(-12, 8)") shouldBe 4 }

  // ===== pow =====

  "pow basic" in { evalWith("main() -> int = pow(2, 10)") shouldBe 1024 }
  "pow zero exponent" in { evalWith("main() -> int = pow(5, 0)") shouldBe 1 }
  "pow one exponent" in { evalWith("main() -> int = pow(7, 1)") shouldBe 7 }
  "pow zero base" in { evalWith("main() -> int = pow(0, 5)") shouldBe 0 }
}
