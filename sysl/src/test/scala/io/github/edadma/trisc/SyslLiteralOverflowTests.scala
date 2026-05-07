package io.github.edadma.trisc

/** Audit item #28 (Tier 4): the analyzer used to silently truncate
 *  out-of-range integer literals at the coercion site. `var x: u8 = 256`
 *  became `0`, `var x: i8 = 200` became `-56`, and so on — bug-prone
 *  whenever a developer mistypes a constant.
 *
 *  The analyzer now rejects such literals with a clear range-violation
 *  diagnostic that names the type, the legal range, and how to opt into
 *  truncation explicitly via a cast (`u8(value)`, `int(value)`, etc.).
 *
 *  These tests pin both directions: known-good values still accepted,
 *  known-bad values rejected with the new wording.
 */
class SyslLiteralOverflowTests extends SyslTestHelpers {

  // ===== u8 =====

  "u8 = 0 is accepted" in {
    eval(
      """main() -> int
        |    var x: u8 = 0
        |    int(x)
        |""".stripMargin) shouldBe 0
  }

  "u8 = 255 is accepted (boundary)" in {
    eval(
      """main() -> int
        |    var x: u8 = 255
        |    int(x)
        |""".stripMargin) shouldBe 255
  }

  "u8 = 256 is rejected (out of range above)" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var x: u8 = 256
          |    int(x)
          |""".stripMargin)
    }
    thrown.getMessage should include("256")
    thrown.getMessage should include("u8")
    thrown.getMessage should include("0..255")
    thrown.getMessage should include("u8(...)")
  }

  "u8 = 1000 is rejected" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var x: u8 = 1000
          |    int(x)
          |""".stripMargin)
    }
    thrown.getMessage should include("1000")
    thrown.getMessage should include("u8")
  }

  // ===== i8 =====

  "i8 = 127 is accepted (max)" in {
    eval(
      """main() -> int
        |    var x: i8 = 127
        |    int(x)
        |""".stripMargin) shouldBe 127
  }

  "i8 = 128 is rejected (one above max)" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var x: i8 = 128
          |    int(x)
          |""".stripMargin)
    }
    thrown.getMessage should include("128")
    thrown.getMessage should include("i8")
    thrown.getMessage should include("-128..127")
  }

  // ===== u16 =====

  "u16 = 65535 is accepted" in {
    eval(
      """main() -> int
        |    var x: u16 = 65535
        |    int(x)
        |""".stripMargin) shouldBe 65535
  }

  "u16 = 65536 is rejected" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var x: u16 = 65536
          |    int(x)
          |""".stripMargin)
    }
    thrown.getMessage should include("65536")
    thrown.getMessage should include("u16")
    thrown.getMessage should include("0..65535")
  }

  // ===== i32 / int =====

  "int max is accepted" in {
    eval(
      """main() -> int
        |    var x: int = 2147483647
        |    x
        |""".stripMargin) shouldBe 2147483647
  }

  "int min is accepted" in {
    eval(
      """main() -> int
        |    var x: int = -2147483648
        |    x
        |""".stripMargin) shouldBe -2147483648
  }

  "int = 2147483648 is rejected (one above max)" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var x: int = 2147483648
          |    int(x)
          |""".stripMargin)
    }
    thrown.getMessage should include("2147483648")
    thrown.getMessage should include("int")
  }

  // ===== u32 =====

  "u32 max is accepted" in {
    // Compare via i64 — `int(x)` would silently sign-extend a u32 max to -1,
    // which is unrelated to the literal-acceptance check.
    eval(
      """main() -> int
        |    var x: u32 = 4294967295
        |    if i64(x) == 4294967295i64 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "u32 = 4294967296 is rejected" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var x: u32 = 4294967296
          |    int(x)
          |""".stripMargin)
    }
    thrown.getMessage should include("4294967296")
    thrown.getMessage should include("u32")
  }

  // ===== i64 / u64 — all Long values fit; check no false rejection =====

  "i64 large literal is accepted" in {
    eval(
      """main() -> int
        |    var x: i64 = 1000000000000i64
        |    int(x / 1000000000000i64)
        |""".stripMargin) shouldBe 1
  }

  // ===== Cast suggestion is syntactically valid =====
  //
  // The error says "cast explicitly: `u8(...)`" — verify that following
  // the suggestion actually compiles and produces the truncated value.

  "cast suggestion for u8 = 256 truncates correctly" in {
    eval(
      """main() -> int
        |    var x: u8 = u8(256)
        |    int(x)
        |""".stripMargin) shouldBe 0  // 256 mod 256 = 0
  }

  "cast suggestion for u8 = 257 truncates correctly" in {
    eval(
      """main() -> int
        |    var x: u8 = u8(257)
        |    int(x)
        |""".stripMargin) shouldBe 1  // 257 mod 256 = 1
  }

  // ===== const decls also covered =====

  "const u8 = 300 is rejected" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """const FLAG: u8 = 300
          |main() -> int
          |    int(FLAG)
          |""".stripMargin)
    }
    thrown.getMessage should include("300")
    thrown.getMessage should include("u8")
  }

  // ===== module-level vars also covered =====

  "module-level val u16 = 70000 is rejected" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """val LIMIT: u16 = 70000
          |main() -> int
          |    int(LIMIT)
          |""".stripMargin)
    }
    thrown.getMessage should include("70000")
    thrown.getMessage should include("u16")
  }
}
