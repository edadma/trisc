package io.github.edadma.trisc

class SyslNumericUnderscoreTests extends SyslTestHelpers {

  // ===== Decimal integer literals =====

  "single underscore separator" in {
    eval("main() -> int = 1_000\n") shouldBe 1000
  }

  "multiple underscore separators" in {
    eval("main() -> int = 1_000_000\n") shouldBe 1000000
  }

  "underscore in small number" in {
    eval("main() -> int = 1_2\n") shouldBe 12
  }

  "consecutive underscores allowed" in {
    eval("main() -> int = 1__000\n") shouldBe 1000
  }

  "many underscores" in {
    eval("main() -> int = 123_456_789\n") shouldBe 123456789
  }

  "underscore with type suffix" in {
    eval(
      """main() -> int
        |    var x: u32 = 1_000_000u32
        |    int(x)
        |""".stripMargin) shouldBe 1000000
  }

  // ===== Hex literals =====

  "underscore in hex literal" in {
    eval("main() -> int = 0xFF_FF\n") shouldBe 0xFFFF
  }

  "underscore grouping in hex" in {
    eval("main() -> int = 0xDEAD_BEEF\n") shouldBe 0xDEADBEEFL
  }

  "byte-grouped hex" in {
    eval("main() -> int = 0xFF_00_FF_00\n") shouldBe 0xFF00FF00L
  }

  "hex with type suffix and underscores" in {
    eval(
      """main() -> int
        |    var x: u8 = 0xF_Fu8
        |    int(x)
        |""".stripMargin) shouldBe 255
  }

  // ===== Float literals =====

  "float with underscore in integer part" in {
    eval(
      """main() -> int
        |    var x: f64 = 1_000.5
        |    int(x)
        |""".stripMargin) shouldBe 1000
  }

  "float with underscore in fractional part" in {
    eval(
      """main() -> int
        |    var x: f64 = 3.141_592
        |    int(x * 1000.0)
        |""".stripMargin) shouldBe 3141
  }

  "float with underscore in both parts" in {
    eval(
      """main() -> int
        |    var x: f64 = 1_000.000_1
        |    int(x)
        |""".stripMargin) shouldBe 1000
  }

  // ===== Exponent =====

  "exponent with underscore" in {
    eval(
      """main() -> int
        |    var x: f64 = 1e1_0
        |    int(x / 1000000000.0)
        |""".stripMargin) shouldBe 10
  }

  // ===== Binary-mask-style hex =====

  "bit mask pattern" in {
    eval("main() -> int = 0x0000_FFFF\n") shouldBe 0xFFFF
  }

  // ===== Does not break regular numbers =====

  "plain integer still works" in {
    eval("main() -> int = 42\n") shouldBe 42
  }

  "plain hex still works" in {
    eval("main() -> int = 0xFF\n") shouldBe 255
  }

  "zero still works" in {
    eval("main() -> int = 0\n") shouldBe 0
  }

  // ===== Usage in larger expressions =====

  "arithmetic with underscores" in {
    eval("main() -> int = 1_000 + 2_000\n") shouldBe 3000
  }

  "underscore numbers in for loop bounds" in {
    eval(
      """main() -> int
        |    count = 0
        |    for i in 1..1_000
        |        count += 1
        |    count
        |""".stripMargin) shouldBe 1000
  }
}
