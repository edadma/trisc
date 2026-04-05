package io.github.edadma.trisc

class SyslCodegenFloatTests extends SyslCodegenHelpers {

  // ===== Float literals =====

  "float literal via ldc" in {
    compileAndRun(
      """main() -> int
        |    x: f64 = 5.0
        |    int(x)
        |""".stripMargin) shouldBe 5
  }

  "negative float literal" in {
    compileAndRun(
      """main() -> int
        |    x: f64 = -3.0
        |    int(x)
        |""".stripMargin) shouldBe -3
  }

  "float zero" in {
    compileAndRun(
      """main() -> int
        |    x: f64 = 0.0
        |    int(x)
        |""".stripMargin) shouldBe 0
  }

  // ===== Float arithmetic =====

  "float addition" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 3.0
        |    b: f64 = 4.0
        |    int(a + b)
        |""".stripMargin) shouldBe 7
  }

  "float subtraction" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 10.0
        |    b: f64 = 3.0
        |    int(a - b)
        |""".stripMargin) shouldBe 7
  }

  "float multiplication" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 6.0
        |    b: f64 = 7.0
        |    int(a * b)
        |""".stripMargin) shouldBe 42
  }

  "float division" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 10.0
        |    b: f64 = 3.0
        |    int(a / b)
        |""".stripMargin) shouldBe 3
  }

  "float division exact" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 15.0
        |    b: f64 = 5.0
        |    int(a / b)
        |""".stripMargin) shouldBe 3
  }

  "float negation" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 5.0
        |    int(-a)
        |""".stripMargin) shouldBe -5
  }

  "float compound expression" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 2.0
        |    b: f64 = 3.0
        |    c: f64 = 4.0
        |    int(a * b + c)
        |""".stripMargin) shouldBe 10
  }

  // ===== Float comparisons =====

  "float equal true" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 3.14
        |    b: f64 = 3.14
        |    if a == b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "float equal false" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 3.14
        |    b: f64 = 2.71
        |    if a == b then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "float not equal" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 1.0
        |    b: f64 = 2.0
        |    if a != b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "float less than true" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 1.0
        |    b: f64 = 2.0
        |    if a < b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "float less than false" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 3.0
        |    b: f64 = 2.0
        |    if a < b then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "float greater than" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 5.0
        |    b: f64 = 3.0
        |    if a > b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "float less than or equal" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 3.0
        |    b: f64 = 3.0
        |    if a <= b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "float greater than or equal" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = 3.0
        |    b: f64 = 5.0
        |    if a >= b then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  // ===== Float ↔ int casts =====

  "int to f64 roundtrip" in {
    compileAndRun(
      """main() -> int
        |    x = 42
        |    y: f64 = f64(x)
        |    int(y)
        |""".stripMargin) shouldBe 42
  }

  "f64 to int truncates toward zero positive" in {
    compileAndRun(
      """main() -> int
        |    x: f64 = 3.7
        |    int(x)
        |""".stripMargin) shouldBe 3
  }

  "f64 to int truncates toward zero negative" in {
    compileAndRun(
      """main() -> int
        |    x: f64 = -2.9
        |    int(x)
        |""".stripMargin) shouldBe -2
  }

  "f64 to int large value" in {
    compileAndRun(
      """main() -> int
        |    x: f64 = 1000.99
        |    int(x)
        |""".stripMargin) shouldBe 1000
  }

  "f64 cast in arithmetic" in {
    compileAndRun(
      """main() -> int
        |    a: f64 = f64(10)
        |    b: f64 = f64(3)
        |    int(a / b)
        |""".stripMargin) shouldBe 3
  }

  // ===== Float in functions =====

  "float as function argument" in {
    compileAndRun(
      """to_int(x: f64) -> int = int(x)
        |main() -> int = to_int(7.5)
        |""".stripMargin) shouldBe 7
  }

  "float as return value" in {
    compileAndRun(
      """half(x: int) -> f64
        |    f64(x) / 2.0
        |
        |main() -> int = int(half(10))
        |""".stripMargin) shouldBe 5
  }

  "float in loop accumulation" in {
    compileAndRun(
      """main() -> int
        |    var sum: f64 = 0.0
        |    var i = 0
        |    while i < 5
        |        sum = sum + 1.5
        |        i += 1
        |    int(sum)
        |""".stripMargin) shouldBe 7
  }
}
