package io.github.edadma.trisc

class SyslFloatTests extends SyslTestHelpers {

  // ===== Float literals =====

  "float variable" in {
    output(
      """main() -> int
        |    x: double = 2.5
        |    println(x)
        |    return 0
        |""".stripMargin) shouldBe "2.5\n"
  }

  // ===== Float arithmetic =====

  "float addition" in {
    output(
      """main() -> int
        |    x: double = 1.5
        |    y: double = 2.5
        |    println(x + y)
        |    return 0
        |""".stripMargin) shouldBe "4\n"
  }

  "float subtraction" in {
    output(
      """main() -> int
        |    x: double = 5.5
        |    y: double = 2.5
        |    println(x - y)
        |    return 0
        |""".stripMargin) shouldBe "3\n"
  }

  "float multiplication" in {
    output(
      """main() -> int
        |    x: double = 3.0
        |    y: double = 4.0
        |    println(x * y)
        |    return 0
        |""".stripMargin) shouldBe "12\n"
  }

  "float division" in {
    output(
      """main() -> int
        |    x: double = 10.0
        |    y: double = 4.0
        |    println(x / y)
        |    return 0
        |""".stripMargin) shouldBe "2.5\n"
  }

  // ===== Float negation =====

  "float negation" in {
    output(
      """main() -> int
        |    x: double = 3.14
        |    println(-x)
        |    return 0
        |""".stripMargin) shouldBe "-3.14\n"
  }

  // ===== Float comparisons =====

  "float less than true" in {
    output(
      """main() -> int
        |    x: double = 1.0
        |    y: double = 2.0
        |    if x < y
        |        println(1)
        |    else
        |        println(0)
        |    return 0
        |""".stripMargin) shouldBe "1\n"
  }

  "float less than false" in {
    output(
      """main() -> int
        |    x: double = 3.0
        |    y: double = 2.0
        |    if x < y
        |        println(1)
        |    else
        |        println(0)
        |    return 0
        |""".stripMargin) shouldBe "0\n"
  }

  "float equal true" in {
    output(
      """main() -> int
        |    x: double = 3.14
        |    y: double = 3.14
        |    if x == y
        |        println(1)
        |    else
        |        println(0)
        |    return 0
        |""".stripMargin) shouldBe "1\n"
  }

  "float equal false" in {
    output(
      """main() -> int
        |    x: double = 3.14
        |    y: double = 2.71
        |    if x == y
        |        println(1)
        |    else
        |        println(0)
        |    return 0
        |""".stripMargin) shouldBe "0\n"
  }

  "float greater than" in {
    output(
      """main() -> int
        |    x: double = 5.0
        |    y: double = 3.0
        |    if x > y
        |        println(1)
        |    else
        |        println(0)
        |    return 0
        |""".stripMargin) shouldBe "1\n"
  }

  "float not equal" in {
    output(
      """main() -> int
        |    x: double = 1.0
        |    y: double = 2.0
        |    if x != y
        |        println(1)
        |    else
        |        println(0)
        |    return 0
        |""".stripMargin) shouldBe "1\n"
  }

  // ===== Int-float promotion =====

  "int + float promotes to float" in {
    output(
      """main() -> int
        |    x: int = 3
        |    y: double = 0.14
        |    println(x + y)
        |    return 0
        |""".stripMargin) shouldBe "3.14\n"
  }

  "float + int promotes to float" in {
    output(
      """main() -> int
        |    x: double = 0.5
        |    y: int = 2
        |    println(x + y)
        |    return 0
        |""".stripMargin) shouldBe "2.5\n"
  }

  // ===== Casts =====

  "cast int to double" in {
    output(
      """main() -> int
        |    x: int = 42
        |    y: double = double(x)
        |    println(y)
        |    return 0
        |""".stripMargin) shouldBe "42\n"
  }

  "cast double to int truncates" in {
    output(
      """main() -> int
        |    x: double = 7.9
        |    y: int = int(x)
        |    println(y)
        |    return 0
        |""".stripMargin) shouldBe "7\n"
  }

  "cast negative double to i64 truncates toward zero" in {
    output(
      """main() -> int
        |    x: double = -3.7
        |    y: i64 = i64(x)
        |    println(int(y))
        |    return 0
        |""".stripMargin) shouldBe "-3\n"
  }

  // ===== Compound assignment =====

  "float compound add" in {
    output(
      """main() -> int
        |    var x: double = 1.5
        |    x += 2.5
        |    println(x)
        |    return 0
        |""".stripMargin) shouldBe "4\n"
  }

  "float compound multiply" in {
    output(
      """main() -> int
        |    var x: double = 3.0
        |    x *= 2.0
        |    println(x)
        |    return 0
        |""".stripMargin) shouldBe "6\n"
  }

  // ===== Float in loops =====

  "float accumulator in while loop" in {
    output(
      """main() -> int
        |    var sum: double = 0.0
        |    var i: int = 0
        |    while i < 5
        |        sum += 1.5
        |        i += 1
        |    println(sum)
        |    return 0
        |""".stripMargin) shouldBe "7.5\n"
  }

  // ===== Float function parameters =====

  "float function parameter" in {
    output(
      """square(x: double) -> double = x * x
        |
        |main() -> int
        |    println(square(3.0))
        |    return 0
        |""".stripMargin) shouldBe "9\n"
  }

  "float function return" in {
    output(
      """half(x: double) -> double = x / 2.0
        |
        |main() -> int
        |    println(half(7.0))
        |    return 0
        |""".stripMargin) shouldBe "3.5\n"
  }

  // ===== Expression body returning float =====

  "expression body returns float" in {
    output(
      """area(r: double) -> double = 3.14159 * r * r
        |
        |main() -> int
        |    println(area(1.0))
        |    return 0
        |""".stripMargin) shouldBe "3.14159\n"
  }

  // ===== f32 (single precision) — interpreter =====

  "f32 variable holds value" in {
    eval(
      """main() -> int
        |    x: f32 = 2.5
        |    int(x * 2.0)
        |""".stripMargin) shouldBe 5
  }

  "float alias resolves to f32" in {
    eval(
      """main() -> int
        |    x: float = 1.5
        |    y: f32 = x
        |    int(y * 4.0)
        |""".stripMargin) shouldBe 6
  }

  "double alias resolves to f64" in {
    eval(
      """main() -> int
        |    x: double = 1.5
        |    y: f64 = x
        |    int(y * 4.0)
        |""".stripMargin) shouldBe 6
  }

  "f32 widens to f64 implicitly" in {
    eval(
      """main() -> int
        |    a: f32 = 1.5
        |    b: f64 = a
        |    int(b * 4.0)
        |""".stripMargin) shouldBe 6
  }

  "f64 to f32 narrowing requires explicit cast" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    a: f64 = 1.5
        |    b: f32 = a
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "f64 to f32 with explicit cast works" in {
    eval(
      """main() -> int
        |    a: f64 = 1.5
        |    b: f32 = f32(a)
        |    int(b * 4.0)
        |""".stripMargin) shouldBe 6
  }

  "sizeof f32 is 4" in {
    eval("main() -> int = sizeof(f32)\n") shouldBe 4
  }

  "sizeof float equals sizeof f32" in {
    eval("main() -> int = sizeof(float) == sizeof(f32)\n") shouldBe 1
  }

  "sizeof f64 is 8" in {
    eval("main() -> int = sizeof(f64)\n") shouldBe 8
  }

  "sizeof double equals sizeof f64" in {
    eval("main() -> int = sizeof(double) == sizeof(f64)\n") shouldBe 1
  }

  "f32 in struct has 4-byte field" in {
    eval(
      """struct Pair
        |    a: f32
        |    b: f32
        |
        |main() -> int = sizeof(Pair)
        |""".stripMargin) shouldBe 8
  }

  "i16 to f32 conversion is allowed" in {
    eval(
      """main() -> int
        |    n: i16 = 7
        |    x: f32 = n
        |    int(x)
        |""".stripMargin) shouldBe 7
  }

  // ===== Prefix round-trip =====

  "f32 prefix round-trip" in {
    SyslType.fromPrefix("f32") shouldBe SyslType.FloatType(32)
  }

  "f64 prefix round-trip" in {
    SyslType.fromPrefix("f64") shouldBe SyslType.FloatType(64)
  }

  "double prefix maps to f64" in {
    SyslType.fromPrefix("double") shouldBe SyslType.FloatType(64)
  }

  "float prefix maps to f32" in {
    SyslType.fromPrefix("float") shouldBe SyslType.FloatType(32)
  }
}
