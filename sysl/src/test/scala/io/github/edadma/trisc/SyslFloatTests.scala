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
        |""".stripMargin) shouldBe "4.0\n"
  }

  "float subtraction" in {
    output(
      """main() -> int
        |    x: double = 5.5
        |    y: double = 2.5
        |    println(x - y)
        |    return 0
        |""".stripMargin) shouldBe "3.0\n"
  }

  "float multiplication" in {
    output(
      """main() -> int
        |    x: double = 3.0
        |    y: double = 4.0
        |    println(x * y)
        |    return 0
        |""".stripMargin) shouldBe "12.0\n"
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
        |""".stripMargin) shouldBe "42.0\n"
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
        |    println(y)
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
        |""".stripMargin) shouldBe "4.0\n"
  }

  "float compound multiply" in {
    output(
      """main() -> int
        |    var x: double = 3.0
        |    x *= 2.0
        |    println(x)
        |    return 0
        |""".stripMargin) shouldBe "6.0\n"
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
        |""".stripMargin) shouldBe "9.0\n"
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
}
