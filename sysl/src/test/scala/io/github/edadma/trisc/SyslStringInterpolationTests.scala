package io.github.edadma.trisc

class SyslStringInterpolationTests extends SyslTestHelpers {

  // ===== Basic $name interpolation =====

  "interpolate variable with $" in {
    output(
      "main() -> int\n    x = 42\n    puts(s\"$x\")\n    0\n") shouldBe "42"
  }

  "interpolate in middle of string" in {
    output(
      "main() -> int\n    x = 5\n    puts(s\"value is $x ok\")\n    0\n") shouldBe "value is 5 ok"
  }

  "multiple interpolations" in {
    output(
      "main() -> int\n    a = 10\n    b = 20\n    puts(s\"$a and $b\")\n    0\n") shouldBe "10 and 20"
  }

  "interpolation at start" in {
    output(
      "main() -> int\n    x = 7\n    puts(s\"$x hello\")\n    0\n") shouldBe "7 hello"
  }

  "interpolation at end" in {
    output(
      "main() -> int\n    x = 99\n    puts(s\"value: $x\")\n    0\n") shouldBe "value: 99"
  }

  // ===== ${expr} interpolation =====

  "interpolate expression with braces" in {
    output(
      "main() -> int\n    x = 3\n    puts(s\"${x + 1}\")\n    0\n") shouldBe "4"
  }

  "interpolate function call" in {
    output(
      "dbl(x: int) -> int = x * 2\nmain() -> int\n    puts(s\"${dbl(5)}\")\n    0\n") shouldBe "10"
  }

  // ===== $$ escape =====

  "dollar escape" in {
    output(
      "main() -> int\n    puts(s\"cost is $$5\")\n    0\n") shouldBe "cost is $5"
  }

  // ===== str() builtin =====

  "str of integer" in {
    output(
      """main() -> int
        |    s: string = str(42)
        |    puts(s)
        |    0
        |""".stripMargin) shouldBe "42"
  }

  "str of zero" in {
    output(
      """main() -> int
        |    puts(str(0))
        |    0
        |""".stripMargin) shouldBe "0"
  }

  "str of negative" in {
    output(
      """main() -> int
        |    puts(str(-123))
        |    0
        |""".stripMargin) shouldBe "-123"
  }

  "str of string is identity" in {
    output(
      """main() -> int
        |    s: string = "hello"
        |    puts(str(s))
        |    0
        |""".stripMargin) shouldBe "hello"
  }

  // ===== Interpolation with string variables =====

  "interpolate string variable" in {
    output(
      "main() -> int\n    name: string = \"world\"\n    puts(s\"hello $name\")\n    0\n") shouldBe "hello world"
  }

  // ===== Plain strings unaffected =====

  "plain string with dollar is not interpolated" in {
    output(
      """main() -> int
        |    puts("$5")
        |    0
        |""".stripMargin) shouldBe "$5"
  }

  "s as variable name still works" in {
    eval(
      """main() -> int
        |    s = 42
        |    s
        |""".stripMargin) shouldBe 42
  }

  // ===== Length of interpolated string =====

  "length of interpolated string" in {
    eval(
      "main() -> int\n    x = 42\n    s: string = s\"val=$x\"\n    len(s)\n") shouldBe 6
  }

  // ===== Interpolation with negative numbers =====

  "interpolate negative" in {
    output(
      "main() -> int\n    x = -5\n    puts(s\"x is $x\")\n    0\n") shouldBe "x is -5"
  }
}
