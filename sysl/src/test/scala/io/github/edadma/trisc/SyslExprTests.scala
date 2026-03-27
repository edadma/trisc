package io.github.edadma.trisc

class SyslExprTests extends SyslTestHelpers {

  // ===== Arithmetic =====

  "addition" in {
    eval("main() -> int = 3 + 4\n") shouldBe 7
  }

  "subtraction" in {
    eval("main() -> int = 10 - 3\n") shouldBe 7
  }

  "multiplication" in {
    eval("main() -> int = 6 * 7\n") shouldBe 42
  }

  "division" in {
    eval("main() -> int = 42 / 6\n") shouldBe 7
  }

  "modulo" in {
    eval("main() -> int = 17 % 5\n") shouldBe 2
  }

  "operator precedence" in {
    eval("main() -> int = 2 + 3 * 4\n") shouldBe 14
  }

  "parentheses" in {
    eval("main() -> int = (2 + 3) * 4\n") shouldBe 20
  }

  "unary minus" in {
    eval("main() -> int = -42\n") shouldBe -42
  }

  // ===== Comparison =====

  "less than true" in {
    eval("main() -> int = 3 < 5\n") shouldBe 1
  }

  "less than false" in {
    eval("main() -> int = 5 < 3\n") shouldBe 0
  }

  "equal true" in {
    eval("main() -> int = 5 == 5\n") shouldBe 1
  }

  "not equal" in {
    eval("main() -> int = 5 != 3\n") shouldBe 1
  }

  // ===== Logical =====

  "logical and" in {
    eval("main() -> int = true && true\n") shouldBe 1
  }

  "logical and short-circuit" in {
    eval("main() -> int = false && true\n") shouldBe 0
  }

  "logical or" in {
    eval("main() -> int = false || true\n") shouldBe 1
  }

  "logical not" in {
    eval("main() -> int = !false\n") shouldBe 1
  }

  // ===== Chained comparisons =====

  "chained comparison lower <= x <= upper" in {
    eval(
      """main() -> int
        |    x = 5
        |    if 1 <= x <= 10 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "chained comparison out of range" in {
    eval(
      """main() -> int
        |    x = 15
        |    if 1 <= x <= 10 then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "chained comparison three operators" in {
    eval(
      """main() -> int
        |    if 1 < 2 < 3 < 4 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "chained comparison three operators fails" in {
    eval(
      """main() -> int
        |    if 1 < 2 < 3 < 2 then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "chained comparison mixed operators" in {
    eval(
      """main() -> int
        |    x = 5
        |    if 0 < x <= 5 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "chained comparison with == and !=" in {
    eval("main() -> int = if 5 == 5 != 0 then 1 else 0\n") shouldBe 1
  }

  "single comparison still works" in {
    eval("main() -> int = if 3 < 5 then 1 else 0\n") shouldBe 1
  }

  // ===== Boolean literals =====

  "true literal" in {
    eval("main() -> int = if true then 1 else 0\n") shouldBe 1
  }

  "false literal" in {
    eval("main() -> int = if false then 1 else 0\n") shouldBe 0
  }

  // ===== Unary precedence =====

  "unary minus with multiplication" in {
    eval("main() -> int = -3 * 2\n") shouldBe -6
  }

  "unary not with comparison" in {
    eval("main() -> int = if !(3 > 5) then 1 else 0\n") shouldBe 1
  }

  // ===== Increment/decrement =====

  "prefix increment" in {
    eval(
      """main() -> int
        |    x = 5
        |    ++x
        |""".stripMargin) shouldBe 6
  }

  "prefix increment returns new value" in {
    eval(
      """main() -> int
        |    x = 5
        |    y = ++x
        |    y
        |""".stripMargin) shouldBe 6
  }

  "prefix decrement" in {
    eval(
      """main() -> int
        |    x = 5
        |    --x
        |""".stripMargin) shouldBe 4
  }

  "postfix increment returns old value" in {
    eval(
      """main() -> int
        |    x = 5
        |    y = x++
        |    y
        |""".stripMargin) shouldBe 5
  }

  "postfix increment modifies variable" in {
    eval(
      """main() -> int
        |    x = 5
        |    x++
        |    x
        |""".stripMargin) shouldBe 6
  }

  "postfix decrement returns old value" in {
    eval(
      """main() -> int
        |    x = 5
        |    y = x--
        |    y
        |""".stripMargin) shouldBe 5
  }

  "postfix decrement modifies variable" in {
    eval(
      """main() -> int
        |    x = 5
        |    x--
        |    x
        |""".stripMargin) shouldBe 4
  }

  "increment in while loop" in {
    eval(
      """main() -> int
        |    i = 0
        |    sum = 0
        |    while i < 5
        |        sum = sum + i
        |        ++i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "postfix in expression" in {
    eval(
      """main() -> int
        |    x = 5
        |    x++ + 10
        |""".stripMargin) shouldBe 15
  }

  "prefix in expression" in {
    eval(
      """main() -> int
        |    x = 5
        |    ++x + 10
        |""".stripMargin) shouldBe 16
  }

  "while with do and postfix increment" in {
    eval(
      """main() -> int
        |    i = 0
        |    while i < 5 do i++
        |    i
        |""".stripMargin) shouldBe 5
  }

  // ===== Compound assignment =====

  "+= basic" in {
    eval(
      """main() -> int
        |    x = 10
        |    x += 5
        |    x
        |""".stripMargin) shouldBe 15
  }

  "-= basic" in {
    eval(
      """main() -> int
        |    x = 10
        |    x -= 3
        |    x
        |""".stripMargin) shouldBe 7
  }

  "*= basic" in {
    eval(
      """main() -> int
        |    x = 6
        |    x *= 7
        |    x
        |""".stripMargin) shouldBe 42
  }

  "/= basic" in {
    eval(
      """main() -> int
        |    x = 42
        |    x /= 6
        |    x
        |""".stripMargin) shouldBe 7
  }

  "%= basic" in {
    eval(
      """main() -> int
        |    x = 17
        |    x %= 5
        |    x
        |""".stripMargin) shouldBe 2
  }

  "+= in while loop" in {
    eval(
      """main() -> int
        |    sum = 0
        |    i = 0
        |    while i < 5
        |        sum += i
        |        i += 1
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "+= with expression" in {
    eval(
      """main() -> int
        |    x = 10
        |    y = 5
        |    x += y * 2
        |    x
        |""".stripMargin) shouldBe 20
  }

  "+= inline with while do" in {
    eval(
      """main() -> int
        |    i = 0
        |    while i < 10 do i += 1
        |    i
        |""".stripMargin) shouldBe 10
  }

  // ===== Bitwise operators =====

  "bitwise and" in {
    eval("main() -> int = 0xFF & 0x0F\n") shouldBe 0x0F
  }

  "bitwise or" in {
    eval("main() -> int = 0xF0 | 0x0F\n") shouldBe 0xFF
  }

  "bitwise xor" in {
    eval("main() -> int = 0xFF ^ 0x0F\n") shouldBe 0xF0
  }

  "bitwise not" in {
    eval("main() -> int = ~0 & 0xFF\n") shouldBe 0xFF
  }

  "left shift" in {
    eval("main() -> int = 1 << 8\n") shouldBe 256
  }

  "right shift" in {
    eval("main() -> int = 256 >> 4\n") shouldBe 16
  }

  "right shift preserves sign" in {
    eval("main() -> int = -1 >> 1\n") shouldBe -1
  }

  // ===== Bitwise precedence (corrected from C) =====

  "bitwise and higher than comparison" in {
    eval(
      """main() -> int
        |    x = 0xF0
        |    mask = 0x0F
        |    if x & mask == 0 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "bitwise or higher than comparison" in {
    eval(
      """main() -> int
        |    if 0xF0 | 0x0F == 0xFF then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "shift higher than bitwise and" in {
    eval("main() -> int = 1 << 4 & 0xFF\n") shouldBe 16
  }

  "bitwise and higher than bitwise xor" in {
    eval("main() -> int = 0xFF & 0x0F ^ 0x05\n") shouldBe 0x0A
  }

  "bitwise xor higher than bitwise or" in {
    eval("main() -> int = 0x0F ^ 0x03 | 0xF0\n") shouldBe 0xFC
  }

  "complex bitwise expression" in {
    eval("main() -> int = (0xAB & 0xF0) | (0xCD & 0x0F)\n") shouldBe 0xAD
  }

  // ===== Bitwise compound assignment =====

  "&= basic" in {
    eval(
      """main() -> int
        |    x = 0xFF
        |    x &= 0x0F
        |    x
        |""".stripMargin) shouldBe 0x0F
  }

  "|= basic" in {
    eval(
      """main() -> int
        |    x = 0xF0
        |    x |= 0x0F
        |    x
        |""".stripMargin) shouldBe 0xFF
  }

  "^= basic" in {
    eval(
      """main() -> int
        |    x = 0xFF
        |    x ^= 0x0F
        |    x
        |""".stripMargin) shouldBe 0xF0
  }

  "<<= basic" in {
    eval(
      """main() -> int
        |    x = 1
        |    x <<= 8
        |    x
        |""".stripMargin) shouldBe 256
  }

  ">>= basic" in {
    eval(
      """main() -> int
        |    x = 256
        |    x >>= 4
        |    x
        |""".stripMargin) shouldBe 16
  }

  // ===== Bitwise in real code =====

  "extract nibbles" in {
    eval(
      """main() -> int
        |    x = 0xAB
        |    hi = (x >> 4) & 0x0F
        |    lo = x & 0x0F
        |    hi * 16 + lo
        |""".stripMargin) shouldBe 0xAB
  }

  "set and clear bits" in {
    eval(
      """main() -> int
        |    flags = 0
        |    flags |= 1 << 3
        |    flags |= 1 << 5
        |    has_bit_3 = if flags & (1 << 3) != 0 then 1 else 0
        |    has_bit_4 = if flags & (1 << 4) != 0 then 1 else 0
        |    flags &= ~(1 << 3)
        |    cleared = if flags & (1 << 3) != 0 then 1 else 0
        |    has_bit_3 * 100 + has_bit_4 * 10 + cleared
        |""".stripMargin) shouldBe 100
  }

  "swap with xor" in {
    eval(
      """main() -> int
        |    a = 42
        |    b = 99
        |    a ^= b
        |    b ^= a
        |    a ^= b
        |    a * 1000 + b
        |""".stripMargin) shouldBe 99042
  }

  "power of two check" in {
    eval(
      """isPow2(n: int) -> int = if n > 0 && n & (n - 1) == 0 then 1 else 0
        |
        |main() -> int
        |    isPow2(16) * 100 + isPow2(15) * 10 + isPow2(1)
        |""".stripMargin) shouldBe 101
  }

  // ===== Hex literals =====

  "hex literal" in {
    eval("main() -> int = 0xFF\n") shouldBe 255
  }

  "hex literal uppercase" in {
    eval("main() -> int = 0XFF\n") shouldBe 255
  }

  "hex literal zero" in {
    eval("main() -> int = 0x0\n") shouldBe 0
  }

  // ===== Chained comparison edge cases =====

  "single comparison is not chained" in {
    eval("main() -> int = if 3 < 5 then 1 else 0\n") shouldBe 1
  }

  "two comparisons chained" in {
    eval("main() -> int = if 1 < 2 < 3 then 1 else 0\n") shouldBe 1
  }

  "four comparisons chained" in {
    eval("main() -> int = if 1 < 2 <= 3 < 4 <= 5 then 1 else 0\n") shouldBe 1
  }

  "chained comparison short-circuits on first false" in {
    eval("main() -> int = if 1 < 2 > 3 < 4 then 1 else 0\n") shouldBe 0
  }

  "chained == comparison" in {
    eval("main() -> int = if 5 == 5 == 5 then 1 else 0\n") shouldBe 1
  }

  "chained != comparison" in {
    eval("main() -> int = if 1 != 2 != 3 then 1 else 0\n") shouldBe 1
  }

  // ===== Error case: division by zero =====

  "division by zero error" in {
    val Right(ast) = (new SyslParser).parseProgram("main() -> int = 42 / 0\n"): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val interp = new SyslInterpreter()
    an[Exception] should be thrownBy interp.run(typed)
  }

}
