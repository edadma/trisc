package io.github.edadma.trisc

class SyslCodegenExprTests extends SyslCodegenHelpers {

  // ===== Arithmetic =====

  "addition" in { compileAndRun("main() -> int = 3 + 4\n") shouldBe 7 }
  "subtraction" in { compileAndRun("main() -> int = 10 - 3\n") shouldBe 7 }
  "multiplication" in { compileAndRun("main() -> int = 6 * 7\n") shouldBe 42 }
  "division" in { compileAndRun("main() -> int = 42 / 6\n") shouldBe 7 }
  "modulo" in { compileAndRun("main() -> int = 17 % 5\n") shouldBe 2 }
  "operator precedence" in { compileAndRun("main() -> int = 2 + 3 * 4\n") shouldBe 14 }
  "parentheses" in { compileAndRun("main() -> int = (2 + 3) * 4\n") shouldBe 20 }
  "unary minus" in { compileAndRun("main() -> int = -42\n") shouldBe -42 }

  // ===== Comparison =====

  "less than true" in { compileAndRun("main() -> int = 3 < 5\n") shouldBe 1 }
  "less than false" in { compileAndRun("main() -> int = 5 < 3\n") shouldBe 0 }
  "equal true" in { compileAndRun("main() -> int = 5 == 5\n") shouldBe 1 }
  "not equal" in { compileAndRun("main() -> int = 5 != 3\n") shouldBe 1 }

  // ===== Logical =====

  "logical and" in { compileAndRun("main() -> int = true && true\n") shouldBe 1 }
  "logical and short-circuit" in { compileAndRun("main() -> int = false && true\n") shouldBe 0 }
  "logical or" in { compileAndRun("main() -> int = false || true\n") shouldBe 1 }
  "logical not" in { compileAndRun("main() -> int = !false\n") shouldBe 1 }

  // ===== Chained comparisons =====

  "chained comparison lower <= x <= upper" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    if 1 <= x <= 10 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "chained comparison out of range" in {
    compileAndRun(
      """main() -> int
        |    x = 15
        |    if 1 <= x <= 10 then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "chained comparison three operators" in {
    compileAndRun(
      """main() -> int
        |    if 1 < 2 < 3 < 4 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "chained comparison three operators fails" in {
    compileAndRun(
      """main() -> int
        |    if 1 < 2 < 3 < 2 then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "chained comparison mixed operators" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    if 0 < x <= 5 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Boolean literals =====

  "true literal" in { compileAndRun("main() -> int = if true then 1 else 0\n") shouldBe 1 }
  "false literal" in { compileAndRun("main() -> int = if false then 1 else 0\n") shouldBe 0 }

  // ===== Unary precedence =====

  "unary minus with multiplication" in { compileAndRun("main() -> int = -3 * 2\n") shouldBe -6 }
  "unary not with comparison" in { compileAndRun("main() -> int = if !(3 > 5) then 1 else 0\n") shouldBe 1 }

  // ===== Increment/decrement =====

  "prefix increment" in {
    compileAndRun("main() -> int\n    x = 5\n    ++x\n") shouldBe 6
  }

  "prefix increment returns new value" in {
    compileAndRun("main() -> int\n    x = 5\n    y = ++x\n    y\n") shouldBe 6
  }

  "prefix decrement" in {
    compileAndRun("main() -> int\n    x = 5\n    --x\n") shouldBe 4
  }

  "postfix increment returns old value" in {
    compileAndRun("main() -> int\n    x = 5\n    y = x++\n    y\n") shouldBe 5
  }

  "postfix increment modifies variable" in {
    compileAndRun("main() -> int\n    x = 5\n    x++\n    x\n") shouldBe 6
  }

  "postfix decrement returns old value" in {
    compileAndRun("main() -> int\n    x = 5\n    y = x--\n    y\n") shouldBe 5
  }

  "postfix decrement modifies variable" in {
    compileAndRun("main() -> int\n    x = 5\n    x--\n    x\n") shouldBe 4
  }

  "increment in while loop" in {
    compileAndRun(
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
    compileAndRun("main() -> int\n    x = 5\n    x++ + 10\n") shouldBe 15
  }

  "prefix in expression" in {
    compileAndRun("main() -> int\n    x = 5\n    ++x + 10\n") shouldBe 16
  }

  // ===== Compound assignment =====

  "+= basic" in { compileAndRun("main() -> int\n    x = 10\n    x += 5\n    x\n") shouldBe 15 }
  "-= basic" in { compileAndRun("main() -> int\n    x = 10\n    x -= 3\n    x\n") shouldBe 7 }
  "*= basic" in { compileAndRun("main() -> int\n    x = 6\n    x *= 7\n    x\n") shouldBe 42 }
  "/= basic" in { compileAndRun("main() -> int\n    x = 42\n    x /= 6\n    x\n") shouldBe 7 }
  "%= basic" in { compileAndRun("main() -> int\n    x = 17\n    x %= 5\n    x\n") shouldBe 2 }

  "+= in while loop" in {
    compileAndRun(
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
    compileAndRun(
      """main() -> int
        |    x = 10
        |    y = 5
        |    x += y * 2
        |    x
        |""".stripMargin) shouldBe 20
  }

  // ===== Bitwise operators =====

  "bitwise and" in { compileAndRun("main() -> int = 0xFF & 0x0F\n") shouldBe 0x0F }
  "bitwise or" in { compileAndRun("main() -> int = 0xF0 | 0x0F\n") shouldBe 0xFF }
  "bitwise xor" in { compileAndRun("main() -> int = 0xFF ^ 0x0F\n") shouldBe 0xF0 }
  "bitwise not" in { compileAndRun("main() -> int = ~0 & 0xFF\n") shouldBe 0xFF }
  "left shift" in { compileAndRun("main() -> int = 1 << 8\n") shouldBe 256 }
  "right shift" in { compileAndRun("main() -> int = 256 >> 4\n") shouldBe 16 }
  "right shift preserves sign" in { compileAndRun("main() -> int = -1 >> 1\n") shouldBe -1 }

  // ===== Bitwise precedence =====

  "bitwise and higher than comparison" in {
    compileAndRun(
      """main() -> int
        |    x = 0xF0
        |    mask = 0x0F
        |    if x & mask == 0 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "shift higher than bitwise and" in {
    compileAndRun("main() -> int = 1 << 4 & 0xFF\n") shouldBe 16
  }

  "complex bitwise expression" in {
    compileAndRun("main() -> int = (0xAB & 0xF0) | (0xCD & 0x0F)\n") shouldBe 0xAD
  }

  // ===== Bitwise compound assignment =====

  "&= basic" in { compileAndRun("main() -> int\n    x = 0xFF\n    x &= 0x0F\n    x\n") shouldBe 0x0F }
  "|= basic" in { compileAndRun("main() -> int\n    x = 0xF0\n    x |= 0x0F\n    x\n") shouldBe 0xFF }
  "^= basic" in { compileAndRun("main() -> int\n    x = 0xFF\n    x ^= 0x0F\n    x\n") shouldBe 0xF0 }
  "<<= basic" in { compileAndRun("main() -> int\n    x = 1\n    x <<= 8\n    x\n") shouldBe 256 }
  ">>= basic" in { compileAndRun("main() -> int\n    x = 256\n    x >>= 4\n    x\n") shouldBe 16 }

  // ===== Practical bitwise =====

  "extract nibbles" in {
    compileAndRun(
      """main() -> int
        |    x = 0xAB
        |    hi = (x >> 4) & 0x0F
        |    lo = x & 0x0F
        |    hi * 16 + lo
        |""".stripMargin) shouldBe 0xAB
  }

  "set and clear bits" in {
    compileAndRun(
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
    compileAndRun(
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
    compileAndRun(
      """isPow2(n: int) -> int = if n > 0 && n & (n - 1) == 0 then 1 else 0
        |
        |main() -> int
        |    isPow2(16) * 100 + isPow2(15) * 10 + isPow2(1)
        |""".stripMargin) shouldBe 101
  }

  // ===== Hex literals =====

  "hex literal" in { compileAndRun("main() -> int = 0xFF\n") shouldBe 255 }
  "hex literal zero" in { compileAndRun("main() -> int = 0x0\n") shouldBe 0 }
}
