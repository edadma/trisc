package io.github.edadma.trisc

class SyslLLVMGapTests extends SyslLLVMTestHelpers {

  // ===== Generics =====

  "generic function" in {
    llvmExit(
      """identity[T](x: T) -> T = x
        |
        |main() -> int = identity(42)
        |""".stripMargin) shouldBe 42
  }

  "generic function two instantiations" in {
    llvmExit(
      """add[T](a: T, b: T) -> T = a + b
        |
        |main() -> int
        |    add(20, 22)
        |""".stripMargin) shouldBe 42
  }

  "generic struct" in {
    llvmExit(
      """struct Pair[T]
        |    first: T
        |    second: T
        |
        |main() -> int
        |    p = Pair(20, 22)
        |    p.first + p.second
        |""".stripMargin) shouldBe 42
  }

  // ===== Traits/impl =====

  "trait impl" in {
    llvmExit(
      """trait Doubler[T]
        |    dbl(x: T) -> T
        |
        |impl Doubler[int]
        |    dbl(x: int) -> int = x * 2
        |
        |main() -> int = Doubler.dbl(21)
        |""".stripMargin) shouldBe 42
  }

  // ===== Methods (dot-call) =====

  "method call on struct" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |Point_sum(self: *Point) -> int = self.x + self.y
        |
        |main() -> int
        |    p = Point(20, 22)
        |    p.sum()
        |""".stripMargin) shouldBe 42
  }

  // ===== String indexing =====

  "string index" in {
    llvmExit(
      """main() -> int
        |    s = "ABC"
        |    s[0]
        |""".stripMargin) shouldBe 65  // 'A' = 65
  }

  // ===== Do-while =====

  "do-while loop" in {
    llvmExit(
      """main() -> int
        |    var x = 0
        |    do
        |        x = x + 1
        |    while x < 10
        |    x
        |""".stripMargin) shouldBe 10
  }

  // ===== Unary ops =====

  "unary negation" in {
    llvmExit(
      """main() -> int
        |    x = 42
        |    -(-x)
        |""".stripMargin) shouldBe 42
  }

  "boolean not" in {
    llvmExit(
      """main() -> int
        |    if !false
        |        42
        |    else
        |        0
        |""".stripMargin) shouldBe 42
  }

  // ===== Bitwise ops =====

  "bitwise and" in {
    llvmExit(
      """main() -> int
        |    0xFF & 0x2A
        |""".stripMargin) shouldBe 42
  }

  "bitwise or" in {
    llvmExit(
      """main() -> int
        |    0x20 | 0x0A
        |""".stripMargin) shouldBe 42
  }

  "bitwise xor" in {
    llvmExit(
      """main() -> int
        |    0x3F ^ 0x15
        |""".stripMargin) shouldBe 42
  }

  "left shift" in {
    llvmExit(
      """main() -> int
        |    21 << 1
        |""".stripMargin) shouldBe 42
  }

  "right shift" in {
    llvmExit(
      """main() -> int
        |    84 >> 1
        |""".stripMargin) shouldBe 42
  }

  // ===== Logical ops =====

  "logical and short-circuit" in {
    llvmExit(
      """main() -> int
        |    if true && true
        |        42
        |    else
        |        0
        |""".stripMargin) shouldBe 42
  }

  "logical or short-circuit" in {
    llvmExit(
      """main() -> int
        |    if false || true
        |        42
        |    else
        |        0
        |""".stripMargin) shouldBe 42
  }

  // ===== Range patterns in match =====

  "match range pattern" in {
    llvmExit(
      """main() -> int
        |    x = 5
        |    x match
        |        1..3 -> 10
        |        4..6 -> 42
        |        else -> 0
        |""".stripMargin) shouldBe 42
  }

  // ===== Nested function calls =====

  "nested calls" in {
    llvmExit(
      """add(a: int, b: int) -> int = a + b
        |mul(a: int, b: int) -> int = a * b
        |
        |main() -> int = add(mul(6, 7), 0)
        |""".stripMargin) shouldBe 42
  }

  // ===== Multiple return paths =====

  "early return" in {
    llvmExit(
      """check(x: int) -> int
        |    if x > 10
        |        return 42
        |    0
        |
        |main() -> int = check(20)
        |""".stripMargin) shouldBe 42
  }
}
