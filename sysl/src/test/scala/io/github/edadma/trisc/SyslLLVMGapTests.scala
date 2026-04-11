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

  // ===== Tuple type disambiguation =====

  "two different 2-tuples in same function" in {
    llvmExit(
      """main() -> int
        |    val a = (42, true)
        |    val b = (10, 20)
        |    if a.1 then a.0 + b.0 + b.1 else 0
        |""".stripMargin) shouldBe 72
  }

  "function returning different tuple types" in {
    llvmExit(
      """pair_int() -> (int, int) = (10, 32)
        |pair_bool() -> (int, bool) = (42, true)
        |
        |main() -> int
        |    val a, b = pair_int()
        |    val c, d = pair_bool()
        |    if d then a + b + c else 0
        |""".stripMargin) shouldBe 84
  }

  "tuple with string field" in {
    llvmOutput(
      """make() -> (string, int) = ("hello", 5)
        |
        |main()
        |    val s, n = make()
        |    puts(s)
        |""".stripMargin) shouldBe "hello"
  }

  "cross-function tuple type disambiguation" in {
    llvmExit(
      """make_pair() -> (int, bool) = (42, true)
        |make_nums() -> (int, int) = (10, 20)
        |
        |main() -> int
        |    val x, ok = make_pair()
        |    val a, b = make_nums()
        |    if ok then x + a + b else 0
        |""".stripMargin) shouldBe 72
  }

  // ===== Dynamic new + slice element access =====

  "new [n]int store and load" in {
    llvmExit(
      """main() -> int
        |    val a = new [3]int
        |    a[0] = 100
        |    a[1] = 200
        |    a[2] = 300
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 600 % 256  // exit code is modulo 256
  }

  "generic reverse from another module" in {
    val (exit, _) = runLLVMMulti(Map(
      "mylib/util/util" ->
        """module mylib.util
          |
          |reverse[T](s: []T) -> []T
          |    val n = len(s)
          |    val buf = new [n]T
          |    for i in 0..<n
          |        buf[i] = s[n - 1 - i]
          |    buf[:]
          |""".stripMargin,
      "main" ->
        """import mylib.util.*
          |
          |main() -> int
          |    val a = new [3]int
          |    a[0] = 1; a[1] = 2; a[2] = 3
          |    val b = reverse(a[:])
          |    b[0] * 100 + b[1] * 10 + b[2]
          |""".stripMargin
    ))
    (exit % 256) shouldBe (321 % 256)
  }

  "new [n]int via slice reverse manual" in {
    llvmExit(
      """main() -> int
        |    val a = new [3]int
        |    a[0] = 1; a[1] = 2; a[2] = 3
        |    val b = new [3]int
        |    val s = a[:]
        |    val n = len(s)
        |    for i in 0..<n
        |        b[i] = s[n - 1 - i]
        |    b[0] * 100 + b[1] * 10 + b[2]
        |""".stripMargin) shouldBe (321 % 256)
  }
}
