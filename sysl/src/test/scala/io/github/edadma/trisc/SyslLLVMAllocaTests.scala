package io.github.edadma.trisc

class SyslLLVMAllocaTests extends SyslLLVMTestHelpers {

  // Tests verifying that all allocas are correctly hoisted to the entry block.
  // These test patterns where variables/aggregates are created inside
  // conditional blocks, loops, or nested scopes.

  "variable declared inside if-then only" in {
    llvmExit(
      """main() -> int
        |    if true
        |        val x = 42
        |        x
        |    else
        |        0
        |""".stripMargin) shouldBe 42
  }

  "variable declared inside loop" in {
    llvmExit(
      """main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 5
        |        val x = i * 2
        |        sum = sum + x
        |        i = i + 1
        |    sum
        |""".stripMargin) shouldBe 20
  }

  "string literal inside conditional" in {
    llvmExit(
      """main() -> int
        |    var n = 0
        |    if true
        |        val s = "hello"
        |        n = len(s)
        |    n
        |""".stripMargin) shouldBe 5
  }

  "different strings in both branches" in {
    llvmExit(
      """main() -> int
        |    var n = 0
        |    if true
        |        val s = "hello"
        |        n = len(s)
        |    else
        |        val s = "world!"
        |        n = len(s)
        |    n
        |""".stripMargin) shouldBe 5
  }

  "nested if with string in inner branch" in {
    llvmExit(
      """main() -> int
        |    var result = 0
        |    if true
        |        if true
        |            val s = "deep"
        |            result = len(s)
        |    result
        |""".stripMargin) shouldBe 4
  }

  "string concat inside loop" in {
    llvmOutput(
      """main()
        |    var s = "a"
        |    var i = 0
        |    while i < 3
        |        s = s + "b"
        |        i = i + 1
        |    puts(s)
        |""".stripMargin) shouldBe "abbb"
  }

  "tuple created inside conditional" in {
    llvmExit(
      """main() -> int
        |    var n = 0
        |    if true
        |        val t = (10, 20)
        |        n = t.0 + t.1
        |    n
        |""".stripMargin) shouldBe 30
  }

  "struct constructed inside conditional" in {
    llvmExit(
      """struct Pt
        |    x: int
        |    y: int
        |
        |main() -> int
        |    var result = 0
        |    if true
        |        val p = Pt(10, 32)
        |        result = p.x + p.y
        |    result
        |""".stripMargin) shouldBe 42
  }

  "enum constructed inside conditional" in {
    llvmExit(
      """enum Color
        |    Red
        |    Green
        |    Blue
        |
        |main() -> int
        |    val c = if true then Green else Red
        |    c match
        |        Green -> 42
        |        _ -> 0
        |""".stripMargin) shouldBe 42
  }

  "match with string result in arm" in {
    llvmExit(
      """main() -> int
        |    val x = 2
        |    val s = x match
        |        1 -> "one"
        |        2 -> "two"
        |        _ -> "other"
        |    len(s)
        |""".stripMargin) shouldBe 3
  }

  "function returning string from inside conditional" in {
    llvmOutput(
      """pick(flag: bool) -> string
        |    if flag
        |        "yes"
        |    else
        |        "no"
        |
        |main()
        |    puts(pick(true))
        |""".stripMargin) shouldBe "yes"
  }

  "array created inside conditional" in {
    llvmExit(
      """main() -> int
        |    var result = 0
        |    if true
        |        val a = [10, 20, 12]
        |        result = a[0] + a[1] + a[2]
        |    result
        |""".stripMargin) shouldBe 42
  }

  "closure created inside loop" in {
    llvmExit(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 3
        |        val offset = i
        |        val f: (int) -> int = x -> x + offset
        |        sum = sum + apply(f, 10)
        |        i = i + 1
        |    sum
        |""".stripMargin) shouldBe 33
  }

  "function call returning aggregate inside conditional" in {
    llvmExit(
      """make_pair() -> (int, int) = (10, 32)
        |
        |main() -> int
        |    var result = 0
        |    if true
        |        val a, b = make_pair()
        |        result = a + b
        |    result
        |""".stripMargin) shouldBe 42
  }

  "multiple aggregates in different branches" in {
    llvmExit(
      """struct Pt
        |    x: int
        |    y: int
        |
        |main() -> int
        |    val flag = true
        |    var result = 0
        |    if flag
        |        val p = Pt(10, 20)
        |        val s = "hello"
        |        result = p.x + p.y + len(s)
        |    else
        |        val t = (100, 200)
        |        result = t.0 + t.1
        |    result
        |""".stripMargin) shouldBe 35
  }

  "aggregate in loop body (multiple iterations)" in {
    llvmExit(
      """main() -> int
        |    var total = 0
        |    var i = 0
        |    while i < 4
        |        val s = "ab"
        |        total = total + len(s)
        |        i = i + 1
        |    total
        |""".stripMargin) shouldBe 8
  }

  // ===== Slice backref RC tests =====

  "slice returned from function (backref survives)" in {
    llvmExit(
      """make_slice() -> []int
        |    val a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    a[:]
        |
        |main() -> int
        |    val s = make_slice()
        |    s[0] + s[1] + s[2]
        |""".stripMargin) shouldBe 60
  }

  "slice param backref (callee uses caller slice)" in {
    llvmExit(
      """sum_slice(s: []int) -> int
        |    var total = 0
        |    var i = 0
        |    while i < len(s)
        |        total = total + s[i]
        |        i = i + 1
        |    total
        |
        |main() -> int
        |    val a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    val s = a[:]
        |    sum_slice(s)
        |""".stripMargin) shouldBe 60
  }

  "slice reassignment (old backref decremented)" in {
    llvmExit(
      """main() -> int
        |    val a = new [3]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    var s = a[:]
        |    val b = new [2]int
        |    b[0] = 10
        |    b[1] = 20
        |    s = b[:]
        |    s[0] + s[1]
        |""".stripMargin) shouldBe 30
  }

  "append replaces backref with null (no crash)" in {
    llvmExit(
      """main() -> int
        |    val a = new [2]int
        |    a[0] = 1
        |    a[1] = 2
        |    var s = a[:]
        |    s = append(s, 3)
        |    s[0] + s[1] + s[2]
        |""".stripMargin) shouldBe 6
  }

  "tuple containing slice returned from function" in {
    llvmExit(
      """make_pair() -> ([]int, int)
        |    val a = new [2]int
        |    a[0] = 10
        |    a[1] = 20
        |    (a[:], 42)
        |
        |main() -> int
        |    val s, n = make_pair()
        |    s[0] + s[1] + n
        |""".stripMargin) shouldBe 72
  }

  "reslice inherits and increments backref" in {
    llvmExit(
      """main() -> int
        |    val a = new [5]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    a[3] = 4
        |    a[4] = 5
        |    val s1 = a[:]
        |    val s2 = s1[1:4]
        |    s2[0] + s2[1] + s2[2]
        |""".stripMargin) shouldBe 9
  }
}
