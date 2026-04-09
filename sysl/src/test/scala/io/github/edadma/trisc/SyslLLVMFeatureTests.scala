package io.github.edadma.trisc

class SyslLLVMFeatureTests extends SyslLLVMTestHelpers {

  // ===== For loops =====

  "for loop sum" in {
    llvmExit(
      """main() -> int
        |    var sum = 0
        |    for var i = 0; i < 10; i = i + 1
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 45
  }

  "for loop with break" in {
    llvmExit(
      """main() -> int
        |    var sum = 0
        |    for var i = 0; i < 100; i = i + 1
        |        if i == 5
        |            break
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "for loop with continue" in {
    llvmExit(
      """main() -> int
        |    var sum = 0
        |    for var i = 0; i < 10; i = i + 1
        |        if i % 2 == 0
        |            continue
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 25
  }

  "while with break" in {
    llvmExit(
      """main() -> int
        |    var i = 0
        |    while true
        |        if i == 42
        |            break
        |        i = i + 1
        |    i
        |""".stripMargin) shouldBe 42
  }

  // ===== Arrays =====

  "array literal and index" in {
    llvmExit(
      """main() -> int
        |    a = [10, 20, 30]
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 60
  }

  "array index assign" in {
    llvmExit(
      """main() -> int
        |    var a = [0, 0, 0]
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 12
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 42
  }

  "array length" in {
    llvmExit(
      """main() -> int
        |    a = [1, 2, 3, 4, 5]
        |    len(a)
        |""".stripMargin) shouldBe 5
  }

  "array in for loop" in {
    llvmExit(
      """main() -> int
        |    a = [3, 7, 11, 21]
        |    var sum = 0
        |    for var i = 0; i < len(a); i = i + 1
        |        sum = sum + a[i]
        |    sum
        |""".stripMargin) shouldBe 42
  }

  // ===== Pointers =====

  "addr-of and deref" in {
    llvmExit(
      """main() -> int
        |    var x = 42
        |    p = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "deref assign" in {
    llvmExit(
      """main() -> int
        |    var x = 0
        |    p = &x
        |    *p = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  // ===== Slices =====

  "slice from array" in {
    llvmExit(
      """main() -> int
        |    a = [10, 20, 30, 40, 50]
        |    s = a[1:4]
        |    s[0] + s[1] + s[2]
        |""".stripMargin) shouldBe 90
  }

  "slice length" in {
    llvmExit(
      """main() -> int
        |    a = [1, 2, 3, 4, 5]
        |    s = a[1:4]
        |    len(s)
        |""".stripMargin) shouldBe 3
  }

  // ===== Refs =====

  "new struct ref" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    val p = new Point(20, 22)
        |    p.x + p.y
        |""".stripMargin) shouldBe 42
  }

  // ===== Combined =====

  "for loop with array and println" in {
    llvmOutput(
      """main() -> int
        |    a = [10, 20, 30]
        |    for var i = 0; i < 3; i = i + 1
        |        println(a[i])
        |    0
        |""".stripMargin) shouldBe "10\n20\n30"
  }
}
