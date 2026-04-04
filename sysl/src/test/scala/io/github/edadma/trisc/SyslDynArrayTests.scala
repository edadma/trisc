package io.github.edadma.trisc

class SyslDynArrayTests extends SyslTestHelpers {

  // ===== Basic new [n]T =====

  "new array allocates and zeros" in {
    eval(
      """main() -> int
        |    val a = new [5]int
        |    a[0] + a[1] + a[2] + a[3] + a[4]
        |""".stripMargin) shouldBe 0
  }

  "new array write and read" in {
    eval(
      """main() -> int
        |    val a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 60
  }

  "new array with runtime size" in {
    eval(
      """main() -> int
        |    val n = 4
        |    val a = new [n]int
        |    var i = 0
        |    while i < n
        |        a[i] = i * 10
        |        i++
        |    a[0] + a[1] + a[2] + a[3]
        |""".stripMargin) shouldBe 60
  }

  "new array with expression size" in {
    eval(
      """main() -> int
        |    val a = new [2 + 3]int
        |    a[4] = 99
        |    a[4]
        |""".stripMargin) shouldBe 99
  }

  // ===== Type annotation =====

  "ref slice type annotation" in {
    eval(
      """main() -> int
        |    val a: &[]int = new [3]int
        |    a[0] = 42
        |    a[0]
        |""".stripMargin) shouldBe 42
  }

  // ===== Bounds checking =====

  "bounds check — negative index" in {
    an[Exception] should be thrownBy eval(
      """main() -> int
        |    val a = new [3]int
        |    a[-1]
        |""".stripMargin)
  }

  "bounds check — index equals length" in {
    an[Exception] should be thrownBy eval(
      """main() -> int
        |    val a = new [3]int
        |    a[3]
        |""".stripMargin)
  }

  "bounds check — index exceeds length" in {
    an[Exception] should be thrownBy eval(
      """main() -> int
        |    val a = new [3]int
        |    a[10]
        |""".stripMargin)
  }

  // ===== len() =====

  "len of new array" in {
    eval(
      """main() -> int
        |    val a = new [7]int
        |    len(a)
        |""".stripMargin) shouldBe 7
  }

  "len of runtime-sized array" in {
    eval(
      """main() -> int
        |    val n = 10
        |    val a = new [n]int
        |    len(a)
        |""".stripMargin) shouldBe 10
  }

  // ===== Ref sharing =====

  "two refs share same array" in {
    eval(
      """main() -> int
        |    val a = new [3]int
        |    val b = a
        |    a[1] = 42
        |    b[1]
        |""".stripMargin) shouldBe 42
  }

  "three refs share same array" in {
    eval(
      """main() -> int
        |    val a = new [2]int
        |    val b = a
        |    val c = b
        |    c[0] = 77
        |    a[0]
        |""".stripMargin) shouldBe 77
  }

  "reassign ref — old preserved" in {
    eval(
      """main() -> int
        |    var a = new [2]int
        |    val b = a
        |    a[0] = 10
        |    a = new [2]int
        |    a[0] = 20
        |    b[0]
        |""".stripMargin) shouldBe 10
  }

  // ===== Passed to functions =====

  "array ref passed to function — read" in {
    eval(
      """sum(a: &[]int, n: int) -> int
        |    var total = 0
        |    var i = 0
        |    while i < n
        |        total += a[i]
        |        i++
        |    total
        |
        |main() -> int
        |    val a = new [4]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    a[3] = 4
        |    sum(a, 4)
        |""".stripMargin) shouldBe 10
  }

  "array ref passed to function — write" in {
    eval(
      """fill(a: &[]int, n: int, v: int)
        |    var i = 0
        |    while i < n
        |        a[i] = v
        |        i++
        |
        |main() -> int
        |    val a = new [5]int
        |    fill(a, 5, 7)
        |    a[0] + a[1] + a[2] + a[3] + a[4]
        |""".stripMargin) shouldBe 35
  }

  "array ref returned from function" in {
    eval(
      """make_range(n: int) -> &[]int
        |    val a = new [n]int
        |    var i = 0
        |    while i < n
        |        a[i] = i
        |        i++
        |    a
        |
        |main() -> int
        |    val a = make_range(5)
        |    a[0] + a[1] + a[2] + a[3] + a[4]
        |""".stripMargin) shouldBe 10
  }

  "array ref passed to function using len" in {
    eval(
      """sum_all(a: &[]int) -> int
        |    var total = 0
        |    var i = 0
        |    while i < len(a)
        |        total += a[i]
        |        i++
        |    total
        |
        |main() -> int
        |    val a = new [3]int
        |    a[0] = 100
        |    a[1] = 200
        |    a[2] = 300
        |    sum_all(a)
        |""".stripMargin) shouldBe 600
  }

  // ===== Different element types =====

  "new array of i8" in {
    eval(
      """main() -> int
        |    val a = new [4]i8
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    a[3] = 4
        |    a[0] + a[1] + a[2] + a[3]
        |""".stripMargin) shouldBe 10
  }

  "new array of i64" in {
    eval(
      """main() -> int
        |    val a = new [2]i64
        |    a[0] = 1000000i64
        |    a[1] = 2000000i64
        |    int(a[0] + a[1])
        |""".stripMargin) shouldBe 3000000
  }

  "new array of bool" in {
    eval(
      """main() -> int
        |    val a = new [3]bool
        |    a[0] = true
        |    a[1] = false
        |    a[2] = true
        |    if a[0] && !a[1] && a[2] then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  // ===== Multiple arrays =====

  "multiple independent arrays" in {
    eval(
      """main() -> int
        |    val a = new [3]int
        |    val b = new [3]int
        |    a[0] = 10
        |    b[0] = 20
        |    a[0] + b[0]
        |""".stripMargin) shouldBe 30
  }

  "array in loop" in {
    eval(
      """main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 5
        |        val a = new [1]int
        |        a[0] = i * 10
        |        sum += a[0]
        |        i++
        |    sum
        |""".stripMargin) shouldBe 100
  }
}
