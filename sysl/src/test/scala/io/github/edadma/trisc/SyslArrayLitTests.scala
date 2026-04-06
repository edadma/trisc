package io.github.edadma.trisc

class SyslArrayLitTests extends SyslTestHelpers:

  "local array literal and indexing" in {
    eval(
      """main() -> int
        |    a = [10, 20, 30]
        |    a[1]
        |""".stripMargin) shouldBe 20
  }

  "local array literal first element" in {
    eval(
      """main() -> int
        |    a = [42, 0, 0]
        |    a[0]
        |""".stripMargin) shouldBe 42
  }

  "local array literal last element" in {
    eval(
      """main() -> int
        |    a = [1, 2, 99]
        |    a[2]
        |""".stripMargin) shouldBe 99
  }

  "local array literal sum" in {
    eval(
      """main() -> int
        |    a = [10, 20, 30]
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 60
  }

  "global array literal" in {
    eval(
      """data = [100, 200, 300]
        |
        |main() -> int
        |    data[0] + data[1] + data[2]
        |""".stripMargin) shouldBe 600
  }

  "global array literal indexing" in {
    eval(
      """table = [1, 4, 9, 16, 25]
        |
        |main() -> int
        |    table[3]
        |""".stripMargin) shouldBe 16
  }

  "array literal passed to function" in {
    eval(
      """sum(arr: *int, n: int) -> int
        |    var total: int = 0
        |    for i = 0; i < n; i++
        |        total = total + arr[i]
        |    total
        |
        |main() -> int
        |    a = [5, 10, 15]
        |    sum(a, 3)
        |""".stripMargin) shouldBe 30
  }

  "global byte array literal" in {
    eval(
      """font: [3]byte = [0x30, 0x78, 0xCC]
        |
        |main() -> int
        |    int(font[0]) + int(font[1]) + int(font[2])
        |""".stripMargin) shouldBe (0x30 + 0x78 + 0xCC)
  }
