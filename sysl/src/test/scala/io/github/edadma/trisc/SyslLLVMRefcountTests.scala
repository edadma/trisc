package io.github.edadma.trisc

class SyslLLVMRefcountTests extends SyslLLVMTestHelpers {

  "ref created and dropped without crash" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    val p = new Point(3, 4)
        |    p.x + p.y
        |""".stripMargin) shouldBe 7
  }

  "ref passed to function" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |sum(p: &Point) -> int = p.x + p.y
        |
        |main() -> int
        |    val p = new Point(20, 22)
        |    sum(p)
        |""".stripMargin) shouldBe 42
  }

  "ref passed to multiple functions" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |get_x(p: &Point) -> int = p.x
        |get_y(p: &Point) -> int = p.y
        |
        |main() -> int
        |    val p = new Point(17, 25)
        |    get_x(p) + get_y(p)
        |""".stripMargin) shouldBe 42
  }

  "ref field assignment" in {
    llvmExit(
      """struct Counter
        |    value: int
        |
        |main() -> int
        |    val c = new Counter(0)
        |    c.value = 42
        |    c.value
        |""".stripMargin) shouldBe 42
  }

  "deinit called on ref drop" in {
    llvmOutput(
      """struct Resource
        |    id: int
        |
        |Resource_deinit(self: &Resource) -> int
        |    puts("freed")
        |    0
        |
        |main() -> int
        |    val r = new Resource(1)
        |    println(r.id)
        |    0
        |""".stripMargin) shouldBe "1\nfreed"
  }

  "ref shared between two variables" in {
    llvmOutput(
      """struct Data
        |    value: int
        |
        |Data_deinit(self: &Data) -> int
        |    puts("deinit")
        |    0
        |
        |main() -> int
        |    val a = new Data(42)
        |    val b = a
        |    println(b.value)
        |    0
        |""".stripMargin) shouldBe "42\ndeinit"
  }

  "new array ref without crash" in {
    llvmExit(
      """main() -> int
        |    val a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 12
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 42
  }
}
