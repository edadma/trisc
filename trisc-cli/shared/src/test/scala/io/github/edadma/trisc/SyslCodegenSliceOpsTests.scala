package io.github.edadma.trisc

class SyslCodegenSliceOpsTests extends SyslCodegenHelpers {

  private val allocSource = scala.io.Source.fromFile("posix/stdlib/alloc.sysl").mkString
  private val stringSource = scala.io.Source.fromFile("posix/string/string.sysl").mkString

  private def sbrkModule(heapSize: Int = 16384): String =
    s"""module posix.unistd
       |
       |var _heap: [$heapSize]i8
       |var _brk: *i8 = *i8(0)
       |var _brk_initialized = false
       |
       |sbrk(increment: int) -> *i8
       |    if !_brk_initialized
       |        _brk = &_heap[0]
       |        _brk_initialized = true
       |    val old = _brk
       |    _brk = *i8(i64(_brk) + increment)
       |    old
       |""".stripMargin

  private def sliceSources(mainSource: String): Map[String, String] =
    Map(
      "posix/unistd/sbrk" -> sbrkModule(),
      "posix/string/string" -> stringSource,
      "posix/stdlib/alloc" -> allocSource,
      "main" -> mainSource,
    )

  // ===== Sub-slice basics =====

  "sub-slice [lo:hi]" in {
    compileMultiAndRun(sliceSources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    a = new [5]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    a[3] = 40
        |    a[4] = 50
        |    s = a[1:4]
        |    s[0] + s[1] + s[2]
        |""".stripMargin)) shouldBe 90
  }

  "sub-slice [:hi]" in {
    compileMultiAndRun(sliceSources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    s = a[:2]
        |    s[0] + s[1]
        |""".stripMargin)) shouldBe 30
  }

  "sub-slice [lo:]" in {
    compileMultiAndRun(sliceSources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    s = a[1:]
        |    s[0] + s[1]
        |""".stripMargin)) shouldBe 50
  }

  "sub-slice [:]" in {
    compileMultiAndRun(sliceSources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    s = a[:]
        |    s[0] + s[1] + s[2]
        |""".stripMargin)) shouldBe 60
  }

  "sub-slice len" in {
    compileMultiAndRun(sliceSources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    a = new [5]int
        |    s = a[1:4]
        |    len(s)
        |""".stripMargin)) shouldBe 3
  }

  "sub-slice cap" in {
    compileMultiAndRun(sliceSources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    a = new [5]int
        |    s = a[1:4]
        |    cap(s)
        |""".stripMargin)) shouldBe 4
  }

  "cap on ref slice" in {
    compileMultiAndRun(sliceSources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    a = new [7]int
        |    cap(a)
        |""".stripMargin)) shouldBe 7
  }

  // ===== Append =====

  "append within capacity" in {
    compileMultiAndRun(sliceSources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    a = new [5]int
        |    a[0] = 10
        |    s = a[:1]
        |    s = append(s, 20)
        |    s = append(s, 30)
        |    s[0] + s[1] + s[2]
        |""".stripMargin)) shouldBe 60
  }

  "append grows" in {
    compileMultiAndRun(sliceSources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    a = new [2]int
        |    a[0] = 10
        |    a[1] = 20
        |    s = a[:]
        |    s = append(s, 30)
        |    s[0] + s[1] + s[2]
        |""".stripMargin)) shouldBe 60
  }

  "append len updates" in {
    compileMultiAndRun(sliceSources(
      """import posix.stdlib.{malloc, free}
        |
        |main() -> int
        |    a = new [5]int
        |    s = a[:0]
        |    s = append(s, 1)
        |    s = append(s, 2)
        |    s = append(s, 3)
        |    len(s)
        |""".stripMargin)) shouldBe 3
  }
}
