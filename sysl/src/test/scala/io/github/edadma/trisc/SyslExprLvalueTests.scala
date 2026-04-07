package io.github.edadma.trisc

class SyslExprLvalueTests extends SyslTestHelpers {

  // ===== Deref then index: (*p)[i] = x =====

  "deref pointer then index assign" in {
    eval(
      """main() -> int
        |    var arr: [3]int
        |    var p = &arr
        |    (*p)[0] = 10
        |    (*p)[1] = 20
        |    (*p)[2] = 30
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 60
  }

  // ===== Deref then field: (*p).field = x =====

  "deref pointer then field assign" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    var pt: Point
        |    var p = &pt
        |    (*p).x = 3
        |    (*p).y = 4
        |    pt.x + pt.y
        |""".stripMargin) shouldBe 7
  }

  // ===== Deref then field then index: (*p).arr[i] = x =====

  "deref then field then index" in {
    eval(
      """struct Data
        |    values: [3]int
        |
        |main() -> int
        |    var d: Data
        |    var p = &d
        |    (*p).values[0] = 100
        |    (*p).values[1] = 200
        |    d.values[0] + d.values[1]
        |""".stripMargin) shouldBe 300
  }

  // ===== Parenthesized expression index: (base + offset)[i] = x =====

  "pointer arithmetic then index assign" in {
    eval(
      """main() -> int
        |    var arr: [4]int
        |    var p = &arr[0]
        |    (p + 1)[0] = 42
        |    arr[1]
        |""".stripMargin) shouldBe 42
  }

  // ===== Simple deref assign still works =====

  "simple deref assign unchanged" in {
    eval(
      """main() -> int
        |    var x = 0
        |    var p = &x
        |    *p = 99
        |    x
        |""".stripMargin) shouldBe 99
  }

  // ===== Compound assign through deref =====

  "deref compound assign" in {
    eval(
      """main() -> int
        |    var x = 10
        |    var p = &x
        |    *p += 5
        |    x
        |""".stripMargin) shouldBe 15
  }

  // ===== Compound assign through deref+index =====

  "deref index compound assign" in {
    eval(
      """main() -> int
        |    var arr: [3]int
        |    arr[0] = 10
        |    var p = &arr
        |    (*p)[0] += 5
        |    arr[0]
        |""".stripMargin) shouldBe 15
  }

  // ===== Compound assign through deref+field =====

  "deref field compound assign" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    var pt = Point(10, 20)
        |    var p = &pt
        |    (*p).x += 5
        |    pt.x
        |""".stripMargin) shouldBe 15
  }
}
