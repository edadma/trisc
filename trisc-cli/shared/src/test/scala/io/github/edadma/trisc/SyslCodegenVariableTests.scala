package io.github.edadma.trisc

class SyslCodegenVariableTests extends SyslCodegenHelpers {

  "local variable" in {
    compileAndRun(
      """main() -> int
        |    x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "variable assignment" in {
    compileAndRun(
      """main() -> int
        |    x = 1
        |    x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "multiple variables" in {
    compileAndRun(
      """main() -> int
        |    a = 10
        |    b = 20
        |    a + b
        |""".stripMargin) shouldBe 30
  }

  "variable in expression" in {
    compileAndRun(
      """main() -> int
        |    x = 21
        |    x * 2
        |""".stripMargin) shouldBe 42
  }

  // ===== Width-Aware Locals =====

  "i8 local store and load" in {
    compileAndRun(
      """main() -> int
        |    var x: byte = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "i16 local store and load" in {
    compileAndRun(
      """main() -> int
        |    var x: i16 = 1000
        |    x
        |""".stripMargin) shouldBe 1000
  }

  "i32 local store and load" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 50000
        |    x
        |""".stripMargin) shouldBe 50000
  }

  "i64 local store and load" in {
    compileAndRun(
      """main() -> int
        |    var x: i64 = 50000
        |    x
        |""".stripMargin) shouldBe 50000
  }

  "mixed width locals" in {
    compileAndRun(
      """main() -> int
        |    var a: byte = 10
        |    var b: i16 = 20
        |    var c: int = 30
        |    var d: i64 = 40
        |    a + b + c + d
        |""".stripMargin) shouldBe 100
  }

  "mixed width locals reverse order" in {
    compileAndRun(
      """main() -> int
        |    var d: i64 = 40
        |    var c: int = 30
        |    var b: i16 = 20
        |    var a: byte = 10
        |    a + b + c + d
        |""".stripMargin) shouldBe 100
  }

  "mixed width locals with reassignment" in {
    compileAndRun(
      """main() -> int
        |    var a: byte = 1
        |    var b: int = 2
        |    a = 10
        |    b = 20
        |    a + b
        |""".stripMargin) shouldBe 30
  }

  // ===== Global variables =====

  "global variable with initializer" in {
    compileAndRun(
      """x = 42
        |main() -> int = x
        |""".stripMargin, memSize = 0x2000) shouldBe 42
  }

  "global variable bool initializer" in {
    compileAndRun(
      """flag = true
        |main() -> int = flag
        |""".stripMargin, memSize = 0x2000) shouldBe 1
  }

  "i32 global variable" in {
    compileAndRun(
      """var g: int = 42
        |main() -> int = g
        |""".stripMargin) shouldBe 42
  }

  "i32 global compound assign" in {
    compileAndRun(
      """var g: int = 10
        |main() -> int
        |    g += 5
        |    g
        |""".stripMargin) shouldBe 15
  }

  "bool global variable" in {
    compileAndRun(
      """var g: bool = true
        |main() -> int
        |    if g then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "i8 global read" in {
    compileAndRun(
      """var g: byte = 42
        |main() -> int = g
        |""".stripMargin) shouldBe 42
  }

  "i16 global read" in {
    compileAndRun(
      """var g: i16 = 1000
        |main() -> int = g
        |""".stripMargin) shouldBe 1000
  }

  "i64 global read" in {
    compileAndRun(
      """var g: i64 = 50000
        |main() -> int = g
        |""".stripMargin) shouldBe 50000
  }

  "i8 global compound assign" in {
    compileAndRun(
      """var g: byte = 10
        |main() -> int
        |    g += 5
        |    g
        |""".stripMargin) shouldBe 15
  }

  "i16 global compound assign" in {
    compileAndRun(
      """var g: i16 = 100
        |main() -> int
        |    g += 50
        |    g
        |""".stripMargin) shouldBe 150
  }

  "global compound assign +=" in {
    compileAndRun(
      """var counter = 10
        |
        |main() -> int
        |    counter += 32
        |    counter
        |""".stripMargin) shouldBe 42
  }

  "global compound assign += multiple times" in {
    compileAndRun(
      """var counter = 0
        |
        |inc()
        |    counter += 1
        |
        |main() -> int
        |    inc()
        |    inc()
        |    inc()
        |    counter
        |""".stripMargin) shouldBe 3
  }

  "global assign from local" in {
    compileAndRun(
      """var result = 0
        |
        |main() -> int
        |    var x = 42
        |    result = x
        |    result
        |""".stripMargin) shouldBe 42
  }

  "global assign from expression" in {
    compileAndRun(
      """var current = 0
        |var count = 5
        |var result = 0
        |
        |main() -> int
        |    var next = current + 1
        |    if next >= count
        |        next = 0
        |    result = next
        |    result
        |""".stripMargin) shouldBe 1
  }

  "global read in arithmetic" in {
    compileAndRun(
      """var base = 40
        |
        |main() -> int
        |    var x = base + 2
        |    x
        |""".stripMargin) shouldBe 42
  }

  "compare local to global" in {
    compileAndRun(
      """var limit = 5
        |
        |main() -> int
        |    var i = 0
        |    var sum = 0
        |    while i < limit
        |        sum += 1
        |        i += 1
        |    sum
        |""".stripMargin) shouldBe 5
  }

  "compare local >= global with wrap" in {
    compileAndRun(
      """var thread_count = 3
        |
        |main() -> int
        |    var next = 2
        |    next += 1
        |    if next >= thread_count
        |        next = 0
        |    next
        |""".stripMargin) shouldBe 0
  }

  // ===== Global arrays =====

  "global array reserves correct space" in {
    compileAndRun(
      """var arr: [4]int
        |
        |main() -> int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    arr[3] = 40
        |    arr[0] + arr[1] + arr[2] + arr[3]
        |""".stripMargin) shouldBe 100
  }

  "global array does not overlap next global" in {
    compileAndRun(
      """var arr: [4]int
        |var sentinel = 99
        |
        |main() -> int
        |    arr[0] = 1
        |    arr[1] = 2
        |    arr[2] = 3
        |    arr[3] = 4
        |    sentinel
        |""".stripMargin) shouldBe 99
  }

  "global array address used in addr-of-index" in {
    compileAndRun(
      """var arr: [4]int
        |
        |main() -> int
        |    arr[2] = 42
        |    var p: *int = &arr[2]
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "global array literal indexing" in {
    compileAndRun(
      """data = [10, 20, 30]
        |main() -> int = data[1]
        |""".stripMargin) shouldBe 20
  }

  "global byte array literal uses db directives" in {
    compileAndRun(
      """vals: [4]byte = [0x10, 0x20, 0x30, 0x40]
        |main() -> int = vals[2]
        |""".stripMargin) shouldBe 0x30
  }

  "global byte array with values above 127" in {
    compileAndRun(
      """data: [3]byte = [0x80, 0xCC, 0xFF]
        |main() -> int = data[1] & 0xFF
        |""".stripMargin) shouldBe 0xCC
  }

  "global byte array sum with unsigned masking" in {
    compileAndRun(
      """data: [3]byte = [100, 200, 50]
        |main() -> int
        |    a = data[0] & 0xFF
        |    b = data[1] & 0xFF
        |    c = data[2] & 0xFF
        |    a + b + c
        |""".stripMargin) shouldBe 350
  }

  "multi-module globals remain aligned after linking" in {
    compileMultiAndRun(Map(
      "lib" ->
        """helper() -> int = 42
          |""".stripMargin,
      "app" ->
        """import "lib"
          |
          |val MAGIC = 12345
          |
          |main() -> int
          |    val v = MAGIC
          |    helper() + v
          |""".stripMargin
    )) shouldBe 12387
  }

  "sieve of eratosthenes" in {
    compileAndRun(
      """main() -> int
        |    arr: [101]byte
        |    for i = 0; i <= 100; i++
        |        arr[i] = 0
        |    arr[0] = 1
        |    arr[1] = 1
        |    for i = 2; i * i <= 100; i++
        |        if arr[i] == 0 then
        |            for j = i * i; j <= 100; j += i
        |                arr[j] = 1
        |    count = 0
        |    for i = 2; i <= 100; i++
        |        if arr[i] == 0 then count += 1
        |    count
        |""".stripMargin) shouldBe 25
  }
}
