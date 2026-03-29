package io.github.edadma.trisc

class SyslCodegenPointerTests extends SyslCodegenHelpers {

  "address-of and dereference" in {
    compileAndRun(
      """main() -> int
        |    x = 42
        |    p = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "write through pointer" in {
    compileAndRun(
      """main() -> int
        |    x = 10
        |    p = &x
        |    *p = 99
        |    x
        |""".stripMargin) shouldBe 99
  }

  "pointer to array element" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 100
        |    arr[1] = 200
        |    arr[2] = 300
        |    p = &arr[1]
        |    *p
        |""".stripMargin) shouldBe 200
  }

  "pointer increment" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    p = &arr[0]
        |    p++
        |    *p
        |""".stripMargin) shouldBe 20
  }

  "pointer deref i8" in {
    compileAndRun(
      """main() -> int
        |    var x: byte = 42
        |    var p: *byte = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "pointer deref i16" in {
    compileAndRun(
      """main() -> int
        |    var x: i16 = 1234
        |    var p: *i16 = &x
        |    *p
        |""".stripMargin) shouldBe 1234
  }

  "pointer deref i32" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 56789
        |    var p: *int = &x
        |    *p
        |""".stripMargin) shouldBe 56789
  }

  "write through i8 pointer" in {
    compileAndRun(
      """main() -> int
        |    var x: byte = 0
        |    var p: *byte = &x
        |    *p = 77
        |    x
        |""".stripMargin) shouldBe 77
  }

  "write through i16 pointer" in {
    compileAndRun(
      """main() -> int
        |    var x: i16 = 0
        |    var p: *i16 = &x
        |    *p = 999
        |    x
        |""".stripMargin) shouldBe 999
  }

  "write through i32 pointer" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 0
        |    var p: *int = &x
        |    *p = 99
        |    x
        |""".stripMargin) shouldBe 99
  }

  "write through i64 pointer" in {
    compileAndRun(
      """main() -> int
        |    var x: i64 = 0
        |    var p: *i64 = &x
        |    *p = 12345
        |    x
        |""".stripMargin) shouldBe 12345
  }

  "pointer increment i8 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]byte
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    p = &arr[0]
        |    p++
        |    *p
        |""".stripMargin) shouldBe 20
  }

  "pointer increment i16 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]i16
        |    arr[0] = 100
        |    arr[1] = 200
        |    arr[2] = 300
        |    p = &arr[0]
        |    p++
        |    *p
        |""".stripMargin) shouldBe 200
  }

  "pointer increment i32 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    p = &arr[0]
        |    p++
        |    *p
        |""".stripMargin) shouldBe 20
  }

  "pointer increment i64 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]i64
        |    arr[0] = 100
        |    arr[1] = 200
        |    arr[2] = 300
        |    p = &arr[0]
        |    p++
        |    *p
        |""".stripMargin) shouldBe 200
  }

  "pointer arithmetic add i32" in {
    compileAndRun(
      """main() -> int
        |    arr: [4]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    arr[3] = 40
        |    p = &arr[0]
        |    val q: *int = p + 2
        |    *q
        |""".stripMargin) shouldBe 30
  }

  "pointer decrement i32 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    p = &arr[2]
        |    p--
        |    *p
        |""".stripMargin) shouldBe 20
  }

  "addr-of i32 array element" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 100
        |    arr[1] = 200
        |    arr[2] = 300
        |    p = &arr[2]
        |    *p
        |""".stripMargin) shouldBe 300
  }

  "compound assign i8 local" in {
    compileAndRun(
      """main() -> int
        |    var x: byte = 5
        |    x += 3
        |    x
        |""".stripMargin) shouldBe 8
  }

  "compound assign i16 local" in {
    compileAndRun(
      """main() -> int
        |    var x: i16 = 100
        |    x += 50
        |    x
        |""".stripMargin) shouldBe 150
  }

  "compound assign i32 local" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 100
        |    x *= 3
        |    x
        |""".stripMargin) shouldBe 300
  }

  "multiple compound assigns mixed widths" in {
    compileAndRun(
      """main() -> int
        |    var a: byte = 10
        |    var b: int = 20
        |    a += 5
        |    b += 10
        |    a + b
        |""".stripMargin) shouldBe 45
  }

  "addr-of global scalar" in {
    compileAndRun(
      """var x = 42
        |
        |main() -> int
        |    var p: *int = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "addr-of global and write through pointer" in {
    compileAndRun(
      """var x = 0
        |
        |main() -> int
        |    var p: *int = &x
        |    *p = 42
        |    x
        |""".stripMargin) shouldBe 42
  }
}
