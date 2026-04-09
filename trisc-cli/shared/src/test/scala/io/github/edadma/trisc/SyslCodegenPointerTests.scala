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

  // === &array pointer arithmetic (array decay) ===

  "addr-of array element plus offset" in {
    compileAndRun(
      """main() -> int
        |    var buf: [64]i8
        |    buf[10] = 42
        |    val p: *i8 = &buf[0] + 10
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "addr-of int array element plus offset" in {
    compileAndRun(
      """main() -> int
        |    var arr: [10]int
        |    arr[3] = 99
        |    val p: *int = &arr[0] + 3
        |    *p
        |""".stripMargin) shouldBe 99
  }

  "addr-of large i8 array element plus offset" in {
    compileAndRun(
      """main() -> int
        |    var buf: [512]i8
        |    buf[32] = 77
        |    val p: *i8 = &buf[0] + 32
        |    *p
        |""".stripMargin) shouldBe 77
  }

  "addr-of array element passed to function" in {
    compileAndRun(
      """read_byte(p: *i8, idx: int) -> int
        |    p[idx]
        |
        |main() -> int
        |    var buf: [32]i8
        |    buf[5] = 88
        |    read_byte(&buf[0], 5)
        |""".stripMargin) shouldBe 88
  }

  "addr-of global array element plus offset" in {
    compileAndRun(
      """var buf: [64]i8
        |
        |main() -> int
        |    buf[20] = 55
        |    val p: *i8 = &buf[0] + 20
        |    *p
        |""".stripMargin) shouldBe 55
  }

  "addr-of global large array element plus offset" in {
    compileAndRun(
      """var buf: [512]i8
        |
        |main() -> int
        |    buf[100] = 33
        |    val p: *i8 = &buf[0] + 100
        |    *p
        |""".stripMargin) shouldBe 33
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

  // ===== Pointer cast codegen =====

  "int-to-pointer cast and dereference" in {
    compileAndRun(
      """main() -> int
        |    var x: i64 = 42
        |    val addr = int(&x)
        |    var p: *i64 = *i64(addr)
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "int-to-pointer cast and store" in {
    compileAndRun(
      """main() -> int
        |    var x: i64 = 0
        |    val addr = int(&x)
        |    var p: *i64 = *i64(addr)
        |    *p = 99
        |    x
        |""".stripMargin) shouldBe 99
  }

  "store through inline pointer cast" in {
    compileAndRun(
      """main() -> int
        |    var x: i64 = 0
        |    val addr = int(&x)
        |    *(*i64(addr)) = 77
        |    x
        |""".stripMargin) shouldBe 77
  }

  "store through inline pointer cast i8" in {
    compileAndRun(
      """main() -> int
        |    var buf: [8]i8
        |    buf[0] = 0
        |    val addr = int(&buf)
        |    *(*i8(addr)) = 42
        |    buf[0]
        |""".stripMargin) shouldBe 42
  }

  "pointer cast round-trip preserves address" in {
    compileAndRun(
      """main() -> int
        |    var x: i64 = 123
        |    val addr1 = int(&x)
        |    var p: *i64 = *i64(addr1)
        |    val addr2 = int(p)
        |    if addr1 == addr2 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "function ref as i64 argument" in {
    compileAndRun(
      """target() -> int = 42
        |take_addr(addr: i64) -> int
        |    if addr != 0 then 1 else 0
        |main() -> int = take_addr(target)
        |""".stripMargin) shouldBe 1
  }

  "null pointer literal 0 as pointer arg" in {
    compileAndRun(
      """check(p: *int) -> int
        |    if int(p) == 0
        |        return 99
        |    0
        |main() -> int = check(0)
        |""".stripMargin) shouldBe 99
  }

  "string as *i8 argument" in {
    compileAndRun(
      """first(s: *i8) -> int = s[0]
        |main() -> int = first("Hello")
        |""".stripMargin) shouldBe 72
  }

  "build stack frame pattern: store through cast" in {
    compileAndRun(
      """main() -> int
        |    var buf: [4]i64
        |    var sp = int(&buf) + 32
        |    sp -= 8
        |    *(*i64(sp)) = 10
        |    sp -= 8
        |    *(*i64(sp)) = 20
        |    sp -= 8
        |    *(*i64(sp)) = 30
        |    sp -= 8
        |    *(*i64(sp)) = 40
        |    buf[0] + buf[1] + buf[2] + buf[3]
        |""".stripMargin) shouldBe 100
  }

  "single-char double-quoted string as *i8 arg" in {
    compileAndRun(
      """first(s: *i8) -> int = s[0]
        |main() -> int = first("A")
        |""".stripMargin) shouldBe 65
  }

  "char literal vs string literal distinction" in {
    compileAndRun(
      """main() -> int
        |    val c = 'A'
        |    c
        |""".stripMargin) shouldBe 65
  }

  // ===== *p++ and *++p semantics =====

  "*p++ dereferences then advances" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    var p = &arr[0]
        |    val a = *p++
        |    val b = *p
        |    a * 100 + b
        |""".stripMargin) shouldBe 1020
  }

  "*++p advances then dereferences" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    var p = &arr[0]
        |    val a = *++p
        |    val b = *p
        |    a * 100 + b
        |""".stripMargin) shouldBe 2020
  }

  "*p-- dereferences then retreats" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    var p = &arr[2]
        |    val a = *p--
        |    val b = *p
        |    a * 100 + b
        |""".stripMargin) shouldBe 3020
  }

  "*--p retreats then dereferences" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    var p = &arr[2]
        |    val a = *--p
        |    val b = *p
        |    a * 100 + b
        |""".stripMargin) shouldBe 2020
  }

  "*dst++ = *src++ copies and advances both" in {
    compileAndRun(
      """main() -> int
        |    src: [3]int
        |    dst: [3]int
        |    src[0] = 10
        |    src[1] = 20
        |    src[2] = 30
        |    var s = &src[0]
        |    var d = &dst[0]
        |    *d++ = *s++
        |    *d++ = *s++
        |    *d++ = *s++
        |    dst[0] * 10000 + dst[1] * 100 + dst[2]
        |""".stripMargin) shouldBe 102030
  }

  "*p++ on byte array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]byte
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    var p = &arr[0]
        |    val a = *p++
        |    val b = *p++
        |    val c = *p
        |    a + b + c
        |""".stripMargin) shouldBe 60
  }

  // ===== TFieldAccess on pointer-to-struct =====

  "read field through pointer" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |main() -> int
        |    var pt = Point(10, 32)
        |    val p: *Point = &pt
        |    p.x + p.y
        |""".stripMargin) shouldBe 42
  }

  "write field through pointer" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |main() -> int
        |    var pt = Point(0, 0)
        |    val p: *Point = &pt
        |    p.x = 20
        |    p.y = 22
        |    pt.x + pt.y
        |""".stripMargin) shouldBe 42
  }

  "pointer field access in function" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |sum_point(p: *Point) -> int = p.x + p.y
        |main() -> int
        |    var pt = Point(20, 22)
        |    sum_point(&pt)
        |""".stripMargin) shouldBe 42
  }

  "pointer walk with *p++ in loop" in {
    compileAndRun(
      """main() -> int
        |    arr: [4]int
        |    arr[0] = 1
        |    arr[1] = 2
        |    arr[2] = 3
        |    arr[3] = 4
        |    var p = &arr[0]
        |    var sum = 0
        |    var i = 0
        |    while i < 4
        |        sum += *p++
        |        i++
        |    sum
        |""".stripMargin) shouldBe 10
  }
}
