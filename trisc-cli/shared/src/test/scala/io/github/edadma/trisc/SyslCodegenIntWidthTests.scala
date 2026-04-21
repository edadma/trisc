package io.github.edadma.trisc

class SyslCodegenIntWidthTests extends SyslCodegenHelpers {

  // ===== Variable declarations =====

  "i8 variable" in { compileAndRun("main() -> int\n    var x: i8 = 42\n    x\n") shouldBe 42 }
  "i16 variable" in { compileAndRun("main() -> int\n    var x: i16 = 1000\n    x\n") shouldBe 1000 }
  "i32 variable" in { compileAndRun("main() -> int\n    var x: i32 = 100000\n    x\n") shouldBe 100000 }
  "i64 variable" in { compileAndRun("main() -> int\n    var x: i64 = 100000\n    x\n") shouldBe 100000 }

  "int is alias for i32" in {
    compileAndRun("main() -> int\n    var x: int = 42\n    var y: i32 = x\n    y\n") shouldBe 42
  }

  "char is alias for u32" in {
    compileAndRun("main() -> int\n    var x: char = 65\n    var y: u32 = x\n    int(y)\n") shouldBe 65
  }

  // ===== Function parameter/return with widths =====

  "i16 parameter" in {
    compileAndRun("dbl(x: i16) -> i16 = x * 2\nmain() -> int = dbl(21)\n") shouldBe 42
  }

  "i32 return type" in {
    compileAndRun("get32() -> i32 = 100000\nmain() -> int = get32()\n") shouldBe 100000
  }

  // ===== Compound types with iN =====

  "pointer to i32" in {
    compileAndRun("main() -> int\n    var x: i32 = 42\n    var p: *i32 = &x\n    *p\n") shouldBe 42
  }

  "array of i16" in {
    compileAndRun(
      """main() -> int
        |    var arr: [3]i16
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 60
  }

  "pointer to i8" in {
    compileAndRun("main() -> int\n    var x: i8 = 99\n    var p: *i8 = &x\n    *p\n") shouldBe 99
  }

  // ===== sizeof =====

  "sizeof(i8)" in { compileAndRun("main() -> int = sizeof(i8)\n") shouldBe 1 }
  "sizeof(i16)" in { compileAndRun("main() -> int = sizeof(i16)\n") shouldBe 2 }
  "sizeof(i32)" in { compileAndRun("main() -> int = sizeof(i32)\n") shouldBe 4 }
  "sizeof(i64)" in { compileAndRun("main() -> int = sizeof(i64)\n") shouldBe 8 }
  "sizeof(*i32)" in { compileAndRun("main() -> int = sizeof(*i32)\n") shouldBe 8 }
  "sizeof([5]i16)" in { compileAndRun("main() -> int = sizeof([5]i16)\n") shouldBe 10 }
  "sizeof([10]i8)" in { compileAndRun("main() -> int = sizeof([10]i8)\n") shouldBe 10 }

  // ===== Cast with iN types =====

  "i8 cast" in { compileAndRun("main() -> int = i8(256)\n") shouldBe 0 }
  "i8 cast preserves low bits" in { compileAndRun("main() -> int = i8(0xFF)\n") shouldBe -1 }
  "i16 cast" in { compileAndRun("main() -> int = i16(0x10000)\n") shouldBe 0 }
  "i16 cast preserves low bits" in { compileAndRun("main() -> int = i16(0xFFFF)\n") shouldBe -1 }
  "i32 cast" in { compileAndRun("main() -> int = i32(65)\n") shouldBe 65 }
  "i64 cast is identity" in { compileAndRun("main() -> int = i64(42)\n") shouldBe 42 }
  "i8 cast truncation" in { compileAndRun("main() -> int = i8(300)\n") shouldBe 44 }
  "i16 cast truncation" in { compileAndRun("main() -> int = i16(70000)\n") shouldBe 4464 }

  // ===== Cross-width assignment =====

  "assign i8 to i64 variable" in {
    compileAndRun("main() -> int\n    var x: i8 = 42\n    var y: i64 = x\n    y\n") shouldBe 42
  }

  "assign i16 to i32 variable" in {
    compileAndRun("main() -> int\n    var x: i16 = 1000\n    var y: i32 = x\n    y\n") shouldBe 1000
  }

  "assign i64 to i8 with explicit cast" in {
    compileAndRun("main() -> int\n    var x: i64 = 42\n    var y: i8 = i8(x)\n    y\n") shouldBe 42
  }

  // ===== Arithmetic with widths =====

  "i16 arithmetic" in {
    compileAndRun("main() -> int\n    var a: i16 = 100\n    var b: i16 = 200\n    a + b\n") shouldBe 300
  }

  "i32 arithmetic" in {
    compileAndRun("main() -> int\n    var a: i32 = 100000\n    var b: i32 = 200000\n    a + b\n") shouldBe 300000
  }

  // ===== int parameter passing =====

  "int parameter" in {
    compileAndRun("add1(x: int) -> int = x + 1\nmain() -> int = add1(41)\n") shouldBe 42
  }

  "int parameter negative" in {
    compileAndRun("negate(x: int) -> int = -x\nmain() -> int = negate(-42)\n") shouldBe 42
  }

  "i8 parameter" in {
    compileAndRun("id8(x: i8) -> int = x\nmain() -> int = id8(99)\n") shouldBe 99
  }

  "multiple int params on stack" in {
    compileAndRun(
      """sum3(a: int, b: int, c: int) -> int = a + b + c
        |main() -> int = sum3(10, 20, 12)
        |""".stripMargin) shouldBe 42
  }

  // ===== int in struct fields =====

  "struct with int fields" in {
    compileAndRun(
      """struct Pair
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p = Pair(20, 22)
        |    p.x + p.y
        |""".stripMargin) shouldBe 42
  }

  "struct int field write and read" in {
    compileAndRun(
      """struct Box
        |    v: int
        |
        |main() -> int
        |    var b = Box(0)
        |    b.v = 42
        |    b.v
        |""".stripMargin) shouldBe 42
  }

  "struct with mixed int widths" in {
    compileAndRun(
      """struct Mixed
        |    a: i8
        |    b: i16
        |    c: int
        |    d: i64
        |
        |main() -> int
        |    m = Mixed(1, 2, 3, 4)
        |    m.a + m.b + m.c + m.d
        |""".stripMargin) shouldBe 10
  }

  // ===== int in arrays =====

  "array of int element access" in {
    compileAndRun(
      """main() -> int
        |    var arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 12
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 42
  }

  // ===== int global variables =====

  "int global variable" in {
    compileAndRun(
      """var g: int = 42
        |main() -> int = g
        |""".stripMargin) shouldBe 42
  }

  "int global variable negative" in {
    compileAndRun(
      """var g: int = -1
        |main() -> int = -g
        |""".stripMargin) shouldBe 1
  }

  // ===== int return values =====

  "int return value" in {
    compileAndRun("get42() -> int = 42\nmain() -> int = get42()\n") shouldBe 42
  }

  "int return value negative" in {
    compileAndRun("neg() -> int = -1\nmain() -> int = neg()\n") shouldBe -1
  }

  // ===== pointer to int roundtrip =====

  "write and read int through pointer" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 0
        |    var p: *int = &x
        |    *p = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  // ===== int in index assignment =====

  "int array index assign" in {
    compileAndRun(
      """main() -> int
        |    var arr: [2]int
        |    arr[0] = 100
        |    arr[1] = arr[0] + 42
        |    arr[1]
        |""".stripMargin) shouldBe 142
  }

  // ===== long / ulong / uint aliases =====

  "long variable" in {
    compileAndRun("main() -> int\n    var x: long = 100000\n    x\n") shouldBe 100000
  }

  "long is alias for i64" in {
    compileAndRun("main() -> int\n    var x: long = 42\n    var y: i64 = x\n    y\n") shouldBe 42
  }

  "ulong variable" in {
    compileAndRun("main() -> int\n    var x: ulong = 100000\n    x\n") shouldBe 100000
  }

  "ulong is alias for u64" in {
    compileAndRun("main() -> int\n    var x: ulong = 42\n    var y: u64 = x\n    y\n") shouldBe 42
  }

  "uint variable" in {
    compileAndRun("main() -> int\n    var x: uint = 42\n    var y: u32 = x\n    y\n") shouldBe 42
  }

  "uint is alias for u32" in {
    compileAndRun("main() -> int\n    var x: u32 = 99\n    var y: uint = x\n    y\n") shouldBe 99
  }

  "long parameter" in {
    compileAndRun("id(x: long) -> long = x\nmain() -> int = id(42)\n") shouldBe 42
  }

  "uint parameter" in {
    compileAndRun("id(x: uint) -> int = x\nmain() -> int = id(42)\n") shouldBe 42
  }

  "sizeof(long)" in { compileAndRun("main() -> int = sizeof(long)\n") shouldBe 8 }
  "sizeof(ulong)" in { compileAndRun("main() -> int = sizeof(ulong)\n") shouldBe 8 }
  "sizeof(uint)" in { compileAndRun("main() -> int = sizeof(uint)\n") shouldBe 4 }

  "long cast" in { compileAndRun("main() -> int = long(42)\n") shouldBe 42 }
  "ulong cast" in { compileAndRun("main() -> int = ulong(42)\n") shouldBe 42 }
  "uint cast" in { compileAndRun("main() -> int = uint(42)\n") shouldBe 42 }

  // ===== u64 struct field (value > 2^32) =====

  "u64 struct field stores and reads value > 2^32" in {
    compileAndRun(
      """struct W
        |    v: u64
        |
        |main() -> i64
        |    w = W(0x1_0000_0000)
        |    w.v
        |""".stripMargin) shouldBe 0x100000000L
  }

  "i64 literal auto-promoted" in {
    compileAndRun(
      """main() -> i64
        |    var x: i64 = 0x1_0000_0000
        |    x
        |""".stripMargin) shouldBe 0x100000000L
  }
}
