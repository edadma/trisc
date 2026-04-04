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
}
