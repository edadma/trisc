package io.github.edadma.trisc

class SyslCodegenCastTests extends SyslCodegenHelpers {

  // ===== int() cast =====

  "int from char" in { compileAndRun("main() -> int = int('A')\n") shouldBe 65 }
  "int from bool true" in { compileAndRun("main() -> int = int(true)\n") shouldBe 1 }
  "int from bool false" in { compileAndRun("main() -> int = int(false)\n") shouldBe 0 }
  "int from comparison" in { compileAndRun("main() -> int = int(3 < 5)\n") shouldBe 1 }

  "int from byte" in {
    compileAndRun("main() -> int = int(byte(200))\n") shouldBe 200 // byte is unsigned u8
  }

  // ===== char() cast =====

  "char from int" in { compileAndRun("main() -> int = char(65)\n") shouldBe 65 }

  // ===== byte() cast =====

  "byte from int" in { compileAndRun("main() -> int = byte(0x1FF)\n") shouldBe 255 }
  "byte truncates to 8 bits" in { compileAndRun("main() -> int = byte(256)\n") shouldBe 0 }

  // ===== bool() cast =====

  "bool from zero is false" in { compileAndRun("main() -> int = if bool(0) then 1 else 0\n") shouldBe 0 }
  "bool from nonzero is true" in { compileAndRun("main() -> int = if bool(42) then 1 else 0\n") shouldBe 1 }
  "bool from negative is true" in { compileAndRun("main() -> int = if bool(-1) then 1 else 0\n") shouldBe 1 }

  // ===== Cast in expressions =====

  "cast in arithmetic" in { compileAndRun("main() -> int = int(true) + int(true)\n") shouldBe 2 }

  "cast in condition" in {
    compileAndRun(
      """main() -> int
        |    x = 42
        |    if bool(x) then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "cast in function argument" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |main() -> int = dbl(int('!'))
        |""".stripMargin) shouldBe 66
  }

  "chained casts" in { compileAndRun("main() -> int = int(bool(42))\n") shouldBe 1 }

  // ===== Practical =====

  "use bool cast for C-style truthiness" in {
    compileAndRun(
      """main() -> int
        |    values: [3]int
        |    values[0] = 0
        |    values[1] = 42
        |    values[2] = 0
        |    count = 0
        |    i = 0
        |    while i < 3
        |        count = count + int(bool(values[i]))
        |        i += 1
        |    count
        |""".stripMargin) shouldBe 1
  }
}
