package io.github.edadma.trisc

class SyslSVMExprTests extends SyslSVMCodegenHelpers {

  // --- Integer Literals ---
  "literal 0" in { compileAndRun("main() -> int = 0\n") shouldBe 0 }
  "literal 1" in { compileAndRun("main() -> int = 1\n") shouldBe 1 }
  "literal 42" in { compileAndRun("main() -> int = 42\n") shouldBe 42 }
  "literal -1" in { compileAndRun("main() -> int = -1\n") shouldBe -1 }
  "literal large" in { compileAndRun("main() -> int = 100000\n") shouldBe 100000 }

  // --- Arithmetic ---
  "addition" in { compileAndRun("main() -> int = 3 + 4\n") shouldBe 7 }
  "subtraction" in { compileAndRun("main() -> int = 10 - 3\n") shouldBe 7 }
  "multiplication" in { compileAndRun("main() -> int = 6 * 7\n") shouldBe 42 }
  "division" in { compileAndRun("main() -> int = 84 / 2\n") shouldBe 42 }
  "modulo" in { compileAndRun("main() -> int = 47 % 10\n") shouldBe 7 }
  "nested arithmetic" in { compileAndRun("main() -> int = (3 + 4) * 6\n") shouldBe 42 }
  "precedence" in { compileAndRun("main() -> int = 3 + 4 * 6\n") shouldBe 27 }

  // --- Comparison ---
  "equal true" in { compileAndRun("main() -> int = if 3 == 3 then 1 else 0\n") shouldBe 1 }
  "equal false" in { compileAndRun("main() -> int = if 3 == 4 then 1 else 0\n") shouldBe 0 }
  "not equal" in { compileAndRun("main() -> int = if 3 != 4 then 1 else 0\n") shouldBe 1 }
  "less than" in { compileAndRun("main() -> int = if 3 < 4 then 1 else 0\n") shouldBe 1 }
  "greater than" in { compileAndRun("main() -> int = if 4 > 3 then 1 else 0\n") shouldBe 1 }
  "less equal" in { compileAndRun("main() -> int = if 3 <= 3 then 1 else 0\n") shouldBe 1 }
  "greater equal" in { compileAndRun("main() -> int = if 3 >= 4 then 1 else 0\n") shouldBe 0 }

  // --- Bitwise ---
  "bitwise and" in { compileAndRun("main() -> int = 0xFF & 0x0F\n") shouldBe 0x0F }
  "bitwise or" in { compileAndRun("main() -> int = 0xF0 | 0x0F\n") shouldBe 0xFF }
  "bitwise xor" in { compileAndRun("main() -> int = 0xFF ^ 0x0F\n") shouldBe 0xF0 }
  "shift left" in { compileAndRun("main() -> int = 1 << 8\n") shouldBe 256 }
  "shift right" in { compileAndRun("main() -> int = 256 >> 4\n") shouldBe 16 }

  // --- Unary ---
  "negate" in { compileAndRun("main() -> int = -(42)\n") shouldBe -42 }
  "logical not true" in { compileAndRun("main() -> int = if !false then 1 else 0\n") shouldBe 1 }
  "logical not false" in { compileAndRun("main() -> int = if !true then 1 else 0\n") shouldBe 0 }
  "bitwise not" in { compileAndRun("main() -> int = ~0\n") shouldBe -1 }

  // --- Logical short-circuit ---
  "and true" in { compileAndRun("main() -> int = if true && true then 1 else 0\n") shouldBe 1 }
  "and false" in { compileAndRun("main() -> int = if true && false then 1 else 0\n") shouldBe 0 }
  "or true" in { compileAndRun("main() -> int = if false || true then 1 else 0\n") shouldBe 1 }
  "or false" in { compileAndRun("main() -> int = if false || false then 1 else 0\n") shouldBe 0 }

  // --- Sizeof ---
  "sizeof int" in { compileAndRun("main() -> int = sizeof(int)\n") shouldBe 4 }
  "sizeof i64" in { compileAndRun("main() -> int = sizeof(i64)\n") shouldBe 8 }
}
