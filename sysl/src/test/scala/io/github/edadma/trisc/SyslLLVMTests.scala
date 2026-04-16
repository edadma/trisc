package io.github.edadma.trisc

class SyslLLVMTests extends SyslLLVMTestHelpers {

  // ===== Basic return values =====

  "main returns 0" in {
    llvmExit("main() -> int\n  0\n") shouldBe 0
  }

  "main returns 42" in {
    llvmExit("main() -> int\n  42\n") shouldBe 42
  }

  "main returns expression" in {
    llvmExit("main() -> int\n  10 + 20 + 12\n") shouldBe 42
  }

  // ===== Arithmetic =====

  "addition" in {
    llvmExit("main() -> int\n  3 + 4\n") shouldBe 7
  }

  "subtraction" in {
    llvmExit("main() -> int\n  10 - 3\n") shouldBe 7
  }

  "multiplication" in {
    llvmExit("main() -> int\n  6 * 7\n") shouldBe 42
  }

  "division" in {
    llvmExit("main() -> int\n  84 / 2\n") shouldBe 42
  }

  "modulo" in {
    llvmExit("main() -> int\n  10 % 3\n") shouldBe 1
  }

  // ===== Variables =====

  "variable declaration and use" in {
    llvmExit("main() -> int\n  x = 42\n  x\n") shouldBe 42
  }

  "variable assignment" in {
    llvmExit("main() -> int\n  var x = 1\n  x = 42\n  x\n") shouldBe 42
  }

  // ===== Functions =====

  "function call" in {
    llvmExit("dbl(x: int) -> int = x * 2\nmain() -> int\n  dbl(21)\n") shouldBe 42
  }

  "function with two params" in {
    llvmExit("add(a: int, b: int) -> int = a + b\nmain() -> int\n  add(20, 22)\n") shouldBe 42
  }

  // ===== String output =====

  "puts prints string" in {
    llvmOutput("main() -> int\n  puts(\"hello\")\n  0\n") shouldBe "hello"
  }

  "println prints integer" in {
    llvmOutput("main() -> int\n  println(42)\n  0\n") shouldBe "42"
  }

  "multiple puts" in {
    llvmOutput("main() -> int\n  puts(\"hello\")\n  puts(\"world\")\n  0\n") shouldBe "hello\nworld"
  }

  // ===== String interpolation =====

  "interpolate integer" in {
    llvmOutput("main() -> int\n  x = 42\n  puts(s\"x = $x\")\n  0\n") shouldBe "x = 42"
  }

  "interpolate float" in {
    llvmOutput("main() -> int\n  pi = 3.14\n  puts(s\"pi = $pi\")\n  0\n") shouldBe "pi = 3.14"
  }

  "interpolate expression" in {
    llvmOutput("main() -> int\n  x = 5\n  puts(s\"${x + 1}\")\n  0\n") shouldBe "6"
  }

  "interpolate multiple values" in {
    llvmOutput("main() -> int\n  a = 10\n  b = 20\n  puts(s\"$a and $b\")\n  0\n") shouldBe "10 and 20"
  }

  "interpolate mixed int and float" in {
    llvmOutput("main() -> int\n  n = 3\n  x = 1.5\n  puts(s\"$n times $x\")\n  0\n") shouldBe "3 times 1.5"
  }

  // ===== Control flow =====

  "if expression true branch" in {
    llvmExit("main() -> int\n  if true\n    42\n  else\n    0\n") shouldBe 42
  }

  "if expression false branch" in {
    llvmExit("main() -> int\n  if false\n    0\n  else\n    42\n") shouldBe 42
  }

  "while loop" in {
    llvmOutput("main() -> int\n  var i = 0\n  while i < 3\n    println(i)\n    i = i + 1\n  0\n") shouldBe "0\n1\n2"
  }

  // ===== Comparisons =====

  "less than true" in {
    llvmExit("main() -> int\n  if 1 < 2\n    1\n  else\n    0\n") shouldBe 1
  }

  "less than false" in {
    llvmExit("main() -> int\n  if 2 < 1\n    1\n  else\n    0\n") shouldBe 0
  }

  "equality true" in {
    llvmExit("main() -> int\n  if 42 == 42\n    1\n  else\n    0\n") shouldBe 1
  }

  "equality false" in {
    llvmExit("main() -> int\n  if 42 == 43\n    1\n  else\n    0\n") shouldBe 0
  }

  // ===== Regression: val initializers with non-sequential values =====

  "val with non-sequential values" in {
    llvmOutput(
      """val A = 0
        |val B = 2
        |val C = 5
        |val D = 9
        |
        |main() -> int
        |  println(A)
        |  println(B)
        |  println(C)
        |  println(D)
        |  0
        |""".stripMargin
    ) shouldBe "0\n2\n5\n9"
  }

  "val with non-sequential values cross-unit" in {
    llvmOutputMulti(Map(
      "consts/consts" ->
        """module consts
          |
          |val STATE_READY = 0
          |val STATE_RUNNING = 2
          |val STATE_TERMINATED = 3
          |val STATE_BLOCKED = 5
          |val STATE_SUSPENDED = 9
          |""".stripMargin,
      "main" ->
        """import consts.*
          |
          |main() -> int
          |  println(STATE_READY)
          |  println(STATE_RUNNING)
          |  println(STATE_TERMINATED)
          |  println(STATE_BLOCKED)
          |  println(STATE_SUSPENDED)
          |  0
          |""".stripMargin
    )) shouldBe "0\n2\n3\n5\n9"
  }

  // ===== Regression: void calls before final return expression =====

  "void call after if-return not dropped" in {
    llvmOutput(
      """foo(x: int) -> int
        |  if x > 0
        |    return 1
        |  puts("tail")
        |  0
        |
        |main() -> int
        |  foo(0)
        |  0
        |""".stripMargin
    ) shouldBe "tail"
  }

  "void call after multiple if-returns not dropped" in {
    llvmOutput(
      """var flag: int
        |
        |set_flag(v: int)
        |  flag = v
        |
        |check(a: int, b: int) -> int
        |  if a > 10
        |    return 1
        |  if b > 10
        |    return 2
        |  set_flag(99)
        |  0
        |
        |main() -> int
        |  println(check(0, 0))
        |  println(flag)
        |  0
        |""".stripMargin
    ) shouldBe "0\n99"
  }
}
