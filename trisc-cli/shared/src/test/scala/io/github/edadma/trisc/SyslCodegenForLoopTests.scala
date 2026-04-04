package io.github.edadma.trisc

class SyslCodegenForLoopTests extends SyslCodegenHelpers {

  // ===== Basic for loop =====

  "basic for loop" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 5; i++
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "for loop with typed init" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i: int = 0; i < 5; i++
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "for loop with compound assignment update" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 10; i += 2
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 20
  }

  "for loop with pre-increment update" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 5; ++i
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  // ===== break in for =====

  "break exits for loop" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 100; i++
        |        if i == 5 then break
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "break with do syntax" in {
    compileAndRun(
      """main() -> int
        |    last = 0
        |    for i = 1; i < 100; i++
        |        last = i
        |        if i == 3 then break
        |    last
        |""".stripMargin) shouldBe 3
  }

  // ===== continue in for =====

  "continue does not skip update" in {
    compileAndRun(
      """main() -> int
        |    count = 0
        |    for i = 0; i < 10; i++
        |        if i < 5 then continue
        |        count += 1
        |    count
        |""".stripMargin) shouldBe 5
  }

  // ===== Nested for loops =====

  "nested for loops sum" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 3; i++
        |        for j = 0; j < 3; j++
        |            sum += i * 3 + j
        |    sum
        |""".stripMargin) shouldBe 36
  }

  // ===== Practical =====

  "sum of squares" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i = 1; i <= 5; i++
        |        sum = sum + i * i
        |    sum
        |""".stripMargin) shouldBe 55
  }

  "factorial with for" in {
    compileAndRun(
      """main() -> int
        |    result = 1
        |    for i = 1; i <= 10; i++
        |        result = result * i
        |    result
        |""".stripMargin) shouldBe 3628800
  }

  "for loop with array" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]int
        |    for i = 0; i < 5; i++
        |        arr[i] = i * i
        |    arr[3]
        |""".stripMargin) shouldBe 9
  }

  "for loop zero iterations" in {
    compileAndRun(
      """main() -> int
        |    count = 0
        |    for i = 0; i < 0; i++
        |        count += 1
        |    count
        |""".stripMargin) shouldBe 0
  }

  "for with if then break" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i = 1; true; i++
        |        if i > 5 then break
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 15
  }
}
