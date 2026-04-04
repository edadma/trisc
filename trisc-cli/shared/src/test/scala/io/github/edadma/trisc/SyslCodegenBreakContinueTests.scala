package io.github.edadma.trisc

class SyslCodegenBreakContinueTests extends SyslCodegenHelpers {

  // ===== break =====

  "break exits while loop" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    while true
        |        if i == 5 then break
        |        i += 1
        |    i
        |""".stripMargin) shouldBe 5
  }

  "break exits loop immediately" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    while i < 100
        |        break
        |        i += 1
        |    i
        |""".stripMargin) shouldBe 0
  }

  "break in nested if block" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    while true
        |        if i >= 3
        |            break
        |        i += 1
        |    i
        |""".stripMargin) shouldBe 3
  }

  "break only exits innermost loop" in {
    compileAndRun(
      """main() -> int
        |    total = 0
        |    i = 0
        |    while i < 3
        |        j = 0
        |        while true
        |            if j == 2 then break
        |            j += 1
        |        total = total + j
        |        i += 1
        |    total
        |""".stripMargin) shouldBe 6
  }

  // ===== continue =====

  "continue re-evaluates condition" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    count = 0
        |    while i < 10
        |        i += 1
        |        if i % 2 == 0 then continue
        |        count += 1
        |    count
        |""".stripMargin) shouldBe 5
  }

  // ===== break and continue together =====

  "break and continue in same loop" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    sum = 0
        |    while true
        |        i += 1
        |        if i > 5 then break
        |        if i % 2 == 0 then continue
        |        sum += i
        |    sum
        |""".stripMargin) shouldBe 9
  }

  // ===== Practical =====

  "find first element matching condition" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 35
        |    arr[3] = 40
        |    arr[4] = 50
        |    i = 0
        |    found = -1
        |    while i < 5
        |        if arr[i] > 30
        |            found = arr[i]
        |            break
        |        i += 1
        |    found
        |""".stripMargin) shouldBe 35
  }

  "sum until threshold with break" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    i = 1
        |    while true
        |        sum = sum + i
        |        if sum > 10 then break
        |        i += 1
        |    sum
        |""".stripMargin) shouldBe 15
  }

  "count non-zero with continue" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]int
        |    arr[0] = 1
        |    arr[1] = 0
        |    arr[2] = 3
        |    arr[3] = 0
        |    arr[4] = 5
        |    count = 0
        |    i = 0
        |    while i < 5
        |        if arr[i] == 0
        |            i += 1
        |            continue
        |        count += 1
        |        i += 1
        |    count
        |""".stripMargin) shouldBe 3
  }
}
