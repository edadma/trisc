package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslBreakContinueTests extends SyslTestHelpers {

  // ===== break =====

  "break exits while loop" in {
    eval(
      """main() -> int
        |    i = 0
        |    while true
        |        if i == 5 then break
        |        i += 1
        |    i
        |""".stripMargin) shouldBe 5
  }

  "break exits loop immediately" in {
    eval(
      """main() -> int
        |    i = 0
        |    while i < 100
        |        break
        |        i += 1
        |    i
        |""".stripMargin) shouldBe 0
  }

  "break in nested if block" in {
    eval(
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
    eval(
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

  "break with do syntax" in {
    eval(
      """main() -> int
        |    i = 0
        |    while true do
        |        if i == 4 then break
        |        i += 1
        |    i
        |""".stripMargin) shouldBe 4
  }

  // ===== continue =====

  "continue skips rest of loop body" in {
    output(
      """main() -> int
        |    i = 0
        |    while i < 5
        |        i += 1
        |        if i == 3 then continue
        |        print(i)
        |    0
        |""".stripMargin) shouldBe "1245"
  }

  "continue in nested loop" in {
    output(
      """main() -> int
        |    i = 0
        |    while i < 3
        |        i += 1
        |        j = 0
        |        while j < 3
        |            j += 1
        |            if j == 2 then continue
        |            print(j)
        |    0
        |""".stripMargin) shouldBe "131313"
  }

  "continue re-evaluates condition" in {
    eval(
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
    output(
      """main() -> int
        |    i = 0
        |    while true
        |        i += 1
        |        if i > 5 then break
        |        if i % 2 == 0 then continue
        |        print(i)
        |    0
        |""".stripMargin) shouldBe "135"
  }

  // ===== Analyzer validation =====

  "analyzer rejects break outside loop" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    break
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "analyzer rejects continue outside loop" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    continue
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "analyzer accepts break inside while" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    while true
        |        break
        |    0
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "analyzer accepts break inside nested if inside while" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    i = 0
        |    while true
        |        if i == 5 then break
        |        i += 1
        |    i
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  // ===== Practical usage =====

  "find first element matching condition" in {
    eval(
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
    eval(
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
    eval(
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
