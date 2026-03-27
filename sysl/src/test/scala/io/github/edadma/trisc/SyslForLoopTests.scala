package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslForLoopTests extends SyslTestHelpers {

  // ===== Basic for loop =====

  "basic for loop" in {
    eval(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 5; i++
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "for loop with typed init" in {
    eval(
      """main() -> int
        |    sum = 0
        |    for i: int = 0; i < 5; i++
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "for loop with compound assignment update" in {
    eval(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 10; i += 2
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 20
  }

  "for loop with pre-increment update" in {
    eval(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 5; ++i
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "for loop counting down" in {
    output(
      """main() -> int
        |    for i = 5; i > 0; i -= 1
        |        print(i)
        |    0
        |""".stripMargin) shouldBe "54321"
  }

  // ===== do syntax =====

  "for loop with do inline" in {
    output(
      """main() -> int
        |    for i = 0; i < 5; i++ do print(i)
        |    0
        |""".stripMargin) shouldBe "01234"
  }

  "for loop with do block" in {
    output(
      """main() -> int
        |    for i = 0; i < 3; i++ do
        |        print(i)
        |    0
        |""".stripMargin) shouldBe "012"
  }

  // ===== break in for =====

  "break exits for loop" in {
    eval(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 100; i++
        |        if i == 5 then break
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "break with do syntax" in {
    eval(
      """main() -> int
        |    last = 0
        |    for i = 1; i < 100; i++
        |        last = i
        |        if i == 3 then break
        |    last
        |""".stripMargin) shouldBe 3
  }

  // ===== continue in for =====

  "continue skips body but runs update" in {
    output(
      """main() -> int
        |    for i = 0; i < 6; i++
        |        if i % 2 == 0 then continue
        |        print(i)
        |    0
        |""".stripMargin) shouldBe "135"
  }

  "continue does not skip update" in {
    eval(
      """main() -> int
        |    count = 0
        |    for i = 0; i < 10; i++
        |        if i < 5 then continue
        |        count += 1
        |    count
        |""".stripMargin) shouldBe 5
  }

  // ===== Nested for loops =====

  "nested for loops" in {
    output(
      """main() -> int
        |    for i = 0; i < 3; i++
        |        for j = 0; j < 3; j++
        |            print(i * 3 + j)
        |    0
        |""".stripMargin) shouldBe "012345678"
  }

  "break only exits inner for loop" in {
    output(
      """main() -> int
        |    for i = 0; i < 3; i++
        |        for j = 0; j < 10; j++
        |            if j == 2 then break
        |            print(j)
        |    0
        |""".stripMargin) shouldBe "010101"
  }

  // ===== Practical =====

  "sum of squares" in {
    eval(
      """main() -> int
        |    sum = 0
        |    for i = 1; i <= 5; i++
        |        sum = sum + i * i
        |    sum
        |""".stripMargin) shouldBe 55
  }

  "factorial with for" in {
    eval(
      """main() -> int
        |    result = 1
        |    for i = 1; i <= 10; i++
        |        result = result * i
        |    result
        |""".stripMargin) shouldBe 3628800
  }

  "for loop with array" in {
    eval(
      """main() -> int
        |    arr: [5]int
        |    for i = 0; i < 5; i++
        |        arr[i] = i * i
        |    arr[3]
        |""".stripMargin) shouldBe 9
  }

  "for loop zero iterations" in {
    eval(
      """main() -> int
        |    count = 0
        |    for i = 0; i < 0; i++
        |        count += 1
        |    count
        |""".stripMargin) shouldBe 0
  }

  // ===== Analyzer validation =====

  "analyzer rejects non-bool for condition" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    for i = 0; 1; i++
        |        i
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "analyzer accepts break in for loop" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    for i = 0; i < 10; i++
        |        break
        |    0
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "analyzer accepts continue in for loop" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    for i = 0; i < 10; i++
        |        continue
        |    0
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  // ===== for with if then inline =====

  "for with if then break inline" in {
    eval(
      """main() -> int
        |    sum = 0
        |    for i = 1; true; i++
        |        if i > 5 then break
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 15
  }

  "for with do and if then continue" in {
    output(
      """main() -> int
        |    for i = 0; i < 10; i++ do if i % 3 == 0 then continue
        |    0
        |""".stripMargin) shouldBe ""
  }
}
