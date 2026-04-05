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

  // ===== Range for loops =====

  "for in inclusive range" in {
    eval(
      """main() -> int
        |    sum = 0
        |    for i in 1..5
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 15
  }

  "for in exclusive range" in {
    eval(
      """main() -> int
        |    sum = 0
        |    for i in 1..<5
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "for in range with do inline" in {
    output(
      """main() -> int
        |    for i in 0..<5 do print(i)
        |    0
        |""".stripMargin) shouldBe "01234"
  }

  "for in range with do block" in {
    output(
      """main() -> int
        |    for i in 1..3 do
        |        print(i)
        |    0
        |""".stripMargin) shouldBe "123"
  }

  "for in range with variable bounds" in {
    eval(
      """main() -> int
        |    lo = 2
        |    hi = 6
        |    sum = 0
        |    for i in lo..<hi
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 14
  }

  "for in range with break" in {
    eval(
      """main() -> int
        |    sum = 0
        |    for i in 0..100
        |        if i > 4 then break
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "for in range with continue" in {
    output(
      """main() -> int
        |    for i in 0..<6
        |        if i % 2 == 0 then continue
        |        print(i)
        |    0
        |""".stripMargin) shouldBe "135"
  }

  "for in range nested" in {
    output(
      """main() -> int
        |    for i in 0..<3
        |        for j in 0..<3
        |            print(i * 3 + j)
        |    0
        |""".stripMargin) shouldBe "012345678"
  }

  "for in range zero iterations (exclusive)" in {
    eval(
      """main() -> int
        |    count = 0
        |    for i in 0..<0
        |        count += 1
        |    count
        |""".stripMargin) shouldBe 0
  }

  "for in range single iteration (inclusive)" in {
    eval(
      """main() -> int
        |    count = 0
        |    for i in 5..5
        |        count += 1
        |    count
        |""".stripMargin) shouldBe 1
  }

  // ===== `in` range membership operator =====

  "in inclusive range — inside" in {
    eval(
      """main() -> int
        |    x = 4
        |    if x in 1..4 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "in inclusive range — at lower bound" in {
    eval(
      """main() -> int
        |    x = 1
        |    if x in 1..4 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "in inclusive range — above" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x in 1..4 then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "in exclusive range — inside" in {
    eval(
      """main() -> int
        |    x = 3
        |    if x in 1..<4 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "in exclusive range — at upper bound (excluded)" in {
    eval(
      """main() -> int
        |    x = 4
        |    if x in 1..<4 then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "in range — below lower" in {
    eval(
      """main() -> int
        |    x = 0
        |    if x in 1..4 then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "in range with variable bounds" in {
    eval(
      """main() -> int
        |    lo = 10
        |    hi = 20
        |    x = 15
        |    if x in lo..hi then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "in range combined with && " in {
    eval(
      """main() -> int
        |    x = 5
        |    y = 7
        |    if x in 1..10 && y in 5..8 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "in range as counting loop" in {
    eval(
      """main() -> int
        |    count = 0
        |    for i in 0..<20
        |        if i in 5..10 then count += 1
        |    count
        |""".stripMargin) shouldBe 6
  }
}
