package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslDoWhileTests extends SyslTestHelpers {

  // ===== Basic do/while =====

  "do/while executes body at least once" in {
    eval(
      """main() -> int
        |    x = 0
        |    do
        |        x = 42
        |    while false
        |    x
        |""".stripMargin) shouldBe 42
  }

  "do/while loops while condition is true" in {
    eval(
      """main() -> int
        |    i = 0
        |    do
        |        i += 1
        |    while i < 5
        |    i
        |""".stripMargin) shouldBe 5
  }

  "do/while with multi-statement body" in {
    output(
      """main() -> int
        |    i = 0
        |    do
        |        print(i)
        |        i += 1
        |    while i < 3
        |    0
        |""".stripMargin) shouldBe "012"
  }

  // ===== Syntax variants =====

  "do/while one-liner: do body while cond" in {
    eval(
      """main() -> int
        |    i = 0
        |    do i += 1 while i < 5
        |    i
        |""".stripMargin) shouldBe 5
  }

  "do/while inline body, while on next line" in {
    eval(
      """main() -> int
        |    i = 0
        |    do i += 1
        |    while i < 5
        |    i
        |""".stripMargin) shouldBe 5
  }

  "do/while block body, while on next line" in {
    eval(
      """main() -> int
        |    sum = 0
        |    i = 1
        |    do
        |        sum = sum + i
        |        i += 1
        |    while i <= 5
        |    sum
        |""".stripMargin) shouldBe 15
  }

  // ===== break in do/while =====

  "break exits do/while" in {
    eval(
      """main() -> int
        |    i = 0
        |    do
        |        i += 1
        |        if i == 3 then break
        |    while true
        |    i
        |""".stripMargin) shouldBe 3
  }

  "break in do/while one-liner" in {
    eval(
      """main() -> int
        |    i = 0
        |    do if i == 0 then break while true
        |    i
        |""".stripMargin) shouldBe 0
  }

  // ===== continue in do/while =====

  "continue skips to condition in do/while" in {
    output(
      """main() -> int
        |    i = 0
        |    do
        |        i += 1
        |        if i == 3 then continue
        |        print(i)
        |    while i < 5
        |    0
        |""".stripMargin) shouldBe "1245"
  }

  "continue re-checks condition" in {
    eval(
      """main() -> int
        |    i = 0
        |    count = 0
        |    do
        |        i += 1
        |        if i % 2 == 0 then continue
        |        count += 1
        |    while i < 10
        |    count
        |""".stripMargin) shouldBe 5
  }

  // ===== Nested =====

  "nested do/while loops" in {
    eval(
      """main() -> int
        |    total = 0
        |    i = 0
        |    do
        |        j = 0
        |        do
        |            total += 1
        |            j += 1
        |        while j < 3
        |        i += 1
        |    while i < 4
        |    total
        |""".stripMargin) shouldBe 12
  }

  "break only exits inner do/while" in {
    eval(
      """main() -> int
        |    total = 0
        |    i = 0
        |    do
        |        j = 0
        |        do
        |            if j == 2 then break
        |            j += 1
        |        while true
        |        total = total + j
        |        i += 1
        |    while i < 3
        |    total
        |""".stripMargin) shouldBe 6
  }

  // ===== Analyzer validation =====

  "analyzer rejects non-bool do/while condition" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    do
        |        x = 1
        |    while 1
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "analyzer accepts break in do/while" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    do
        |        break
        |    while true
        |    0
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "analyzer accepts continue in do/while" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    i = 0
        |    do
        |        i += 1
        |        continue
        |    while i < 5
        |    i
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  // ===== Practical =====

  "do/while digit counter" in {
    eval(
      """main() -> int
        |    n = 12345
        |    digits = 0
        |    do
        |        digits += 1
        |        n = n / 10
        |    while n > 0
        |    digits
        |""".stripMargin) shouldBe 5
  }

  "do/while with zero gives one digit" in {
    eval(
      """main() -> int
        |    n = 0
        |    digits = 0
        |    do
        |        digits += 1
        |        n = n / 10
        |    while n > 0
        |    digits
        |""".stripMargin) shouldBe 1
  }
}
