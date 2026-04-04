package io.github.edadma.trisc

class SyslCodegenDoWhileTests extends SyslCodegenHelpers {

  "do/while executes body at least once" in {
    compileAndRun(
      """main() -> int
        |    x = 0
        |    do
        |        x = 42
        |    while false
        |    x
        |""".stripMargin) shouldBe 42
  }

  "do/while loops while condition is true" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    do
        |        i += 1
        |    while i < 5
        |    i
        |""".stripMargin) shouldBe 5
  }

  "do/while one-liner" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    do i += 1 while i < 5
        |    i
        |""".stripMargin) shouldBe 5
  }

  "do/while block body" in {
    compileAndRun(
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

  "break exits do/while" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    do
        |        i += 1
        |        if i == 3 then break
        |    while true
        |    i
        |""".stripMargin) shouldBe 3
  }

  "continue re-checks condition" in {
    compileAndRun(
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

  "nested do/while loops" in {
    compileAndRun(
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
    compileAndRun(
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

  "do/while digit counter" in {
    compileAndRun(
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
    compileAndRun(
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
