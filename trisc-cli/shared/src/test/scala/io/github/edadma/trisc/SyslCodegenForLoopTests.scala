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

  // ===== Range for loops =====

  "for in inclusive range" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i in 1..5
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 15
  }

  "for in exclusive range" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i in 1..<5
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "for in range with variable bounds" in {
    compileAndRun(
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
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i in 0..100
        |        if i > 4 then break
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "for in range nested" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i in 0..<3
        |        for j in 0..<3
        |            sum = sum + i * 3 + j
        |    sum
        |""".stripMargin) shouldBe 36
  }

  // ===== `in` range membership operator =====

  "in inclusive range — inside" in {
    compileAndRun(
      """main() -> int
        |    x = 4
        |    if x in 1..4 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "in inclusive range — above" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    if x in 1..4 then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "in exclusive range — upper bound excluded" in {
    compileAndRun(
      """main() -> int
        |    x = 4
        |    if x in 1..<4 then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "in range with variable bounds" in {
    compileAndRun(
      """main() -> int
        |    lo = 10
        |    hi = 20
        |    x = 15
        |    if x in lo..hi then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== !in negated membership =====

  "!in inclusive range — outside" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    if x !in 1..4 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "!in inclusive range — inside" in {
    compileAndRun(
      """main() -> int
        |    x = 3
        |    if x !in 1..4 then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  // ===== downTo =====

  "for in downTo range sum" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i in 10 downTo 1
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 55
  }

  "for in downTo single iteration" in {
    compileAndRun(
      """main() -> int
        |    count = 0
        |    for i in 5 downTo 5
        |        count += 1
        |    count
        |""".stripMargin) shouldBe 1
  }

  "for in downTo zero iterations" in {
    compileAndRun(
      """main() -> int
        |    count = 0
        |    for i in 0 downTo 5
        |        count += 1
        |    count
        |""".stripMargin) shouldBe 0
  }

  // ===== step =====

  "for in inclusive with step" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i in 0..10 step 2
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 30
  }

  "for in exclusive with step" in {
    compileAndRun(
      """main() -> int
        |    count = 0
        |    for i in 0..<10 step 3
        |        count += 1
        |    count
        |""".stripMargin) shouldBe 4
  }

  "for in downTo with step" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i in 10 downTo 0 step 2
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 30
  }

  // ===== Go-style for i, x in arr =====

  "for i, x in array" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    arr[3] = 40
        |    arr[4] = 50
        |    sum = 0
        |    for i, x in arr
        |        sum = sum + x
        |    sum
        |""".stripMargin) shouldBe 150
  }

  "for i, x in array with index" in {
    compileAndRun(
      """main() -> int
        |    arr: [4]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    arr[3] = 40
        |    total = 0
        |    for i, x in arr
        |        total = total + i * x
        |    total
        |""".stripMargin) shouldBe 200
  }
}
