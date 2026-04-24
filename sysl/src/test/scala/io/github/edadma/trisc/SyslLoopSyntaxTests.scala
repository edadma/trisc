package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslLoopSyntaxTests extends SyslTestHelpers {

  // ===== while syntax variants =====

  "while: indented block body" in {
    output(
      """main() -> int
        |    i = 0
        |    while i < 3
        |        print(i)
        |        i += 1
        |    0
        |""".stripMargin) shouldBe "012"
  }

  "while: do inline body" in {
    eval(
      """main() -> int
        |    i = 0
        |    while i < 10 do i += 1
        |    i
        |""".stripMargin) shouldBe 10
  }

  "while: do block body" in {
    output(
      """main() -> int
        |    i = 0
        |    while i < 3 do
        |        print(i)
        |        i += 1
        |    0
        |""".stripMargin) shouldBe "012"
  }

  "while: do inline with if then break" in {
    eval(
      """main() -> int
        |    i = 0
        |    while true
        |        if i == 5 then break
        |        i += 1
        |    i
        |""".stripMargin) shouldBe 5
  }

  "while: do inline with compound assignment" in {
    eval(
      """main() -> int
        |    i = 0
        |    while i < 10 do i += 3
        |    i
        |""".stripMargin) shouldBe 12
  }

  "while: do inline with assignment" in {
    eval(
      """main() -> int
        |    i = 0
        |    while i < 5 do i = i + 1
        |    i
        |""".stripMargin) shouldBe 5
  }

  // ===== for syntax variants =====

  "for: indented block body" in {
    output(
      """main() -> int
        |    for i = 0; i < 3; i++
        |        print(i)
        |    0
        |""".stripMargin) shouldBe "012"
  }

  "for: multi-statement indented block" in {
    output(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 3; i++
        |        sum = sum + i
        |        print(i)
        |    0
        |""".stripMargin) shouldBe "012"
  }

  "for: do inline body" in {
    output(
      """main() -> int
        |    for i = 0; i < 3; i++ do print(i)
        |    0
        |""".stripMargin) shouldBe "012"
  }

  "for: do block body" in {
    output(
      """main() -> int
        |    for i = 0; i < 3; i++ do
        |        print(i)
        |    0
        |""".stripMargin) shouldBe "012"
  }

  "for: do inline with if then break" in {
    eval(
      """main() -> int
        |    x = 0
        |    for i = 0; true; i++ do if i == 7 then break
        |    0
        |""".stripMargin) shouldBe 0
  }

  "for: do inline with compound assignment" in {
    eval(
      """main() -> int
        |    sum = 0
        |    for i = 1; i <= 5; i++ do sum += i
        |    sum
        |""".stripMargin) shouldBe 15
  }

  // ===== do/while syntax variants =====

  "do/while: indented block body" in {
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

  "do/while: inline body, same line" in {
    eval(
      """main() -> int
        |    i = 0
        |    do i += 1 while i < 5
        |    i
        |""".stripMargin) shouldBe 5
  }

  "do/while: inline body, while on next line" in {
    eval(
      """main() -> int
        |    i = 0
        |    do i += 1
        |    while i < 5
        |    i
        |""".stripMargin) shouldBe 5
  }

  "do/while: inline body with break, same line" in {
    eval(
      """main() -> int
        |    i = 0
        |    do if true then break while true
        |    i
        |""".stripMargin) shouldBe 0
  }

  "do/while: inline body with break, while next line" in {
    eval(
      """main() -> int
        |    x = 0
        |    do x = 99
        |    while false
        |    x
        |""".stripMargin) shouldBe 99
  }

  // ===== Mixed loop types =====

  "while inside for" in {
    eval(
      """main() -> int
        |    total = 0
        |    for i = 0; i < 3; i++
        |        j = 0
        |        while j < i
        |            total += 1
        |            j += 1
        |    total
        |""".stripMargin) shouldBe 3
  }

  "for inside do/while" in {
    eval(
      """main() -> int
        |    total = 0
        |    rounds = 0
        |    do
        |        for i = 0; i < 3; i++
        |            total += 1
        |        rounds += 1
        |    while rounds < 2
        |    total
        |""".stripMargin) shouldBe 6
  }

  "do/while inside while" in {
    eval(
      """main() -> int
        |    total = 0
        |    i = 0
        |    while i < 3
        |        j = 0
        |        do
        |            total += 1
        |            j += 1
        |        while j < 2
        |        i += 1
        |    total
        |""".stripMargin) shouldBe 6
  }

  "break in inner loop does not affect outer" in {
    eval(
      """main() -> int
        |    count = 0
        |    for i = 0; i < 3; i++
        |        j = 0
        |        do
        |            if j == 1 then break
        |            j += 1
        |        while true
        |        count += 1
        |    count
        |""".stripMargin) shouldBe 3
  }

  // ===== loop (Ada-style infinite loop) =====

  "loop: must be exited via break" in {
    eval(
      """main() -> int
        |    i = 0
        |    loop
        |        if i == 5 then break
        |        i += 1
        |    i
        |""".stripMargin) shouldBe 5
  }

  "loop: continue jumps to top" in {
    eval(
      """main() -> int
        |    i = 0
        |    sum = 0
        |    loop
        |        i += 1
        |        if i > 10 then break
        |        if i % 2 == 0 then continue
        |        sum += i
        |    sum
        |""".stripMargin) shouldBe 25  // 1+3+5+7+9
  }

  "loop: nested with labeled break" in {
    eval(
      """main() -> int
        |    found = 0
        |    outer: loop
        |        loop
        |            found = 42
        |            break outer
        |        found = 0
        |    found
        |""".stripMargin) shouldBe 42
  }

  "loop: with optional end loop terminator" in {
    eval(
      """main() -> int
        |    i = 0
        |    loop
        |        if i == 3 then break
        |        i += 1
        |    end loop
        |    i
        |""".stripMargin) shouldBe 3
  }

  "loop: end loop optional, omitted form still works" in {
    eval(
      """main() -> int
        |    i = 0
        |    loop
        |        if i == 3 then break
        |        i += 1
        |    i
        |""".stripMargin) shouldBe 3
  }

  "loop: nested with end loop on inner only" in {
    eval(
      """main() -> int
        |    sum = 0
        |    outer: loop
        |        if sum >= 6 then break
        |        loop
        |            sum += 1
        |            if sum % 3 == 0 then break
        |        end loop
        |    sum
        |""".stripMargin) shouldBe 6
  }
}
