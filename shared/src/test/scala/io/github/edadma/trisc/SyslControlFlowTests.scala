package io.github.edadma.trisc

class SyslControlFlowTests extends SyslTestHelpers {

  // ===== If/else =====

  "if true branch" in {
    eval(
      """main() -> int
        |    if 1
        |        return 42
        |    return 0
        |""".stripMargin) shouldBe 42
  }

  "if false branch" in {
    eval(
      """main() -> int
        |    if 0
        |        return 42
        |    return 0
        |""".stripMargin) shouldBe 0
  }

  "if-else" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 3
        |        return 1
        |    else
        |        return 0
        |""".stripMargin) shouldBe 1
  }

  "if-else if-else" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10
        |        return 3
        |    else if x > 3
        |        return 2
        |    else
        |        return 1
        |""".stripMargin) shouldBe 2
  }

  // ===== If with then (inline) =====

  "if then inline expression" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 3 then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "if then else inline expression" in {
    eval("main() -> int = if 1 then 42 else 0\n") shouldBe 42
  }

  "if then else inline false branch" in {
    eval("main() -> int = if 0 then 42 else 99\n") shouldBe 99
  }

  "if as expression in variable" in {
    eval(
      """main() -> int
        |    x = if 1 then 42 else 0
        |    x
        |""".stripMargin) shouldBe 42
  }

  "if as expression in argument" in {
    eval(
      """double(x: int) -> int = x * 2
        |
        |main() -> int = double(if 1 then 21 else 0)
        |""".stripMargin) shouldBe 42
  }

  "if then with block body" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 3 then
        |        return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "if then else with block bodies" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 3 then
        |        42
        |    else
        |        0
        |""".stripMargin) shouldBe 42
  }

  "nested if expressions" in {
    eval("main() -> int = if 1 then if 0 then 1 else 2 else 3\n") shouldBe 2
  }

  // ===== elif =====

  "elif basic" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10
        |        3
        |    elif x > 3
        |        2
        |    else
        |        1
        |""".stripMargin) shouldBe 2
  }

  "elif with then" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10 then 3
        |    elif x > 3 then 2
        |    else 1
        |""".stripMargin) shouldBe 2
  }

  "multiple elifs" in {
    eval(
      """main() -> int
        |    x = 25
        |    if x > 100
        |        5
        |    elif x > 50
        |        4
        |    elif x > 20
        |        3
        |    elif x > 10
        |        2
        |    else
        |        1
        |""".stripMargin) shouldBe 3
  }

  "elif without else" in {
    eval(
      """main() -> int
        |    x = 5
        |    r = 0
        |    if x > 10
        |        r = 3
        |    elif x > 3
        |        r = 2
        |    r
        |""".stripMargin) shouldBe 2
  }

  "elif first branch taken" in {
    eval(
      """main() -> int
        |    x = 50
        |    if x > 10
        |        1
        |    elif x > 5
        |        2
        |    else
        |        3
        |""".stripMargin) shouldBe 1
  }

  "elif last else taken" in {
    eval(
      """main() -> int
        |    x = 1
        |    if x > 10
        |        3
        |    elif x > 5
        |        2
        |    else
        |        1
        |""".stripMargin) shouldBe 1
  }

  "elif with then and blocks" in {
    eval(
      """main() -> int
        |    x = 15
        |    if x > 20 then
        |        3
        |    elif x > 10 then
        |        2
        |    else
        |        1
        |""".stripMargin) shouldBe 2
  }

  "elif mixed with else if" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10
        |        3
        |    elif x > 7
        |        2
        |    else if x > 3
        |        1
        |    else
        |        0
        |""".stripMargin) shouldBe 1
  }

  "else with nested if block" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10
        |        3
        |    else
        |        if x > 3
        |            2
        |        else
        |            1
        |""".stripMargin) shouldBe 2
  }

  "else with deeply nested if blocks" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 100
        |        4
        |    else
        |        if x > 50
        |            3
        |        else
        |            if x > 3
        |                2
        |            else
        |                1
        |""".stripMargin) shouldBe 2
  }

  "else-if (no extra indent) three branches" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 100
        |        4
        |    else if x > 50
        |        3
        |    else if x > 3
        |        2
        |    else
        |        1
        |""".stripMargin) shouldBe 2
  }

  // ===== While =====

  "while loop" in {
    eval(
      """main() -> int
        |    i = 0
        |    sum = 0
        |    while i < 10
        |        sum = sum + i
        |        i = i + 1
        |    return sum
        |""".stripMargin) shouldBe 45
  }

  "while loop never enters" in {
    eval(
      """main() -> int
        |    x = 0
        |    while 0
        |        x = 42
        |    return x
        |""".stripMargin) shouldBe 0
  }

  // ===== While with do =====

  "while do block" in {
    eval(
      """main() -> int
        |    i = 0
        |    sum = 0
        |    while i < 10 do
        |        sum = sum + i
        |        i = i + 1
        |    sum
        |""".stripMargin) shouldBe 45
  }

  "while do inline" in {
    eval(
      """main() -> int
        |    i = 0
        |    while i < 5 do i = i + 1
        |    i
        |""".stripMargin) shouldBe 5
  }

  "while do inline function call" in {
    eval(
      """inc(p: int) -> int
        |    *p = *p + 1
        |    *p
        |
        |main() -> int
        |    i = 0
        |    while i < 5 do inc(&i)
        |    i
        |""".stripMargin) shouldBe 5
  }

  "while do never enters" in {
    eval(
      """main() -> int
        |    x = 0
        |    while 0 do x = 42
        |    x
        |""".stripMargin) shouldBe 0
  }

  // ===== Nested while =====

  "nested while loops" in {
    eval(
      """main() -> int
        |    sum = 0
        |    i = 0
        |    while i < 3
        |        j = 0
        |        while j < 3
        |            sum = sum + 1
        |            j = j + 1
        |        i = i + 1
        |    sum
        |""".stripMargin) shouldBe 9
  }

  // ===== Complex conditions =====

  "complex if condition" in {
    eval(
      """main() -> int
        |    x = 5
        |    y = 3
        |    if x > 3 && y < 10 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== If without else returns 0 =====

  "if without else returns 0" in {
    eval(
      """main() -> int
        |    x = if 0 then 42
        |    x
        |""".stripMargin) shouldBe 0
  }

  // ===== Abs using if expression =====

  "abs function with if expression" in {
    eval(
      """abs(x: int) -> int = if x >= 0 then x else -x
        |
        |main() -> int = abs(-42)
        |""".stripMargin) shouldBe 42
  }

  // ===== Clamp using chained comparison =====

  "clamp with chained comparison" in {
    eval(
      """clamp(x: int, lo: int, hi: int) -> int =
        |    if x < lo then lo
        |    else if x > hi then hi
        |    else x
        |
        |main() -> int
        |    a = clamp(5, 0, 10)
        |    b = clamp(-5, 0, 10)
        |    c = clamp(15, 0, 10)
        |    a + b + c
        |""".stripMargin) shouldBe 15
  }

  // ===== Syntactic edge cases: if/then/else combinations =====

  "if block without then, no else" in {
    eval(
      """main() -> int
        |    x = 0
        |    if 1
        |        x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "if block without then, with else block" in {
    eval(
      """main() -> int
        |    if 0
        |        1
        |    else
        |        2
        |""".stripMargin) shouldBe 2
  }

  "if then inline, no else" in {
    eval(
      """main() -> int
        |    x = 0
        |    if 1 then x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "if then inline, with inline else" in {
    eval("main() -> int = if 1 then 42 else 0\n") shouldBe 42
  }

  "if then inline, with block else" in {
    eval(
      """main() -> int
        |    if 0 then 42
        |    else
        |        99
        |""".stripMargin) shouldBe 99
  }

  "if then block, no else" in {
    eval(
      """main() -> int
        |    x = 0
        |    if 1 then
        |        x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "if then block, with else block" in {
    eval(
      """main() -> int
        |    if 1 then
        |        42
        |    else
        |        99
        |""".stripMargin) shouldBe 42
  }

  "if then block, with else if block" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10 then
        |        3
        |    else if x > 3 then
        |        2
        |    else
        |        1
        |""".stripMargin) shouldBe 2
  }

  "if then inline, else if inline, else inline" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10 then 3
        |    else if x > 3 then 2
        |    else 1
        |""".stripMargin) shouldBe 2
  }

  "if then inline return" in {
    eval(
      """main() -> int
        |    if 1 then return 42
        |    0
        |""".stripMargin) shouldBe 42
  }

  "if block, else if inline" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10
        |        3
        |    else if x > 3 then 2
        |    else 1
        |""".stripMargin) shouldBe 2
  }

  // ===== Syntactic edge cases: return =====

  "explicit return in middle of block" in {
    eval(
      """main() -> int
        |    x = 10
        |    if x > 5
        |        return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "return from nested while" in {
    eval(
      """main() -> int
        |    i = 0
        |    while i < 100
        |        if i == 42
        |            return i
        |        i = i + 1
        |    0
        |""".stripMargin) shouldBe 42
  }

  "return void (no value)" in {
    output(
      """f()
        |    print(1)
        |    return
        |    print(2)
        |
        |main() -> int
        |    f()
        |    0
        |""".stripMargin) shouldBe "1"
  }

  // ===== Syntactic edge cases: expressions as statements =====

  "bare function call as statement" in {
    output(
      """main() -> int
        |    print(42)
        |    0
        |""".stripMargin) shouldBe "42"
  }

  "bare expression as last statement" in {
    eval(
      """main() -> int
        |    x = 40
        |    x + 2
        |""".stripMargin) shouldBe 42
  }

}
