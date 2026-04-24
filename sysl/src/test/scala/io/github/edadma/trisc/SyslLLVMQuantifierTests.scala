package io.github.edadma.trisc

/** End-to-end tests for `for all` / `for some` quantifier expressions through the LLVM
  * backend (compile → opt → clang → run). Mirrors the interpreter coverage in
  * SyslQuantifierTests; here we verify the same semantics survive code generation. */
class SyslLLVMQuantifierTests extends SyslLLVMTestHelpers {

  "for all: true on satisfied range" in {
    val (code, _) = runLLVM(
      """main() -> int
        |    if for all x in 0..10 => x >= 0 then 1 else 0
        |""".stripMargin)
    code shouldBe 1
  }

  "for all: false when violated" in {
    val (code, _) = runLLVM(
      """main() -> int
        |    if for all x in 0..10 => x < 5 then 1 else 0
        |""".stripMargin)
    code shouldBe 0
  }

  "for all: vacuously true on empty range" in {
    val (code, _) = runLLVM(
      """main() -> int
        |    if for all x in 5..<5 => false then 1 else 0
        |""".stripMargin)
    code shouldBe 1
  }

  "for some: true with witness" in {
    val (code, _) = runLLVM(
      """main() -> int
        |    if for some x in 0..10 => x == 7 then 1 else 0
        |""".stripMargin)
    code shouldBe 1
  }

  "for some: false without witness" in {
    val (code, _) = runLLVM(
      """main() -> int
        |    if for some x in 0..10 => x > 100 then 1 else 0
        |""".stripMargin)
    code shouldBe 0
  }

  "for some: false on empty range" in {
    val (code, _) = runLLVM(
      """main() -> int
        |    if for some x in 5..<5 => true then 1 else 0
        |""".stripMargin)
    code shouldBe 0
  }

  "outer-scope capture in predicate" in {
    val (code, _) = runLLVM(
      """main() -> int
        |    var threshold = 7
        |    if for all x in 0..6 => x < threshold then 1 else 0
        |""".stripMargin)
    code shouldBe 1
  }

  "outer-scope array indexed by bound variable" in {
    val (code, _) = runLLVM(
      """main() -> int
        |    var a: [5]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    a[3] = 4
        |    a[4] = 5
        |    if for all i in 0..<5 => a[i] > 0 then 1 else 0
        |""".stripMargin)
    code shouldBe 1
  }

  "for all in require precondition (passes)" in {
    val (code, _) = runLLVM(
      """allPositive(a: *int, n: int) -> int
        |    require for all i in 0..<n => a[i] > 0
        |    var sum = 0
        |    for j = 0; j < n; j++
        |        sum = sum + a[j]
        |    sum
        |
        |main() -> int
        |    var arr: [4]int
        |    arr[0] = 1
        |    arr[1] = 2
        |    arr[2] = 3
        |    arr[3] = 4
        |    allPositive(&arr[0], 4)
        |""".stripMargin)
    code shouldBe 10
  }

  "for all in loop invariant" in {
    val (code, _) = runLLVM(
      """main() -> int
        |    var a: [5]int
        |    var i = 0
        |    while i < 5
        |        invariant for all j in 0..<i => a[j] == 0
        |        a[i] = 0
        |        i = i + 1
        |    a[2]
        |""".stripMargin)
    code shouldBe 0
  }

  "nested: for all of for some" in {
    val (code, _) = runLLVM(
      """main() -> int
        |    if for all i in 0..3 => for some j in 0..3 => i + j == 3 then 1 else 0
        |""".stripMargin)
    code shouldBe 1
  }

  "outer-scope name shadowed by bound variable then restored" in {
    val (code, _) = runLLVM(
      """main() -> int
        |    var x = 99
        |    var ok = if for all x in 0..3 => x >= 0 then 1 else 0
        |    ok + x
        |""".stripMargin)
    code shouldBe 100
  }
}
