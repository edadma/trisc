package io.github.edadma.trisc

/** Audit item #30 (Tier 4): the `volatile` keyword is a codegen instruction
 *  to LLVM/TRISC ("emit volatile load/store, do not coalesce or reorder")
 *  and has no semantic effect on the interpreter. The interpreter should
 *  parse, analyze, and evaluate volatile decls identically to plain ones —
 *  no crash, no divergent answer.
 *
 *  These tests pin that contract: each program is run through the
 *  interpreter and produces the answer it would have produced without
 *  `volatile`. If we ever add a side-effect-tracking interpreter (for the
 *  `#pure` annotation, say) and forget to neutralize `volatile` there,
 *  the answers will change and these regressions catch it.
 */
class SyslInterpreterVolatileTests extends SyslTestHelpers {

  // ===== Volatile local var =====

  "volatile var local: read-back" in {
    eval(
      """main() -> int
        |    volatile var x: int = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "volatile var local: assign and read" in {
    eval(
      """main() -> int
        |    volatile var x: int = 0
        |    x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "volatile var local: arithmetic preserved" in {
    eval(
      """main() -> int
        |    volatile var x: int = 10
        |    x = x + 32
        |    x
        |""".stripMargin) shouldBe 42
  }

  "volatile var local in loop: increment runs full count" in {
    // If the interpreter accidentally elided volatile reads or writes,
    // a loop counter would diverge — but this is value-semantics, so
    // it should match a plain `var`.
    eval(
      """main() -> int
        |    volatile var i: int = 0
        |    var sum: int = 0
        |    while i < 5
        |        sum = sum + i
        |        i = i + 1
        |    sum
        |""".stripMargin) shouldBe 10  // 0+1+2+3+4
  }

  // ===== Volatile struct field =====

  "volatile struct field: read-back" in {
    eval(
      """struct Regs
        |    volatile status: int
        |    data: int
        |
        |main() -> int
        |    var r = Regs(7, 0)
        |    r.status
        |""".stripMargin) shouldBe 7
  }

  "volatile struct field: assign and read" in {
    eval(
      """struct Regs
        |    volatile status: int
        |    data: int
        |
        |main() -> int
        |    var r = Regs(0, 0)
        |    r.status = 99
        |    r.status
        |""".stripMargin) shouldBe 99
  }

  "volatile and non-volatile fields independent" in {
    eval(
      """struct Regs
        |    volatile status: int
        |    data: int
        |
        |main() -> int
        |    var r = Regs(1, 2)
        |    r.status = 10
        |    r.data = 32
        |    r.status + r.data
        |""".stripMargin) shouldBe 42
  }

  // ===== Volatile module-level var =====

  "volatile module-level var: round-trip" in {
    eval(
      """volatile var counter: int = 0
        |
        |bump() -> unit
        |    counter = counter + 1
        |
        |main() -> int
        |    bump()
        |    bump()
        |    bump()
        |    counter
        |""".stripMargin) shouldBe 3
  }

  // ===== Volatile + value semantics combine cleanly =====

  "volatile decl does not interfere with conditionals" in {
    eval(
      """main() -> int
        |    volatile var flag: int = 1
        |    if flag == 1 then 42 else 0
        |""".stripMargin) shouldBe 42
  }
}
