package io.github.edadma.trisc

/** Asserts the LLVM codegen emits a null-pointer guard after every
 *  `@malloc(...)` call. Without the guard, an out-of-memory failure
 *  silently writes the refcount header to address 0 — segfault on
 *  hosted backends, silent corruption on bare metal. The guard
 *  branches to `@__malloc_fail` which writes "panic: out of memory"
 *  to stderr and aborts. Per the project CLAUDE.md "Critical TODO"
 *  on runtime error handling.
 *
 *  We assert at the IR-pattern level rather than via a runtime OOM
 *  test because triggering a real malloc failure is fragile (huge
 *  literal allocations get caught by parser/typecheck, and overcommit
 *  on macOS lets the malloc itself succeed). The IR pattern check
 *  catches every codegen path that emits malloc — if a new alloc
 *  site is added without the guard, this test fails.
 */
class SyslLLVMMallocGuardTests extends SyslLLVMTestHelpers {

  /** Codegen must route every allocation through `@__checked_malloc`
    * (the wrapper-function form) — direct `@malloc(...)` call sites
    * outside the wrapper definition itself are a regression and would
    * silently write the next refcount-header store to address 0 on
    * OOM. We assert: (a) the wrapper is defined, (b) the failure
    * helper is defined, (c) no code-emit site uses bare `@malloc(`
    * (the only legal direct call is inside the wrapper body). */
  private def assertEveryMallocGuarded(ir: String): Unit =
    ir should include("define i8* @__checked_malloc")
    ir should include("define void @__malloc_fail")
    // Count `call i8* @malloc(` sites — should be exactly one (the
    // wrapper's body) and never more. Codegen sites all go through
    // `call i8* @__checked_malloc(`.
    val directMalloc = "call i8\\* @malloc\\(".r.findAllMatchIn(ir).length
    val checkedMalloc = "call i8\\* @__checked_malloc\\(".r.findAllMatchIn(ir).length
    withClue(s"direct @malloc calls = $directMalloc, @__checked_malloc calls = $checkedMalloc:\n") {
      directMalloc shouldBe 1  // only the wrapper body's call
      checkedMalloc should be > 0
    }

  "TNew (heap-allocated struct) emits malloc guard" in {
    val ir = compileLLVM(
      """struct Box
        |    v: int
        |
        |main() -> int
        |    val r = new Box(42)
        |    r.v
        |""".stripMargin)
    assertEveryMallocGuarded(ir)
  }

  "TNewArray (dynamic array) emits malloc guard" in {
    val ir = compileLLVM(
      """main() -> int
        |    val a = new [10]int
        |    a[0]
        |""".stripMargin)
    assertEveryMallocGuarded(ir)
  }

  "TNewEnum (heap-allocated data enum) emits malloc guard" in {
    val ir = compileLLVM(
      """enum E
        |    A(v: int)
        |    B
        |
        |main() -> int
        |    val e = new A(7)
        |    0
        |""".stripMargin)
    assertEveryMallocGuarded(ir)
  }

  "string buffer alloc emits malloc guard" in {
    // s"$n" lowers to a snprintf path that allocates a string buffer.
    val ir = compileLLVM(
      """main() -> int
        |    var n = 42
        |    val s = s"$n"
        |    0
        |""".stripMargin)
    assertEveryMallocGuarded(ir)
  }

  "closure with heap env emits malloc guard" in {
    val ir = compileLLVM(
      """make_adder(k: int) -> (int) -> int = (x: int) -> x + k
        |
        |main() -> int
        |    val f = make_adder(5)
        |    f(10)
        |""".stripMargin)
    assertEveryMallocGuarded(ir)
  }

  "owns=true iface box emits malloc guard" in {
    val ir = compileLLVM(
      """interface Holder
        |    value() -> int
        |
        |struct Tally
        |    n: int
        |
        |Tally.value() -> int = self.n
        |
        |make() -> Holder
        |    val t = Tally(99)
        |    return t
        |
        |main() -> int
        |    val h = make()
        |    h.value()
        |""".stripMargin)
    assertEveryMallocGuarded(ir)
  }

  "malloc_fail helper has the expected message" in {
    val ir = compileLLVM(
      """struct Box
        |    v: int
        |
        |main() -> int
        |    val r = new Box(1)
        |    0
        |""".stripMargin)
    ir should include("panic: out of memory")
    ir should include("define void @__malloc_fail")
    ir should include("call void @abort()")
  }
}
