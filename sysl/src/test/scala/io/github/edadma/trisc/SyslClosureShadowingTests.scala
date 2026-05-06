package io.github.edadma.trisc

/** Regression coverage for two analyzer scope bugs originally reported as
 *  open follow-ups, but already fixed:
 *
 *  - **Inner-local closure shadows global function** — fixed at sysl@964fdd3bc
 *    (CallAST) and sysl@b2ca927a6 (VarRefAST). Both name-resolution paths now
 *    consult the local scope-stack before the flat global-functions namespace.
 *
 *  - **Closure-in-closure fresh local** — fixed at sysl@71fd95193. Bare
 *    `name = expr` (no `var`/`val`) inside a closure body now lowers to
 *    `TVarStmt` for previously-undefined names, not `TAssignStmt`. Without the
 *    fix, the capture-detection pass treats the assignment as a write-to-outer
 *    and registers the fresh local as an outer capture, then crashes at lookup
 *    time with "undefined variable".
 *
 *  These tests pin the fix shape so a future analyzer refactor can't silently
 *  re-introduce either bug. */
class SyslClosureShadowingTests extends SyslTestHelpers {

  // ===== VarRefAST locals shadow globals =====

  "bare ref to local shadowing global function lambda — VarRefAST path" in {
    // The trailing `dispatch` is a VarRefAST. Without the fix it would resolve
    // to the global `dispatch` and produce a value of the wrong function type;
    // with the fix it picks up the local closure.
    eval(
      """dispatch(n: int, action: int) -> int = n + action
        |
        |make_dispatch() -> (int) -> int
        |    val dispatch = (action: int) -> action * 100
        |    dispatch
        |
        |main() -> int
        |    val f = make_dispatch()
        |    f(7)
        |""".stripMargin) shouldBe 700
  }

  "bare-binding local shadowing global — implicit-let path through AssignStmtAST" in {
    // No `val` keyword on the inner binding — the parser emits AssignStmtAST,
    // and after sysl@71fd95193 the analyzer creates a fresh local. Combined
    // with the VarRefAST shadow fix, the trailing `dispatch` reads the local.
    eval(
      """dispatch(n: int, action: int) -> int = n + action
        |
        |make_dispatch() -> (int) -> int
        |    dispatch = (action: int) -> action * 11
        |    dispatch
        |
        |main() -> int
        |    val f = make_dispatch()
        |    f(3)
        |""".stripMargin) shouldBe 33
  }

  // ===== CallAST locals shadow globals =====

  "call to local closure shadowing global function — CallAST path" in {
    // The inner `dispatch(7)` is a CallAST. Without the fix it would resolve
    // to the global; with the fix it routes to the local closure.
    eval(
      """dispatch(action: int) -> int = action * 1000
        |
        |run_local() -> int
        |    val dispatch = (action: int) -> action + 1
        |    dispatch(7)
        |
        |main() -> int = run_local()
        |""".stripMargin) shouldBe 8
  }

  // ===== Closure-in-closure fresh-local creation =====

  "inner closure-typed local inside outer closure body" in {
    // The original bug: `inner = (b) -> a+b` inside the outer closure body
    // was lowered to TAssignStmt, captured as an outer-write, and crashed
    // with "undefined variable: inner" at runtime.
    eval(
      """make_outer() -> (int) -> int
        |    val outer = (a: int) -> a * 10
        |    outer
        |
        |compose() -> (int, int) -> int
        |    val combine = (a: int, b: int) ->
        |        val inner = (x: int) -> a + x
        |        inner(b)
        |    combine
        |
        |main() -> int
        |    val f = compose()
        |    f(3, 4)
        |""".stripMargin) shouldBe 7
  }

  "closure-in-closure with implicit-let bare name" in {
    // The bare-name flavor (`inner = ...` rather than `val inner = ...`).
    // Same shape as the above but exercises the AssignStmtAST → TVarStmt
    // conversion specifically.
    eval(
      """compose_implicit() -> (int, int) -> int
        |    combine = (a: int, b: int) ->
        |        inner = (x: int) -> a + x
        |        inner(b)
        |    combine
        |
        |main() -> int
        |    val f = compose_implicit()
        |    f(5, 6)
        |""".stripMargin) shouldBe 11
  }

  "closure-in-closure shadows outer parameter" in {
    // The inner closure has a parameter named `a` that shadows the outer's
    // `a`. The body should use the inner's binding.
    eval(
      """make_shadowy() -> (int, int) -> int
        |    f = (a: int, b: int) ->
        |        g = (a: int) -> a * 2
        |        g(b)
        |    f
        |
        |main() -> int
        |    val f = make_shadowy()
        |    f(99, 7)
        |""".stripMargin) shouldBe 14
  }
}
