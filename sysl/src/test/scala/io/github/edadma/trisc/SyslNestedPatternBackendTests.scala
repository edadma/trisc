package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Pin for nested variant pattern codegen on all three native backends
  * (TRISC, LLVM, SVM).
  *
  * Before this fix, `Wrap(A(v))` and similar nested patterns threw
  * `sys.error("nested patterns in match arms are not yet supported on
  * the X backend")` at codegen time on TRISC, LLVM, and SVM. The
  * analyzer accepts the syntax and the interpreter executes it — but
  * anywhere user code runs through a native backend (e.g. all of std/
  * via `--backend trisc`), it died.
  *
  * Each backend now emits:
  *   - A discriminator chain: outer tag check, then a recursive
  *     `emitNestedPatternCheck` (per backend) walks the nested
  *     patterns. Each level branches to a per-alternative fail label
  *     (TRISC/SVM) or AND-s into a combined i1 (LLVM) on mismatch;
  *     control falls through to the next pattern alternative.
  *   - A binding chain: outer field bindings copy field values to
  *     locals (existing behaviour); a recursive
  *     `emitNestedPatternBindings` (per backend) walks the nested
  *     patterns and binds names from fields deeper inside the
  *     scrutinee, with each backend's own field-address discipline
  *     (TRISC: refcount-aware copy; LLVM: alias-or-alloca; SVM:
  *     local_set with absolute offset).
  *
  * These tests just confirm codegen produces output without throwing.
  * Runtime correctness is exercised by the interpreter tests in
  * SyslNestedPatternTests; full TRISC end-to-end coverage layers on
  * via the std/ test runner.
  */
class SyslNestedPatternBackendTests extends AnyFreeSpec with Matchers {

  private def asm(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val triscOut = (new SyslTriscCodegen).generate(typed)
    // Cross-check that LLVM and SVM also codegen without throwing.
    val _ = (new SyslLLVMCodegen).generate(typed)
    val _ = (new SyslSVMCodegen).generate(typed)
    triscOut

  "nested variant pattern compiles to TRISC asm without throwing" in {
    val out = asm(
      """enum Inner
        |    Val(x: int)
        |    None
        |
        |enum Outer
        |    Wrap(i: Inner)
        |    Empty
        |
        |unwrap(o: Outer) -> int
        |    o match
        |        Wrap(Val(v)) -> v
        |        Wrap(None) -> -1
        |        Empty -> 0
        |        else -> -99
        |
        |main() -> int = unwrap(Wrap(Val(42)))
        |""".stripMargin)
    out should include("# function: unwrap")
    out should include("# function: main")
  }

  "two-level nested variant compiles to TRISC asm" in {
    val out = asm(
      """enum Z
        |    Z1(v: int)
        |    Z0
        |
        |enum Y
        |    Y1(z: Z)
        |    Y0
        |
        |enum X
        |    X1(y: Y)
        |    X0
        |
        |peel(x: X) -> int
        |    x match
        |        X1(Y1(Z1(v))) -> v
        |        X1(Y1(Z0)) -> -1
        |        X1(Y0) -> -2
        |        X0 -> -3
        |        else -> -99
        |
        |main() -> int = peel(X1(Y1(Z1(7))))
        |""".stripMargin)
    out should include("# function: peel")
    out should include("# function: main")
  }

  "no-arg nested variant in field position compiles" in {
    val out = asm(
      """enum Inner
        |    A
        |    B
        |
        |enum Outer
        |    Wrap(i: Inner)
        |    Empty
        |
        |classify(o: Outer) -> int
        |    o match
        |        Wrap(A) -> 1
        |        Wrap(B) -> 2
        |        Empty -> 0
        |        else -> -99
        |
        |main() -> int = classify(Wrap(B))
        |""".stripMargin)
    out should include("# function: classify")
  }

  "nested pattern inside struct destructure compiles" in {
    val out = asm(
      """enum Inner
        |    Some(x: int)
        |    None
        |
        |struct Box
        |    inner: Inner
        |    other: int
        |
        |unwrap(b: Box) -> int
        |    b match
        |        Box(Some(v), _) -> v
        |        Box(None, k) -> k
        |        else -> -99
        |
        |main() -> int = unwrap(Box(Some(123), 0))
        |""".stripMargin)
    out should include("# function: unwrap")
  }
}
