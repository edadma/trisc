package io.github.edadma.trisc

class SyslEffectLatticeOpDispatchTests extends SyslTestHelpers {

  // ===== Concrete operator-trait impls and the effect lattice =====
  //
  // Generic operator-trait impls (`impl[A, B] Map[Parser[A], (A) -> B, ...]`)
  // already pick up effect refinement on closure-literal arguments via
  // `tryUnifyAll`'s `latticeEqual` post-validation: a `(int) -> int #pure`
  // closure literal flows into an `(A) -> B` slot when `A = int`, `B = int`.
  //
  // Concrete impls (`impl Map[int, (int) -> int, int]`) used to compare with
  // strict structural equality (`ps == argTypes`), so the same closure
  // literal — semantically identical to the generic case — was rejected with
  // "no impl of 'Map' for operator '^^' on int, (int) -> int #pure". The fix
  // routes the concrete-impl compare through `latticeEqual` so the FuncType
  // effect-lattice rule fires symmetrically with the generic path.

  "operator-trait concrete impl with `(T) -> U` pattern accepts a `(T) -> U #pure` closure" in {
    eval(
      """trait Map[A, F, R]
        |    #operator("^^")
        |    pmap(a: A, f: F) -> R
        |
        |impl Map[int, (int) -> int, int]
        |    pmap(x: int, f: (int) -> int) -> int = f(x)
        |
        |main() -> int = 7 ^^ ((y: int) -> y * 2)
        |""".stripMargin) shouldBe 14
  }

  "operator-trait concrete impl, named function (no inferred #pure) still dispatches" in {
    // Regression: the lattice change must not break the easy case — a named
    // function with the exact declared type still flows through.
    eval(
      """trait Map[A, F, R]
        |    #operator("^^")
        |    pmap(a: A, f: F) -> R
        |
        |impl Map[int, (int) -> int, int]
        |    pmap(x: int, f: (int) -> int) -> int = f(x)
        |
        |inc(y: int) -> int = y + 1
        |
        |main() -> int = 7 ^^ inc
        |""".stripMargin) shouldBe 8
  }

  "regression: plain function call already accepted #pure closure into unannotated param" in {
    // Mirror of the above for a non-operator path. Already worked
    // pre-fix — pinning here so the bidirectional behavior doesn't drift.
    eval(
      """apply_fn(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply_fn((y: int) -> y * 2, 7)
        |""".stripMargin) shouldBe 14
  }

  "concrete impl on user struct + concrete `(T) -> U` slot accepts #pure closure" in {
    // Same forcing function but with a user struct as the first operand —
    // exercises the lattice rule on a mixed-shape param list.
    eval(
      """struct Box
        |    n: int
        |
        |trait Apply[A, F, R]
        |    #operator("|>")
        |    apply_op(a: A, f: F) -> R
        |
        |impl Apply[Box, (int) -> int, int]
        |    apply_op(b: Box, f: (int) -> int) -> int = f(b.n)
        |
        |main() -> int = Box(20) |> ((x: int) -> x + 22)
        |""".stripMargin) shouldBe 42
  }

  "no-match: operand structure mismatch still produces 'no impl' error" in {
    // The fix relaxes the FuncType *effect* compare. Other structural
    // differences must still reject — pin that the error remains crisp.
    val ex = intercept[Exception] {
      eval(
        """trait Map[A, F, R]
          |    #operator("^^")
          |    pmap(a: A, f: F) -> R
          |
          |impl Map[int, (int) -> int, int]
          |    pmap(x: int, f: (int) -> int) -> int = f(x)
          |
          |main() -> int = 7 ^^ ((s: string) -> 0)
          |""".stripMargin)
    }
    val msg = ex.getMessage.toLowerCase
    assert(
      msg.contains("no impl") || msg.contains("'^^'") || msg.contains("map"),
      s"structural mismatch should still error, got: ${ex.getMessage}",
    )
  }

  "ambiguity check is unchanged after the lattice relaxation" in {
    // Two concrete impls with identical pattern shapes still produce an
    // ambiguity error. The lattice rule loosens single-impl matching but
    // doesn't introduce new dispatch ambiguity by itself.
    val ex = intercept[Exception] {
      eval(
        """trait Map[A, F, R]
          |    #operator("^^")
          |    pmap(a: A, f: F) -> R
          |
          |impl Map[int, (int) -> int, int]
          |    pmap(x: int, f: (int) -> int) -> int = f(x)
          |
          |impl Map[int, (int) -> int, int]
          |    pmap(x: int, f: (int) -> int) -> int = f(x) + 1
          |
          |main() -> int = 7 ^^ ((y: int) -> y)
          |""".stripMargin)
    }
    val msg = ex.getMessage.toLowerCase
    assert(
      msg.contains("ambiguous") || msg.contains("conflict") || msg.contains("overlap") || msg.contains("already") || msg.contains("duplicate"),
      s"two identical impls should be rejected, got: ${ex.getMessage}",
    )
  }
}
