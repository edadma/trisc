package io.github.edadma.trisc

class SyslEffectLatticeInferTests extends SyslTestHelpers {

  // ===== Effect lattice in generic-type-parameter inference =====
  //
  // c1afefc0e made the function-type *unifier* lattice-aware: when checking
  // whether an `actual` flows into a `slot`, it compared FuncType effects via
  // `effectsSatisfy(actual, slot)` rather than `==`. But the inference path
  // that *collects* concrete observations of the same type variable still used
  // `==` to merge two bindings. Result: any combinator-library call where the
  // user supplies a literal closure (auto-`#pure`) and the same type variable
  // is also constrained by an unannotated context failed to infer with
  // "cannot infer type parameter 'A': seen both X and X #pure".
  //
  // Fix: when a second observation arrives for an already-bound tvar, take the
  // **lattice LUB** on `FuncType` effects (Pure ≤ RW(R, W) ≤ Unknown, with
  // `RW` ordered by subset). The merged binding is wide enough that *every*
  // observation satisfies it as a slot via the same lattice rule the unifier
  // uses — picking the GLB instead would let the merge succeed but make the
  // "larger" observation fail `checkArgs` immediately afterwards. Recursion
  // walks slice / array / ptr / ref / struct / named so an embedded FuncType
  // anywhere in the shape uses the lattice. Incomparable cases (different
  // param/result shape, RW with unrelated read/write sets) still error out as
  // today.

  // ===== Lattice — accept cases =====

  "explicit-tparam unannotated meets pure-closure: parsyl repro" in {
    eval(
      """type Parser[A] = new (int) -> int
        |
        |success[A](v: A) -> Parser[A] = Parser[A]((x: int) -> x)
        |
        |main() -> int
        |    val sub: Parser[(int, int) -> int] =
        |        success[(int, int) -> int]((a: int, b: int) -> a - b)
        |    sub(7)
        |""".stripMargin) shouldBe 7
  }

  "two pure-closure args + same fn-type tvar: no merge needed (regression)" in {
    eval(
      """choose[A](a: A, b: A, pick_first: bool) -> A = if pick_first then a else b
        |
        |main() -> int
        |    val f = choose((x: int) -> x + 1, (x: int) -> x * 2, true)
        |    f(20)
        |""".stripMargin) shouldBe 21
  }

  "explicit unannotated tparam + pure-closure arg: merge to unannotated" in {
    // Without the merge this used to fail at the second observation: explicit
    // type arg pre-seeds env(A) = unannotated, then the closure arg produces
    // #pure, and the equality check rejected. Under the lattice LUB the
    // binding stays at the unannotated form (the wider one); the closure flows
    // in via `effectsSatisfy(#pure, unannotated)`.
    eval(
      """apply[A](v: A) -> A = v
        |
        |main() -> int
        |    val f: (int) -> int = apply[(int) -> int]((x: int) -> x + 5)
        |    f(37)
        |""".stripMargin) shouldBe 42
  }

  // ===== Lattice on the read/write axis =====

  "RW callee + unannotated explicit tparam: merge to unannotated" in {
    // env(A) pre-seeded as unannotated `() -> int`, then `&read_x` arrives
    // with `() -> int #reads(x)`. LUB picks Unknown (the wider one); the RW
    // callee flows into the unannotated slot via lattice subtyping.
    eval(
      """var x = 5
        |
        |#reads(x)
        |read_x() -> int = x
        |
        |fst[A](a: A, b: A) -> A = a
        |
        |main() -> int
        |    val v: () -> int = fst[() -> int](&read_x, &read_x)
        |    v()
        |""".stripMargin) shouldBe 5
  }

  "RW subset merge: #reads(x) and #reads(x, y) bind to wider read set" in {
    // {x} ⊆ {x, y}, so the LUB on effects is `#reads(x, y)` — the larger set.
    // Both callees are then valid actuals at A's slot under the lattice rule.
    eval(
      """var x = 5
        |var y = 10
        |
        |#reads(x)
        |read_x() -> int = x
        |
        |#reads(x, y)
        |read_xy() -> int = x + y
        |
        |fst[A](a: A, b: A) -> A = a
        |
        |main() -> int
        |    val v = fst(&read_x, &read_xy)
        |    v()
        |""".stripMargin) shouldBe 5
  }

  "incomparable RW (reads-only vs writes-only) rejected" in {
    val ex = intercept[Exception] {
      eval(
        """var x = 5
          |var y = 0
          |
          |#reads(x)
          |read_x() -> int = x
          |
          |#writes(y)
          |write_y() -> int
          |    y = 99
          |    return 0
          |
          |fst[A](a: A, b: A) -> A = a
          |
          |main() -> int
          |    val v = fst(&read_x, &write_y)
          |    v()
          |""".stripMargin)
    }
    val msg = ex.getMessage.toLowerCase
    assert(
      msg.contains("seen both") || msg.contains("cannot infer"),
      s"reads/writes that aren't subset-related should reject, got: $msg",
    )
  }

  // ===== Lattice — reject cases =====

  "incompatible param types still rejected" in {
    val ex = intercept[Exception] {
      eval(
        """fst[A](a: A, b: A) -> A = a
          |
          |main() -> int = fst((x: int) -> x, (a: int, b: int) -> a + b)
          |""".stripMargin)
    }
    val msg = ex.getMessage.toLowerCase
    assert(
      msg.contains("seen both") || msg.contains("cannot infer") || msg.contains("expects") || msg.contains("mismatch"),
      s"different param arity should be rejected, got: $msg",
    )
  }

  "incompatible result types still rejected" in {
    val ex = intercept[Exception] {
      eval(
        """fst[A](a: A, b: A) -> A = a
          |
          |main() -> int
          |    val v = fst(7, "hello")
          |    0
          |""".stripMargin)
    }
    val msg = ex.getMessage.toLowerCase
    assert(
      msg.contains("seen both") || msg.contains("cannot infer") || msg.contains("mismatch"),
      s"different element types should be rejected, got: $msg",
    )
  }

  // ===== Regressions — equality path unchanged =====

  "regression: same-effect merge (both unannotated fn types)" in {
    eval(
      """type FnRef = (int) -> int
        |
        |fst[A](a: A, b: A) -> A = a
        |
        |add1(x: int) -> int = x + 1
        |add2(x: int) -> int = x + 2
        |
        |main() -> int
        |    val a: FnRef = add1
        |    val b: FnRef = add2
        |    val pick = fst[FnRef](a, b)
        |    pick(40)
        |""".stripMargin) shouldBe 41
  }

  "regression: same-effect merge (both #pure closures)" in {
    eval(
      """fst[A](a: A, b: A) -> A = a
        |
        |main() -> int
        |    val pick = fst((x: int) -> x + 10, (x: int) -> x + 20)
        |    pick(30)
        |""".stripMargin) shouldBe 40
  }

  "regression: trivial generic on int still works" in {
    eval(
      """fst[A](a: A, b: A) -> A = a
        |
        |main() -> int = fst(40, 2) + 2
        |""".stripMargin) shouldBe 42
  }

  // ===== Lattice — nested fn-type inside a generic shape =====

  "fn type inside Parser[T] shape: tparam appears in both A and Parser[A] positions" in {
    // success[A](v: A) -> Parser[A] — A appears in the parameter position
    // (constrained by the closure type as `(int) -> int #pure`) AND in the
    // return position (constrained by the val ascription via Parser[A] as
    // `(int) -> int` unannotated). Inference must merge these two A
    // observations under the lattice.
    eval(
      """type Parser[A] = new (int) -> int
        |
        |success[A](v: A) -> Parser[A] = Parser[A]((x: int) -> x)
        |
        |main() -> int
        |    val p: Parser[(int) -> int] =
        |        success[(int) -> int]((x: int) -> x * 3)
        |    p(11)
        |""".stripMargin) shouldBe 11
  }
}
