package io.github.edadma.trisc

class SyslNestedPatternTests extends SyslTestHelpers {

  // ===== Nested variant patterns =====
  //
  // Patterns like `Outer(Inner(v))` were rejected with "unsupported
  // pattern in destructure" — the analyzer's `analyzeFieldPattern` only
  // handled wildcard / variable / tuple-literal field patterns. Now
  // `DestructurePatternAST` is also accepted as a field pattern: it
  // binds the field to a synthetic outer name, then recursively
  // analyzes the nested pattern. The resulting `TVariantPattern` /
  // `TDestructurePattern` carries an optional `nestedPatterns` list
  // parallel to its `bindings` list. Runtime support is implemented in
  // the interpreter (these tests, via `eval`) and in all three native
  // backends (TRISC, LLVM, SVM) — see SyslNestedPatternBackendTests
  // for codegen-only verification across the native backends. The
  // `--backend trisc` runner exercises end-to-end correctness in std/.
  //
  // Limitation: the analyzer's exhaustiveness check treats any arm
  // with an active nested pattern as NOT fully covering its outer
  // variant — `Wrap(A) -> ...` doesn't guarantee all `Wrap` cases
  // because the inner `A` might not match. Tests therefore include
  // an `else` arm. Full nested-coverage analysis (recognising that
  // `Wrap(A) | Wrap(B)` together cover `Wrap(Inner)` when `Inner`
  // has only `A` and `B`) is a future improvement.

  "match a wrapping variant containing a no-arg variant" in {
    eval(
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
        |""".stripMargin) shouldBe 2
  }

  "match a wrapping variant containing a data variant binds the inner field" in {
    eval(
      """enum Inner
        |    Val(x: int)
        |    None
        |
        |enum Outer
        |    Wrap(i: Inner)
        |    Empty
        |
        |sum_or_zero(o: Outer) -> int
        |    o match
        |        Wrap(Val(v)) -> v
        |        Wrap(None) -> -1
        |        Empty -> 0
        |        else -> -99
        |
        |main() -> int = sum_or_zero(Wrap(Val(7)))
        |""".stripMargin) shouldBe 7
  }

  "nested-pattern arm fails through to the next arm when nested doesn't match" in {
    // The whole point: when the OUTER discriminator matches but the
    // INNER doesn't, the arm fails and the next arm runs. Without
    // proper nested-pattern support this would have been impossible.
    eval(
      """enum Inner
        |    A(x: int)
        |    B
        |
        |enum Outer
        |    Wrap(i: Inner)
        |
        |go(o: Outer) -> int
        |    o match
        |        Wrap(A(v)) -> v * 100
        |        Wrap(B) -> 99
        |        else -> -1
        |
        |main() -> int = go(Wrap(B))   // outer is Wrap, but inner is B not A
        |""".stripMargin) shouldBe 99
  }

  "two-level nesting" in {
    eval(
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
        |main() -> int = peel(X1(Y1(Z1(42))))
        |""".stripMargin) shouldBe 42
  }

  "nested wildcard inside a variant pattern" in {
    eval(
      """enum Inner
        |    A(x: int)
        |    B
        |
        |enum Outer
        |    Wrap(i: Inner)
        |    Empty
        |
        |go(o: Outer) -> int
        |    o match
        |        Wrap(A(_)) -> 100
        |        Wrap(B) -> 200
        |        Empty -> 0
        |        else -> -1
        |
        |main() -> int = go(Wrap(A(99)))
        |""".stripMargin) shouldBe 100
  }

  "nested pattern inside a struct destructure" in {
    eval(
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
        |
        |main() -> int = unwrap(Box(Some(123), 0))
        |""".stripMargin) shouldBe 123
  }

  // ===== Regression: existing flat patterns unchanged =====

  "regression: flat variant pattern still binds field" in {
    eval(
      """enum Result
        |    Ok(value: int)
        |    Err(code: int)
        |
        |main() -> int
        |    val r = Ok(42)
        |    r match
        |        Ok(v) -> v
        |        Err(_) -> -1
        |""".stripMargin) shouldBe 42
  }

  "regression: tuple-pattern field still works" in {
    eval(
      """enum Pair
        |    Pair(p: (int, int))
        |
        |main() -> int
        |    val p = Pair((10, 32))
        |    p match
        |        Pair((a, b)) -> a + b
        |""".stripMargin) shouldBe 42
  }
}
