package io.github.edadma.trisc

/** Negative tests — programs that MUST be rejected by the analyzer.
 *
 *  These pin the analyzer's compile-time diagnostics: each test asserts
 *  the rejection fires AND that the diagnostic message names the right
 *  surface, so a future refactor that silently allows one of these
 *  through (or surfaces a misleading error) fails loudly.
 *
 *  The `rejects(msgFragment)(source)` helper compiles `source`, expects
 *  an `AnalysisError`, and asserts its message contains `msgFragment`.
 *  Substring match deliberately — exact-string matching would fail on
 *  every cosmetic message tweak.
 */
class SyslCompileErrorTests extends SyslTestHelpers {

  /** Parse + analyze `source`; assert that compilation throws an
   *  analyzer-side `RuntimeException` (the actual type is the
   *  path-dependent `SyslAnalyzer#AnalysisError`, a `RuntimeException`
   *  subclass) whose message contains `msgFragment`. */
  def rejects(msgFragment: String)(source: String): Unit =
    val ast = (new SyslParser).parseProgram(source) match
      case Right(p) => p
      case Left(err) => fail(s"parse error (expected analysis-time rejection, not parse failure): $err")
    val ex = intercept[RuntimeException] {
      (new SyslAnalyzer).analyze(ast)
    }
    withClue(s"actual error: '${ex.getMessage}'\n") {
      ex.getMessage.should(include(msgFragment))
    }

  "duplicate function declaration rejected" in rejects("duplicate function") {
    """foo() -> int = 0
      |foo() -> int = 1
      |
      |main() -> int = foo()
      |""".stripMargin
  }

  "duplicate struct declaration rejected" in rejects("duplicate struct") {
    """struct Point
      |    x: int
      |    y: int
      |
      |struct Point
      |    a: int
      |
      |main() -> int = 0
      |""".stripMargin
  }

  "accessing a field that doesn't exist rejected" in rejects("has no field") {
    """struct Box
      |    v: int
      |
      |main() -> int
      |    var b = Box(42)
      |    b.nonexistent
      |""".stripMargin
  }

  "calling a function with wrong arg count rejected" in rejects("argument(s), got") {
    """add(a: int, b: int) -> int = a + b
      |
      |main() -> int = add(1)
      |""".stripMargin
  }

  "assigning to a `val` rejected" in rejects("immutable variable") {
    """main() -> int
      |    val x = 1
      |    x = 2
      |    x
      |""".stripMargin
  }

  // The ptr→ref direction is the dual of `ref → ptr` (which IS allowed,
  // see SyslRefTests "&r yields *T pointing at the heap data ..."). We
  // can't synthesize a refcount from a raw pointer; the analyzer must
  // reject the assignment at the val-decl. Message comes from the
  // val-decl compatibility check in SyslAnalyzerStatements.
  "ptr to ref conversion rejected (can't manufacture a refcount)" in
    rejects("cannot assign") {
      """struct Box
        |    v: int
        |
        |main() -> int
        |    var b = Box(0)
        |    val p: *Box = &b
        |    val r: &Box = p
        |    0
        |""".stripMargin
    }
}
