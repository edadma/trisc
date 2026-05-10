package io.github.edadma.trisc

/** LLVM codegen used to have six silent-fallthrough sites that emitted
 *  a `; TODO: …` comment plus an arbitrary placeholder (`"0"` for exprs,
 *  no-op for stmts, the literal string `"???"` for `str()` on an unsupported
 *  type). Audit items #23, #24, #25 from `project_sysl_audit_plan.md` flagged
 *  these as silent miscompilation hazards.
 *
 *  Following the SVM precedent at sysl@3ada8ece (which converted SVM's two
 *  silent-placeholder fallthroughs to `sys.error(…)` for the same reason),
 *  the LLVM equivalents now also throw. The unhandled-shape paths are likely
 *  unreachable in current source — the analyzer pre-checks operand types for
 *  `len()`, `cap()`, `str()`, and direct/indirect-call shape — but a future
 *  analyzer regression that produces an unsupported AST shape now surfaces
 *  loudly at compile time instead of producing wrong runtime output.
 *
 *  Two invariants are pinned by these tests:
 *
 *    1. Every reachable surface that hits one of the six former fallthrough
 *       sites still compiles and produces correct output. Reachable cases of
 *       `len()`, `cap()`, `str()`, indirect-call, and a wide grab-bag of
 *       statement/expression node types are exercised end-to-end.
 *
 *    2. Emitted IR never contains the historical `; TODO:` comment, which
 *       was the signature of a silent fallthrough. If a future codegen change
 *       reintroduces the pattern by accident, this assertion fails.
 *
 *  Pinned at sysl@<TBD>. */
class SyslLLVMSilentFallthroughHardeningTests extends SyslLLVMTestHelpers {

  // ===== Reachable cases for the previously-soft len()/cap() fallthroughs =====

  "len on ArrayType" in {
    llvmExit(
      """main() -> int
        |    var a: [5]int = [1, 2, 3, 4, 5]
        |    len(a)
        |""".stripMargin) shouldBe 5
  }

  "len on SliceType" in {
    llvmExit(
      """main() -> int
        |    var s = [10, 20, 30][:]
        |    len(s)
        |""".stripMargin) shouldBe 3
  }

  "len on StringType" in {
    llvmExit(
      """main() -> int
        |    val s = "hello"
        |    len(s)
        |""".stripMargin) shouldBe 5
  }

  "len on RefType-of-SliceType" in {
    llvmExit(
      """main() -> int
        |    val s = new [4]int
        |    len(s)
        |""".stripMargin) shouldBe 4
  }

  "cap on ArrayType" in {
    llvmExit(
      """main() -> int
        |    var a: [7]int = [1, 2, 3, 4, 5, 6, 7]
        |    cap(a)
        |""".stripMargin) shouldBe 7
  }

  "cap on SliceType" in {
    llvmExit(
      """main() -> int
        |    var s = [10, 20, 30][:]
        |    cap(s)
        |""".stripMargin) shouldBe 3
  }

  "cap on RefType-of-SliceType" in {
    llvmExit(
      """main() -> int
        |    val s = new [9]int
        |    cap(s)
        |""".stripMargin) shouldBe 9
  }

  // ===== Reachable cases for the previously-soft str() fallthrough =====

  "str on int" in {
    val (exit, out) = runLLVM(
      """main() -> int
        |    puts(str(42))
        |    0
        |""".stripMargin)
    out shouldBe "42"
    exit shouldBe 0
  }

  "str on i64" in {
    val (exit, out) = runLLVM(
      """main() -> int
        |    puts(str(i64(9999)))
        |    0
        |""".stripMargin)
    out shouldBe "9999"
    exit shouldBe 0
  }

  "str on bool" in {
    val (exit, out) = runLLVM(
      """main() -> int
        |    puts(str(true))
        |    puts(str(false))
        |    0
        |""".stripMargin)
    out shouldBe "true\nfalse"
    exit shouldBe 0
  }

  "str on f64" in {
    val (exit, out) = runLLVM(
      """main() -> int
        |    puts(str(1.5))
        |    0
        |""".stripMargin)
    // f64 → string via snprintf %g
    out should include("1.5")
    exit shouldBe 0
  }

  "str on string is identity" in {
    val (exit, out) = runLLVM(
      """main() -> int
        |    val s = "hi"
        |    puts(str(s))
        |    0
        |""".stripMargin)
    out shouldBe "hi"
    exit shouldBe 0
  }

  // ===== Reachable case for the previously-soft indirect-call fallthrough =====

  "indirect call through a closure-typed local" in {
    llvmExit(
      """main() -> int
        |    val f = (x: int) -> x + 1
        |    f(41)
        |""".stripMargin) shouldBe 42
  }

  "indirect call through a closure-typed parameter" in {
    llvmExit(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply((n: int) -> n * 2, 21)
        |""".stripMargin) shouldBe 42
  }

  // ===== IR-shape invariant: no silent-fallthrough comment ever escapes =====

  "compiled IR for a kitchen-sink program contains no '; TODO' comments" in {
    // Exercise a broad mix of expressions and statements through codegen.
    // If any of the historical fallthrough sites reappears, the emitted IR
    // would contain a `; TODO:` comment.
    val ir = compileLLVM(
      """struct Pair
        |    a: int
        |    b: int
        |
        |enum Result
        |    Ok(value: int)
        |    Err(msg: string)
        |
        |use_pair(p: Pair) -> int = p.a + p.b
        |
        |run(r: Result) -> int
        |    r match
        |        Ok(v) -> v
        |        Err(_) -> -1
        |
        |main() -> int
        |    var x = 1
        |    x += 2
        |    x -= 1
        |    var arr: [3]int = [10, 20, 30]
        |    val s = arr[:]
        |    val n = len(s) + cap(s)
        |    val pair = Pair(7, 35)
        |    val r1 = Ok(100)
        |    val r2: Result = Err("boom")
        |    val ok = run(r1)
        |    val err = run(r2)
        |    val f = (k: int) -> k * 3
        |    val fk = f(2)
        |    puts(str(n))
        |    puts(str(true))
        |    if x > 0 then ok + err + fk + use_pair(pair) else 0
        |""".stripMargin)
    ir should not include ("; TODO:")
  }
}
