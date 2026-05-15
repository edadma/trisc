package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

/** End-to-end tests for the wasm32 backend test runner.
 *
 *  These tests require:
 *    - Homebrew LLVM 22 at `/opt/homebrew/opt/llvm/bin/clang` (overridable
 *      via `SYSL_WASM_CLANG`)
 *    - `wasmtime` at `/opt/homebrew/bin/wasmtime` (overridable via
 *      `SYSL_WASMTIME`)
 *    - The freestanding runtime under `sysl/runtime/wasm/` (cwd-relative;
 *      overridable via `SYSL_WASM_RUNTIME`)
 *
 *  When any of those are missing — typical on CI without wasmtime
 *  installed — the whole suite skips. We test the *runner* (wasmtime
 *  launch, output capture, panic-marker parsing) and the chunk-2
 *  runtime, not the codegen itself. Chunk-1's
 *  `SyslLLVMWasmTargetTests` covers IR shape; chunk-2's
 *  `SyslCliWasmRuntimeTests` covers the freestanding runtime in
 *  isolation; this suite covers the CLI dispatcher + runner.
 *
 *  Tagged `Slow` because each wasmtime launch is ~30ms wall on top of a
 *  one-time ~450ms runtime build. The whole suite runs in ~2s.
 */
class SyslCliWasmRunnerTests extends AnyFreeSpec with Matchers {

  FileOps.instance = JvmFileOps

  private val clang: String = sys.env.getOrElse("SYSL_WASM_CLANG", "/opt/homebrew/opt/llvm/bin/clang")
  private val wasmtime: String = sys.env.getOrElse("SYSL_WASMTIME", "/opt/homebrew/bin/wasmtime")
  private val runtimeDir: String = sys.env.getOrElse("SYSL_WASM_RUNTIME", "sysl/runtime/wasm")
  private val corpus: String = "sysl/tests/riscv_smoke/"

  /** True iff every external tool the runner needs is present. We check
    * at startup time and short-circuit each test with `cancel` if
    * anything's missing — ScalaTest reports cancelled tests separately
    * so they don't pollute the failure column. */
  private val toolchainReady: Boolean =
    java.nio.file.Files.exists(java.nio.file.Paths.get(clang)) &&
    java.nio.file.Files.exists(java.nio.file.Paths.get(wasmtime)) &&
    java.nio.file.Files.exists(java.nio.file.Paths.get(runtimeDir)) &&
    java.nio.file.Files.exists(java.nio.file.Paths.get(corpus))

  private def requireToolchain(): Unit =
    if !toolchainReady then
      cancel(s"wasm toolchain or corpus not present (clang=$clang wasmtime=$wasmtime runtimeDir=$runtimeDir corpus=$corpus)")

  private def runCli(args: String*): (Int, String) = CliCapture.runCli(args)

  /** Stage a single .lsysl under `target/sysl_wasm_runner/<label>_<n>/`
    * so the module declaration we embed inside matches the directory
    * path the driver derives from it. (The driver requires the
    * declared `module a.b.c` to be a prefix of the cwd-relative path
    * with `/` -> `.`.)
    *
    * Returns (absoluteDir, cwdRelativeDir, modulePathPrefix). */
  private def stage(label: String): (Path, String, String) =
    val baseRel = "target/sysl_wasm_runner"
    val baseAbs = java.nio.file.Paths.get(baseRel).toAbsolutePath
    Files.createDirectories(baseAbs)
    val unique = SyslCliWasmRunnerTests.nextStagingId()
    val abs = baseAbs.resolve(s"${label}_$unique")
    Files.createDirectories(abs)
    val rel = s"$baseRel/${label}_$unique"
    val modBase = rel.replace("/", ".")
    (abs, rel, modBase)

  private def writeFile(dir: Path, rel: String, contents: String): Path =
    val p = dir.resolve(rel)
    Files.createDirectories(p.getParent)
    Files.writeString(p, contents)
    p

  "wasm32 backend runs the smoke corpus end-to-end" taggedAs Slow in {
    requireToolchain()
    val (code, out) = runCli("test", "--backend", "wasm32", corpus)
    withClue(out) {
      code shouldBe 0
      out should include("running 7 tests (backend: wasm32)")
      out should include("7 passed, 0 failed")
    }
  }

  "unexpected panic on wasm32 reports the panic message from captured output" taggedAs Slow in {
    requireToolchain()
    val (abs, rel, modBase) = stage("neg_panic")
    writeFile(abs, "neg.lsysl",
      s"""    module $modBase
         |
         |    #test
         |    test_unexpected_panic() -> unit
         |        panic("kaboom-marker")
         |""".stripMargin)
    val (code, out) = runCli("test", "--backend", "wasm32", rel)
    withClue(out) {
      out should include("0 passed, 1 failed")
      out should include("panic: kaboom-marker")
      out should include("test_unexpected_panic")
    }
  }

  "mismatched should_panic expected-message is flagged on wasm32" taggedAs Slow in {
    requireToolchain()
    val (abs, rel, modBase) = stage("neg_msg")
    writeFile(abs, "msg.lsysl",
      s"""    module $modBase
         |
         |    #test(should_panic: "expected-msg")
         |    test_wrong_msg() -> unit
         |        panic("actual-msg")
         |""".stripMargin)
    val (code, out) = runCli("test", "--backend", "wasm32", rel)
    withClue(out) {
      out should include("0 passed, 1 failed")
      out should include("panic message did not contain 'expected-msg'")
      out should include("actual-msg")
    }
  }

  /** Regression for the print-then-panic case originally surfaced on
    *  rv64. On wasm the same risk applies: `print` without a newline
    *  followed immediately by `panic` produces a single line like
    *  `12panic: mismatch: got 1, want 2`. The `panicMarker` substring
    *  search (shared with the rv runner) must catch it. */
  "should_panic test that prints values without newline before panicking is recognized as a panic on wasm32" taggedAs Slow in {
    requireToolchain()
    val (abs, rel, modBase) = stage("neg_no_nl")
    writeFile(abs, "neg_no_nl.lsysl",
      s"""    module $modBase
         |
         |    #test(should_panic: "mismatch")
         |    test_print_then_panic() -> unit
         |        print(1)
         |        print(2)
         |        panic("mismatch: got 1, want 2")
         |""".stripMargin)
    val (code, out) = runCli("test", "--backend", "wasm32", rel)
    withClue(out) {
      code shouldBe 0
      out should include("1 passed, 0 failed")
    }
  }

  "assertion failure on wasm32 is detected via its own marker" taggedAs Slow in {
    requireToolchain()
    val (abs, rel, modBase) = stage("neg_assert")
    writeFile(abs, "neg_assert.lsysl",
      s"""    module $modBase
         |
         |    #test(should_panic: "two-plus-two")
         |    test_assert_marker() -> unit
         |        assert(2 + 2 == 5, "two-plus-two")
         |""".stripMargin)
    val (code, out) = runCli("test", "--backend", "wasm32", rel)
    withClue(out) {
      code shouldBe 0
      out should include("1 passed, 0 failed")
    }
  }

  "unknown --backend is rejected before any wasmtime/clang invocation" taggedAs Slow in {
    val (code, out) = runCli("test", "--backend", "bogus", corpus)
    withClue(out) {
      // scopt prints to stderr and returns None, which CliCapture maps to 2.
      code shouldBe 2
      out should include("Unknown backend")
    }
  }

  // === SYSL_WASM_HOST fork (chunk-5 plumbing) ===========================
  //
  // The dispatcher in `runOneWasm` reads `SYSL_WASM_HOST` /
  // `SYSL_WASM_SCALA_INTERP` via the shared `wasmConfig` helper, which
  // prefers a Java system property over an env var. That gives these
  // in-process tests a way to flip host configuration per-test without
  // mutating the JVM env map (which is unmodifiable). Production users
  // set env vars; tests set sys-props.

  /** Wrap a body in a temporary system-property override. Restores the
    * prior state (set / unset) on exit so a failing test can't leak
    * state into the next case. */
  private def withSysProp[A](name: String, value: String)(body: => A): A =
    val prior = Option(System.getProperty(name))
    System.setProperty(name, value)
    try body
    finally prior match
      case Some(v) => System.setProperty(name, v)
      case None    => System.clearProperty(name)

  /** Resolve the scala-interp launcher once. Cancels the test when the
    * env var isn't set or points at a missing file — same shape as
    * `requireToolchain` so CI without a built launcher reports
    * "cancelled" rather than "failed". Set `SYSL_WASM_SCALA_INTERP` to
    * either the wasm-stable scala-native binary
    * (`/Users/ed/dev/wasm-stable/cli/native/target/scala-3.8.3/cli-out`
    * after `sbt cliNative/nativeLink`) or a `java -jar wasm-cli.jar`
    * wrapper script after sbt-assembly. */
  private def requireScalaInterpLauncher(): String =
    sys.env.get("SYSL_WASM_SCALA_INTERP")
      .filter(p => java.nio.file.Files.exists(java.nio.file.Paths.get(p)))
      .getOrElse(cancel("SYSL_WASM_SCALA_INTERP not set or launcher file missing — point it at the wasm-stable CLI binary or fat-jar wrapper"))

  "SYSL_WASM_HOST=scala-interp routes the smoke corpus through the configured launcher" taggedAs Slow in {
    requireToolchain()
    val launcher = requireScalaInterpLauncher()
    val (code, out) =
      withSysProp("SYSL_WASM_HOST", "scala-interp") {
        withSysProp("SYSL_WASM_SCALA_INTERP", launcher) {
          runCli("test", "--backend", "wasm32", corpus)
        }
      }
    withClue(out) {
      code shouldBe 0
      out should include("running 7 tests (backend: wasm32)")
      out should include("7 passed, 0 failed")
    }
  }

  "SYSL_WASM_HOST=scala-interp without SYSL_WASM_SCALA_INTERP fails clearly" taggedAs Slow in {
    requireToolchain()
    val (_, out) =
      withSysProp("SYSL_WASM_HOST", "scala-interp") {
        // Deliberately do NOT set SYSL_WASM_SCALA_INTERP — and clear it
        // if it leaked in from the env (avoid resolving the real
        // launcher and accidentally passing). Saving/restoring is
        // sys-prop-only; the test trusts that no concurrent test has
        // a stale sys-prop bound to the same name.
        val prior = Option(System.getProperty("SYSL_WASM_SCALA_INTERP"))
        System.clearProperty("SYSL_WASM_SCALA_INTERP")
        try runCli("test", "--backend", "wasm32", corpus)
        finally prior match
          case Some(v) => System.setProperty("SYSL_WASM_SCALA_INTERP", v)
          case None    => System.clearProperty("SYSL_WASM_SCALA_INTERP")
      }
    withClue(out) {
      // The dispatcher returns Fail for each test (rather than aborting
      // the suite), so the runner output names the missing env var on
      // every failing test row.
      out should include("SYSL_WASM_SCALA_INTERP not set")
    }
  }

  "unknown SYSL_WASM_HOST value is rejected per-test with a clear message" taggedAs Slow in {
    requireToolchain()
    val (_, out) =
      withSysProp("SYSL_WASM_HOST", "bogus") {
        runCli("test", "--backend", "wasm32", corpus)
      }
    withClue(out) {
      out should include("unknown SYSL_WASM_HOST: 'bogus'")
    }
  }
}

object SyslCliWasmRunnerTests:
  private val staging = new AtomicInteger(0)
  def nextStagingId(): Int = staging.incrementAndGet()
