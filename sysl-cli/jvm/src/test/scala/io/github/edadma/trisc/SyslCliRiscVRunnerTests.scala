package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

/** End-to-end tests for the rv64 / rv32 backend test runner.
 *
 *  These tests require:
 *    - Homebrew LLVM 22 at `/opt/homebrew/opt/llvm/bin/clang` (overridable via
 *      `SYSL_RV_CLANG`)
 *    - `qemu-system-riscv64` and `qemu-system-riscv32` (overridable via
 *      `SYSL_RV_QEMU64` / `SYSL_RV_QEMU32`)
 *    - The freestanding runtime under `sysl/runtime/rv/` (cwd-relative;
 *      overridable via `SYSL_RV_RUNTIME`)
 *
 *  When any of those are missing — typical on CI without QEMU installed — the
 *  whole suite skips. We test the *runner* (qemu launch, output capture,
 *  panic-marker parsing, OpenSBI banner stripping), not the codegen itself.
 *  Chunk-1's `SyslLLVMRiscVTargetTests` covers IR shape; this suite covers
 *  what happens after the IR is built.
 *
 *  Tagged `Slow` because each qemu launch is ~100ms wall on top of a one-time
 *  ~2s runtime build per xlen. The whole suite runs in ~5s.
 */
class SyslCliRiscVRunnerTests extends AnyFreeSpec with Matchers {

  FileOps.instance = JvmFileOps

  private val clang: String = sys.env.getOrElse("SYSL_RV_CLANG", "/opt/homebrew/opt/llvm/bin/clang")
  private val qemu64: String = sys.env.getOrElse("SYSL_RV_QEMU64", "/opt/homebrew/bin/qemu-system-riscv64")
  private val qemu32: String = sys.env.getOrElse("SYSL_RV_QEMU32", "/opt/homebrew/bin/qemu-system-riscv32")
  private val runtimeDir: String = sys.env.getOrElse("SYSL_RV_RUNTIME", "sysl/runtime/rv")
  private val corpus: String = "sysl/tests/riscv_smoke/"

  /** True iff every external tool the runner needs is present. We check at
    * startup time and short-circuit each test with `cancel` if anything's
    * missing — ScalaTest reports cancelled tests separately so they don't
    * pollute the failure column. */
  private val toolchainReady: Boolean =
    java.nio.file.Files.exists(java.nio.file.Paths.get(clang)) &&
    java.nio.file.Files.exists(java.nio.file.Paths.get(qemu64)) &&
    java.nio.file.Files.exists(java.nio.file.Paths.get(qemu32)) &&
    java.nio.file.Files.exists(java.nio.file.Paths.get(runtimeDir)) &&
    java.nio.file.Files.exists(java.nio.file.Paths.get(corpus))

  private def requireToolchain(): Unit =
    if !toolchainReady then
      cancel(s"RV toolchain or corpus not present (clang=$clang qemu64=$qemu64 qemu32=$qemu32 runtimeDir=$runtimeDir corpus=$corpus)")

  private def runCli(args: String*): (Int, String) = CliCapture.runCli(args)

  /** Stage a single .lsysl under `target/sysl_rv_runner/<label>_<n>/` so the
    * module declaration we embed inside matches the directory path the
    * driver derives from it. (The driver requires the declared `module
    * a.b.c` to be a prefix of the cwd-relative path with `/` -> `.`.)
    *
    * Returns (absoluteDir, cwdRelativeDir, modulePathPrefix). */
  private def stage(label: String): (Path, String, String) =
    val baseRel = "target/sysl_rv_runner"
    val baseAbs = java.nio.file.Paths.get(baseRel).toAbsolutePath
    Files.createDirectories(baseAbs)
    val unique = SyslCliRiscVRunnerTests.nextStagingId()
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

  "riscv64 backend runs the smoke corpus end-to-end" taggedAs Slow in {
    requireToolchain()
    val (code, out) = runCli("test", "--backend", "riscv64", corpus)
    withClue(out) {
      code shouldBe 0
      out should include("running 7 tests (backend: riscv64)")
      out should include("7 passed, 0 failed")
    }
  }

  "riscv32 backend runs the smoke corpus end-to-end" taggedAs Slow in {
    requireToolchain()
    val (code, out) = runCli("test", "--backend", "riscv32", corpus)
    withClue(out) {
      code shouldBe 0
      out should include("running 7 tests (backend: riscv32)")
      out should include("7 passed, 0 failed")
    }
  }

  "unexpected panic on rv64 reports the panic message from captured stdout" taggedAs Slow in {
    requireToolchain()
    val (abs, rel, modBase) = stage("neg_panic")
    writeFile(abs, "neg.lsysl",
      s"""    module $modBase
         |
         |    #test
         |    test_unexpected_panic() -> unit
         |        panic("kaboom-marker")
         |""".stripMargin)
    val (code, out) = runCli("test", "--backend", "riscv64", rel)
    withClue(out) {
      // Exit code from `executeTest` is nonzero when any test fails; the
      // CliCapture wrapper maps that into runCli's exit code (1 for runtime
      // throw). The important check is what got reported.
      out should include("0 passed, 1 failed")
      // The runner must surface the panic message from captured stdout.
      out should include("panic: kaboom-marker")
      // The output preamble lines should also appear (echo of the failing run).
      out should include("test_unexpected_panic")
    }
  }

  "mismatched should_panic expected-message is flagged on rv64" taggedAs Slow in {
    requireToolchain()
    val (abs, rel, modBase) = stage("neg_msg")
    writeFile(abs, "msg.lsysl",
      s"""    module $modBase
         |
         |    #test(should_panic: "expected-msg")
         |    test_wrong_msg() -> unit
         |        panic("actual-msg")
         |""".stripMargin)
    val (code, out) = runCli("test", "--backend", "riscv64", rel)
    withClue(out) {
      out should include("0 passed, 1 failed")
      out should include("panic message did not contain 'expected-msg'")
      out should include("actual-msg")
    }
  }

  /** Regression for the std/testing/testing panic-detection miss surfaced
    *  during the chunk-4 rv64 sweep. The test's `assert_eq` helper prints
    *  the values it got/wanted via `print` (no newline) immediately before
    *  panicking, so the SBI console emits `12panic: mismatch: got 1, want 2`
    *  on one line. The earlier `panicMarker` used `startsWith("panic: ")` —
    *  which misses that layout — and the runner reported "expected panic,
    *  got normal return". `panicMarker` now searches anywhere in the line. */
  "should_panic test that prints values without newline before panicking is recognized as a panic on rv64" taggedAs Slow in {
    requireToolchain()
    val (abs, rel, modBase) = stage("neg_no_nl")
    writeFile(abs, "neg_no_nl.lsysl",
      s"""    module $modBase
         |
         |    #test(should_panic: "mismatch")
         |    test_print_then_panic() -> unit
         |        print("1")
         |        print("2")
         |        panic("mismatch: got 1, want 2")
         |""".stripMargin)
    val (code, out) = runCli("test", "--backend", "riscv64", rel)
    withClue(out) {
      code shouldBe 0
      out should include("1 passed, 0 failed")
    }
  }

  "unknown --backend is rejected before any qemu/clang invocation" taggedAs Slow in {
    val (code, out) = runCli("test", "--backend", "bogus", corpus)
    withClue(out) {
      // scopt prints to stderr and returns None, which CliCapture maps to 2.
      code shouldBe 2
      out should include("Unknown backend")
    }
  }
}

object SyslCliRiscVRunnerTests:
  private val staging = new AtomicInteger(0)
  def nextStagingId(): Int = staging.incrementAndGet()

