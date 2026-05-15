package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.atomic.AtomicInteger

/** Chunk-2 sanity pin for the freestanding wasm32-wasi runtime.
 *
 *  Boundary contract: chunk 2 ships the runtime files under
 *  `sysl/runtime/wasm/` (imports.c, libc.c, llvm_intrinsics.c). Chunk 3 will
 *  wire `runOneWasm` into the CLI dispatcher (`--backend wasm32`); the full
 *  end-to-end runner test belongs there. This file covers the gap between
 *  those two chunks: it bypasses the not-yet-implemented `--backend wasm32`
 *  test path and instead drives the toolchain directly — codegen → clang →
 *  wasmtime — so a regression in any of the three runtime files surfaces
 *  before chunk 3 work even begins.
 *
 *  When chunk 3 lands, the runner-shaped end-to-end tests will live in
 *  `SyslCliWasmRunnerTests` next to the rv counterpart; this file stays
 *  because it pins the lower-level "the freestanding runtime alone is
 *  sound" contract regardless of dispatcher wiring.
 *
 *  Tools required: Homebrew LLVM 22 at `/opt/homebrew/opt/llvm/bin/clang`
 *  (overridable via `SYSL_WASM_CLANG`) and wasmtime at
 *  `/opt/homebrew/bin/wasmtime` (overridable via `SYSL_WASMTIME`). Tests
 *  cancel when either is absent so CI without a wasm host doesn't fail.
 *
 *  Tagged `Slow` because each test launches a subprocess (clang ~250ms,
 *  wasmtime ~30ms).
 */
class SyslCliWasmRuntimeTests extends AnyFreeSpec with Matchers {

  FileOps.instance = JvmFileOps

  private val clang: String  = sys.env.getOrElse("SYSL_WASM_CLANG", "/opt/homebrew/opt/llvm/bin/clang")
  private val wasmtime: String = sys.env.getOrElse("SYSL_WASMTIME", "/opt/homebrew/bin/wasmtime")
  private val runtimeDir: String = sys.env.getOrElse("SYSL_WASM_RUNTIME", "sysl/runtime/wasm")

  private val toolchainReady: Boolean =
    Files.exists(Paths.get(clang)) &&
    Files.exists(Paths.get(wasmtime)) &&
    Files.exists(Paths.get(runtimeDir)) &&
    Files.exists(Paths.get(runtimeDir, "imports.c")) &&
    Files.exists(Paths.get(runtimeDir, "libc.c")) &&
    Files.exists(Paths.get(runtimeDir, "llvm_intrinsics.c"))

  private def requireToolchain(): Unit =
    if !toolchainReady then
      cancel(s"wasm toolchain or runtime missing (clang=$clang wasmtime=$wasmtime runtime=$runtimeDir)")

  /** Run a sysl source string end-to-end and return (exit code, stdout). */
  private def runSyslOnWasm(label: String, source: String): (Int, String) =
    val unique = SyslCliWasmRuntimeTests.nextStagingId()
    val baseAbs = Paths.get("target/sysl_wasm_runtime").toAbsolutePath
    Files.createDirectories(baseAbs)
    val dir = baseAbs.resolve(s"${label}_$unique")
    Files.createDirectories(dir)

    // Drive codegen directly — no CLI dispatch (chunk 3 not yet wired). We
    // bypass `SyslDriver` (which enforces module-path-vs-directory matching)
    // so a single `module wasm_runtime_test` works for every test regardless
    // of the staging dir; the analyzer + codegen only need a well-formed
    // ProgramAST.
    val raw = source.replace("MODULE_BASE", "wasm_runtime_test")
    val tangled = LiterateRenderer.tangle(new LiterateParser().parse(raw))
    val parsed = (new SyslParser).parseProgram(tangled) match
      case Right(p) => p
      case Left(e)  => fail(s"parse failed: $e\ntangled source:\n$tangled")
    val typed = (new SyslAnalyzer).analyze(parsed)
    val ir = (new SyslLLVMCodegen("wasm32")).generate(typed)
    val irPath = dir.resolve("prog.ll")
    Files.writeString(irPath, ir)

    val wasmPath = dir.resolve("prog.wasm")
    val clangArgs = Seq(
      clang,
      "--target=wasm32-unknown-wasi",
      "-ffreestanding", "-nostdlib",
      "-Wl,--no-entry", "-Wl,--export=_start", "-Wl,--allow-undefined",
      Paths.get(runtimeDir, "imports.c").toString,
      Paths.get(runtimeDir, "libc.c").toString,
      Paths.get(runtimeDir, "llvm_intrinsics.c").toString,
      irPath.toString,
      "-O2", "-w",
      "-o", wasmPath.toString,
    )
    val clangLog = new StringBuilder
    val clangLogger = scala.sys.process.ProcessLogger(
      line => clangLog.append(line).append('\n'),
      line => clangLog.append(line).append('\n'),
    )
    val clangExit = scala.sys.process.Process(clangArgs).!(clangLogger)
    if clangExit != 0 then fail(s"clang failed (exit $clangExit):\n$clangLog\nIR:\n$ir")

    // Wasmtime: stdout flows through the WASI fd_write import, exit code via
    // proc_exit. Capture both.
    val outBuf = new StringBuilder
    val errBuf = new StringBuilder
    val wmLogger = scala.sys.process.ProcessLogger(
      line => outBuf.append(line).append('\n'),
      line => errBuf.append(line).append('\n'),
    )
    val proc = scala.sys.process.Process(Seq(wasmtime, "run", wasmPath.toString)).run(wmLogger)
    val deadline = System.currentTimeMillis() + 15000L
    while proc.isAlive() && System.currentTimeMillis() < deadline do Thread.sleep(20)
    if proc.isAlive() then
      proc.destroy()
      fail(s"wasmtime timed out after 15s; stdout=${outBuf.toString} stderr=${errBuf.toString}")
    val exitCode = proc.exitValue()
    // wasmtime writes user stderr (panic messages, abort traces) to its own
    // stderr; for our chunk-2 contract that text needs to come back to the
    // assertion site too. We concatenate so a panic-marker substring test
    // catches it regardless of stream.
    (exitCode, outBuf.toString + errBuf.toString)

  "puts a string and exits 0" taggedAs Slow in {
    requireToolchain()
    val (code, out) = runSyslOnWasm("hello",
      """    module MODULE_BASE
        |
        |    main() -> int
        |        puts("hello from chunk2")
        |        0
        |""".stripMargin)
    withClue(s"stdout/stderr: $out") {
      code shouldBe 0
      out should include("hello from chunk2")
    }
  }

  "nonzero main return propagates to wasmtime exit code" taggedAs Slow in {
    requireToolchain()
    val (code, _) = runSyslOnWasm("exitcode",
      """    module MODULE_BASE
        |
        |    main() -> int
        |        42
        |""".stripMargin)
    code shouldBe 42
  }

  "panic emits marker via fd_write(2) and aborts with exit 1" taggedAs Slow in {
    requireToolchain()
    val (code, out) = runSyslOnWasm("panic",
      """    module MODULE_BASE
        |
        |    main() -> int
        |        panic("from-wasm-chunk2")
        |        0
        |""".stripMargin)
    withClue(s"stdout/stderr: $out") {
      // abort() in our libc calls proc_exit(1), so wasmtime returns 1.
      code shouldBe 1
      out should include("panic: from-wasm-chunk2")
    }
  }

  "println(i32) goes through printf %d engine" taggedAs Slow in {
    requireToolchain()
    val (code, out) = runSyslOnWasm("println_int",
      """    module MODULE_BASE
        |
        |    main() -> int
        |        val x = 12345
        |        println(x)
        |        0
        |""".stripMargin)
    withClue(s"stdout/stderr: $out") {
      code shouldBe 0
      out should include("12345")
    }
  }

  "i64 multiply prints correctly via puts(str(...)) — native i64 ops on wasm" taggedAs Slow in {
    requireToolchain()
    val (code, out) = runSyslOnWasm("i64_mul",
      """    module MODULE_BASE
        |
        |    main() -> int
        |        val a = 1_000_000i64
        |        val b = 1_000i64
        |        puts(str(a * b))
        |        0
        |""".stripMargin)
    withClue(s"stdout/stderr: $out") {
      code shouldBe 0
      out should include("1000000000")
    }
  }

  "malloc + string concat — bump allocator delivers a fresh chunk" taggedAs Slow in {
    requireToolchain()
    val (code, out) = runSyslOnWasm("string_concat",
      """    module MODULE_BASE
        |
        |    main() -> int
        |        val a = "hello, "
        |        val b = "wasm world"
        |        puts(a + b)
        |        0
        |""".stripMargin)
    withClue(s"stdout/stderr: $out") {
      code shouldBe 0
      out should include("hello, wasm world")
    }
  }
}

object SyslCliWasmRuntimeTests:
  private val staging = new AtomicInteger(0)
  def nextStagingId(): Int = staging.incrementAndGet()
