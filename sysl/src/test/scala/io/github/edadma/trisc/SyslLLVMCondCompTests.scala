package io.github.edadma.trisc

/** `#if` / `#else` conditional compilation on the LLVM backend.
  *
  * Conditional compilation is resolved at the SyslDriver level
  * (`resolveCondDecls`) *before* typing and codegen — by the time LLVM sees the
  * program, only the chosen branch's declarations remain in the AST. So the LLVM
  * backend never sees a `CondDeclAST` and doesn't need a `TConditional` node;
  * the cross-backend mechanism is entirely upstream.
  *
  * These tests pin that contract by compiling a program with `#if/#else`
  * conditions toggled by the `config` map and verifying the IR contains the
  * code from the chosen branch and *not* the dropped branch. Closes audit
  * item #9. */
class SyslLLVMCondCompTests extends SyslLLVMTestHelpers {

  /** Compile sources to LLVM IR with a conditional-compilation config map. */
  private def compileWithConfig(sources: Map[String, String], config: Map[String, String]): String =
    val driver = new SyslDriver(config = config)
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    (new SyslLLVMCodegen).generate(merged)

  /** Compile + link + run with config. */
  private def runWithConfig(sources: Map[String, String], config: Map[String, String]): Int =
    // We reuse the same temp-dir runner as runLLVM, but with config-aware IR.
    val ir = compileWithConfig(sources, config)
    val tmp = java.nio.file.Files.createTempDirectory("sysl-llvm-condcomp")
    try
      val ll = tmp.resolve("test.ll")
      val exe = tmp.resolve("test")
      java.nio.file.Files.writeString(ll, ir)
      val cc = scala.sys.process.Process(Seq("clang", "-w", ll.toString, "-o", exe.toString)).!
      if cc != 0 then fail(s"clang failed (exit $cc):\n$ir")
      scala.sys.process.Process(exe.toString).!
    finally
      java.nio.file.Files.walk(tmp).sorted(java.util.Comparator.reverseOrder()).forEach(java.nio.file.Files.deleteIfExists)

  "#if-then branch runs when symbol is true" in {
    runWithConfig(
      Map("app" ->
        """#if HAS_FPU
          |compute() -> int = 42
          |#else
          |compute() -> int = 99
          |#endif
          |
          |main() -> int = compute()
          |""".stripMargin),
      Map("HAS_FPU" -> "true")
    ) shouldBe 42
  }

  "#else branch runs when symbol is false" in {
    runWithConfig(
      Map("app" ->
        """#if HAS_FPU
          |compute() -> int = 42
          |#else
          |compute() -> int = 99
          |#endif
          |
          |main() -> int = compute()
          |""".stripMargin),
      Map.empty
    ) shouldBe 99
  }

  "#if equality on string symbol routes to the correct branch" in {
    runWithConfig(
      Map("app" ->
        """#if ARCH == "trisc"
          |word_size() -> int = 16
          |#else
          |word_size() -> int = 64
          |#endif
          |
          |main() -> int = word_size()
          |""".stripMargin),
      Map("ARCH" -> "trisc")
    ) shouldBe 16
  }

  "#if equality on numeric symbol" in {
    runWithConfig(
      Map("app" ->
        """#if WORD == 64
          |bits() -> int = 64
          |#else
          |bits() -> int = 32
          |#endif
          |
          |main() -> int = bits()
          |""".stripMargin),
      Map("WORD" -> "64")
    ) shouldBe 64
  }

  "dropped branch is not present in the emitted IR" in {
    val ir = compileWithConfig(
      Map("app" ->
        """#if INCLUDE_FAST_PATH
          |fast_path() -> int = 1
          |#else
          |slow_path() -> int = 2
          |#endif
          |
          |main() -> int = 0
          |""".stripMargin),
      Map("INCLUDE_FAST_PATH" -> "true")
    )
    // Functions emit under their mangled names. The chosen function should appear;
    // the dropped one must be absent.
    ir should include("fast_path")
    ir should not include "slow_path"
  }

  "negated condition routes correctly" in {
    runWithConfig(
      Map("app" ->
        """#if !BARE_METAL
          |status() -> int = 1
          |#else
          |status() -> int = 2
          |#endif
          |
          |main() -> int = status()
          |""".stripMargin),
      Map("BARE_METAL" -> "true")
    ) shouldBe 2
  }

  "nested #if resolves both layers" in {
    runWithConfig(
      Map("app" ->
        """#if HAS_FPU
          |#if PRECISION == "double"
          |fp_bits() -> int = 64
          |#else
          |fp_bits() -> int = 32
          |#endif
          |#else
          |fp_bits() -> int = 0
          |#endif
          |
          |main() -> int = fp_bits()
          |""".stripMargin),
      Map("HAS_FPU" -> "true", "PRECISION" -> "double")
    ) shouldBe 64
  }
}
