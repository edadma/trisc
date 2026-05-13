package io.github.edadma.trisc

/** Chunk 1 of the RISC-V backend roadmap: verify the LLVM codegen emits the
  * right `target triple` and `target datalayout` directives for both rv64-elf
  * and rv32-elf. No clang or qemu is invoked here — these are pure IR-shape
  * checks against the strings the codegen prints. Runtime verification under
  * qemu-system-riscv* belongs in chunk 3 (CLI test runner wiring).
  *
  * The datalayout strings are the canonical LLVM values for these targets
  * (matches clang's `TargetInfo::getDataLayoutString` for `riscv64` and
  * `riscv32`). A mismatch here means LLVM will compute wrong struct layouts
  * downstream — values get loaded from wrong offsets — so a regression must
  * fail loudly. */
class SyslLLVMRiscVTargetTests extends SyslLLVMTestHelpers {

  private def compileFor(target: String, source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslLLVMCodegen(target)).generate(typed)

  private val trivial = """main() -> int = 0
                          |""".stripMargin

  // -- rv64-elf --

  "riscv64-elf emits rv64 triple" in {
    compileFor("riscv64-elf", trivial) should include("""target triple = "riscv64-unknown-elf"""")
  }

  "riscv64-elf emits rv64 datalayout" in {
    compileFor("riscv64-elf", trivial) should include(
      """target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128""""
    )
  }

  "riscv64 alias resolves to elf" in {
    compileFor("riscv64", trivial) should include("""target triple = "riscv64-unknown-elf"""")
  }

  // -- rv32-elf --

  "riscv32-elf emits rv32 triple" in {
    compileFor("riscv32-elf", trivial) should include("""target triple = "riscv32-unknown-elf"""")
  }

  "riscv32-elf emits rv32 datalayout" in {
    compileFor("riscv32-elf", trivial) should include(
      """target datalayout = "e-m:e-p:32:32-i64:64-n32-S128""""
    )
  }

  "riscv32 alias resolves to elf" in {
    compileFor("riscv32", trivial) should include("""target triple = "riscv32-unknown-elf"""")
  }

  // -- the two RV targets must not bleed into each other --

  "rv64 does not emit rv32 datalayout" in {
    compileFor("riscv64-elf", trivial) should not include "p:32:32"
  }

  "rv32 does not emit rv64 datalayout" in {
    compileFor("riscv32-elf", trivial) should not include "p:64:64"
  }

  // -- existing targets are not regressed --

  "x86_64-elf still emits its triple" in {
    compileFor("x86_64-elf", trivial) should include("""target triple = "x86_64-unknown-elf"""")
  }

  "aarch64-elf still emits its triple" in {
    compileFor("aarch64-elf", trivial) should include("""target triple = "aarch64-unknown-elf"""")
  }

  // -- the IR validates as something clang can at least parse — exercise a
  //    non-trivial program shape to make sure no codegen path bails because
  //    of the new target string. We don't link or run; the existence of the
  //    triple plus a well-formed function body is enough for chunk 1.

  "rv64 IR contains a main function for a non-trivial program" in {
    val ir = compileFor("riscv64-elf",
      """add(a: int, b: int) -> int = a + b
        |
        |main() -> int
        |    add(2, 40)
        |""".stripMargin)
    ir should include("define i32 @main()")
    ir should include("@add")
  }

  "rv32 IR contains a main function for a non-trivial program" in {
    val ir = compileFor("riscv32-elf",
      """add(a: int, b: int) -> int = a + b
        |
        |main() -> int
        |    add(2, 40)
        |""".stripMargin)
    ir should include("define i32 @main()")
    ir should include("@add")
  }
}
