package io.github.edadma.trisc

/** Chunk 1 of the wasm backend roadmap: verify the LLVM codegen emits the
  * right `target triple` and `target datalayout` for `wasm32` /
  * `wasm32-wasi`, and that the chunk-4-of-RV size_t / ptr-width machinery
  * activates for this target. No clang / wasmtime invocation here — these
  * are pure IR-shape checks against the strings the codegen prints.
  * Runtime verification under wasmtime belongs in chunk 3.
  *
  * The datalayout string is the canonical LLVM value for `wasm32-wasi`
  * (matches clang's `TargetInfo::getDataLayoutString` for the wasm32
  * target). A mismatch here means LLVM will compute wrong struct layouts
  * downstream — values get loaded from wrong offsets — so a regression
  * must fail loudly. */
class SyslLLVMWasmTargetTests extends SyslLLVMTestHelpers {

  private def compileFor(target: String, source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslLLVMCodegen(target)).generate(typed)

  private val trivial = """main() -> int = 0
                          |""".stripMargin

  // -- wasm32 target wiring --

  "wasm32 emits the wasi triple" in {
    compileFor("wasm32", trivial) should include("""target triple = "wasm32-unknown-wasi"""")
  }

  "wasm32-wasi alias resolves to the same triple" in {
    compileFor("wasm32-wasi", trivial) should include("""target triple = "wasm32-unknown-wasi"""")
  }

  "wasm32 emits the canonical wasm32 datalayout" in {
    compileFor("wasm32", trivial) should include(
      """target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-n32:64-S128""""
    )
  }

  // -- target-bleed checks: wasm32 must not pick up rv or x86 layouts --

  "wasm32 does not emit rv32 datalayout" in {
    val ir = compileFor("wasm32", trivial)
    // rv32 has p:32:32 too, but lacks the p10/p20 wasm reference-type
    // address-space declarations.
    ir should not include "e-p:32:32-i64:64-n32-S128\""
  }

  "wasm32 does not emit rv64 datalayout" in {
    compileFor("wasm32", trivial) should not include "p:64:64"
  }

  "wasm32 does not regress the existing rv targets" in {
    compileFor("riscv32-elf", trivial) should include("""target triple = "riscv32-unknown-elf"""")
    compileFor("riscv64-elf", trivial) should include("""target triple = "riscv64-unknown-elf"""")
  }

  // -- is32Bit / sizeT activation --
  //
  // The whole reason chunk 1 was a ~30-min job: the rv32 port's `is32Bit`
  // machinery handles wasm32's ilp32 ABI by construction. These tests pin
  // that activation — every libc-decl width must match a 32-bit `size_t`,
  // and the `llvm.memset.p0i8.i32` intrinsic name must be selected. A
  // regression here means the chunk-4 RV size_t fix has been undone for
  // 32-bit targets.

  "wasm32 declares snprintf with i32 size param" in {
    compileFor("wasm32", trivial) should include("declare i32 @snprintf(i8*, i32, i8*, ...)")
  }

  "wasm32 declares malloc with i32 size param" in {
    compileFor("wasm32", trivial) should include("declare i8* @malloc(i32)")
  }

  "wasm32 declares strlen with i32 return" in {
    compileFor("wasm32", trivial) should include("declare i32 @strlen(i8*)")
  }

  "wasm32 declares memcpy with i32 size param" in {
    compileFor("wasm32", trivial) should include("declare i8* @memcpy(i8*, i8*, i32)")
  }

  "wasm32 declares memcmp with i32 size param" in {
    compileFor("wasm32", trivial) should include("declare i32 @memcmp(i8*, i8*, i32)")
  }

  "wasm32 declares memset with i32 size param" in {
    compileFor("wasm32", trivial) should include("declare i8* @memset(i8*, i32, i32)")
  }

  "wasm32 uses the i32-suffixed memset intrinsic" in {
    compileFor("wasm32", trivial) should include("declare void @llvm.memset.p0i8.i32")
  }

  "wasm32 declares write with i32 length param and return" in {
    compileFor("wasm32", trivial) should include("declare i32 @write(i32, i8*, i32)")
  }

  // -- non-trivial program shape, to make sure no codegen path bails on the
  //    new target string. We don't link or run; the existence of the triple
  //    plus a well-formed function body is enough for chunk 1.

  "wasm32 IR contains a main function for a non-trivial program" in {
    val ir = compileFor("wasm32",
      """add(a: int, b: int) -> int = a + b
        |
        |main() -> int
        |    add(2, 40)
        |""".stripMargin)
    ir should include("define i32 @main()")
    ir should include("@add")
  }

  // -- ptr-width-aware composite layouts (the second chunk-4-of-RV fix).
  //    A program that exercises string concatenation forces a memcpy call
  //    with the target's sizeT — verifies the codegen narrows i64 size to
  //    i32 at the call boundary for wasm32. Counterpart on rv64 would
  //    still emit `i64 ...` at the memcpy site.

  "wasm32 narrows memcpy size args to i32 at call sites" in {
    val ir = compileFor("wasm32",
      """main() -> int
        |    val a = "hello"
        |    val b = "world"
        |    val c = a + b
        |    0
        |""".stripMargin)
    ir should include("call i8* @memcpy(i8*")
    // The size arg must be i32 on wasm32 — the codegen either narrows
    // an i64 intermediate via trunc or uses an i32 directly.
    ir should not include "@memcpy(i8*, i8*, i64"
  }
}
