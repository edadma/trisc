/* Compiler-rt placeholder for wasm32-wasi.
 *
 * On wasm32, all the rv32-style compiler-rt symbols (__udivdi3, __divdi3,
 * __fixdfdi, __floatdidf, ...) are *not* needed: the wasm specification
 * includes native 64-bit integer ops (`i64.div_u`, `i64.div_s`, ...) and
 * native double-precision float ops (`f64.convert_i64_u`, etc.) on every
 * conforming engine. Clang on wasm32 lowers everything inline.
 *
 * This file is kept (1) so the runtime source list mirrors the RV layout
 * exactly, simplifying the chunk-3 test-runner port; and (2) as a landing
 * pad if some future LLVM release decides to emit a compiler-rt helper for
 * an op we don't expect — adding it here is one line.
 *
 * Intentionally empty body. clang -ffreestanding -nostdlib accepts a TU
 * with no extern decls.
 */
