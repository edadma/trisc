/* Stub implementations of compiler-rt builtins that musl's libc.so
 * references but Homebrew's clang on darwin doesn't supply for
 * aarch64-linux-musl / x86_64-linux-musl targets.
 *
 * Phase 1 chunk 9 / Stage D: get libc.so to fully resolve so ld-musl
 * can load it as the dynamic linker. Hello-world dynamic binaries
 * don't call any of these (they're long-double / 128-bit-float
 * helpers and complex-multiply routines). If real compiler-rt for
 * the target is available later, drop this file and link the proper
 * builtins archive instead.
 *
 * setjmp / longjmp don't actually live in compiler-rt either —
 * they're musl-internal but on aarch64 the build emits external
 * references to them when --dynamic-list filters them out. We
 * provide weak no-op stubs so libc.so links; real setjmp/longjmp
 * still come from musl's own arch/aarch64/setjmp.s when callers
 * resolve via the dynamic-list-exported names.
 *
 * Every stub aborts if invoked. Only the load-time relocation
 * resolution matters for chunk-9 dyn-hello; if any of these get
 * called at run time it means the test reached a code path that
 * needs real implementations.
 */

static void compiler_rt_stub_unreachable(void)
{
    /* Hard-loop on aarch64 / x86_64. Replaces undefined-reference
     * errors with a deterministic deadlock that's easy to spot in
     * a debugger. */
    for (;;) {
#if defined(__aarch64__)
        __asm__ volatile("brk #0");
#elif defined(__x86_64__)
        __asm__ volatile("ud2");
#else
        __asm__ volatile("");
#endif
    }
}

#define STUB(name) \
    void name(void); \
    void name(void) { compiler_rt_stub_unreachable(); }

/* 128-bit float (TF) helpers — long double on aarch64. */
STUB(__addtf3)
STUB(__subtf3)
STUB(__multf3)
STUB(__divtf3)
STUB(__negtf2)
STUB(__eqtf2)
STUB(__netf2)
STUB(__lttf2)
STUB(__gttf2)
STUB(__letf2)
STUB(__getf2)
STUB(__unordtf2)
STUB(__extenddftf2)
STUB(__extendsftf2)
STUB(__trunctfsf2)
STUB(__trunctfdf2)
STUB(__floatsitf)
STUB(__floatunsitf)
STUB(__floatditf)
STUB(__floatunditf)
STUB(__fixtfsi)
STUB(__fixtfdi)
STUB(__fixunstfsi)
STUB(__fixunstfdi)

/* Complex multiply / divide (sf = float, dc = double complex,
 * sc = single complex, tc = long double complex). */
STUB(__mulsc3)
STUB(__muldc3)
STUB(__multc3)
STUB(__divsc3)
STUB(__divdc3)
STUB(__divtc3)
