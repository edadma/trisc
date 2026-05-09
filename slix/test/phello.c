#include <stdio.h>
#include <math.h>

/* phello — chunk 10 dynamic libc bring-up.
 *
 * Built with build-c-dyn.sh: PIE + libc.so + PT_INTERP set to
 * /lib/ld-musl-<arch>.so.1. Compared to chunk-9's dhello (which only
 * used write(1, ...) — a single direct syscall), this exercises:
 *
 *   1. /usr/lib resolution. Chunk 10 reorganizes the ramdisk so /lib
 *      carries only ld-musl-<arch>.so.1 and shared objects (libc.so,
 *      libm.so) live in /usr/lib per FHS. ld-musl finds libc.so via
 *      its default search path (/lib:/usr/local/lib:/usr/lib).
 *   2. Real libc — printf goes through vfprintf, __stdio_write,
 *      writev. The FILE* buffer comes from malloc, which exercises
 *      mmap-or-brk + the kernel's anon-page allocator + the per-FILE
 *      lock and the libc atomic-cmpxchg helpers.
 *   3. sqrt() resolves from libc.so (musl unifies math into libc;
 *      libm.a is empty). Demonstrates symbol lookup across libraries
 *      lined up at runtime.
 *
 * The integer cast on sqrt() is deliberate: musl's vfprintf
 * unconditionally extends every %a/%e/%f/%g argument from double to
 * long double via __extenddftf2, which ld-musl resolves from our
 * compiler_rt stubs at /slix/musl-compat/compiler_rt_stubs.c — and
 * those stubs trap (`brk #0`/`ud2`) when called. Until real
 * compiler-rt builtins ship for aarch64-linux-musl / x86_64-linux-
 * musl, floating-point printf isn't usable. %d sidesteps fmt_fp
 * entirely and still exercises the dynamic-link + stdio + malloc
 * surface that chunk 10 cares about.
 *
 * If we see "hello printf, sqrt(2.0)*1e6 = 1414213\n", every step
 * worked: dynamic linker bring-up, libc.so under /usr/lib, sqrt via
 * libc, printf integer formatting through vfprintf, and the stdout
 * line buffering that flushes on '\n'.
 */
int main(void) {
    int s = (int)(sqrt(2.0) * 1000000.0);
    printf("hello printf, sqrt(2.0)*1e6 = %d\n", s);
    return 0;
}
