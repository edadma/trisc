#include <unistd.h>

/* dhello — chunk 9 dynamic-linker bring-up.
 *
 * Built with build-c-dyn.sh: PIE + libc.so + PT_INTERP set to
 * /lib/ld-musl-<arch>.so.1. Running this exercises:
 *   1. Kernel/PM detect PT_INTERP, load ld-musl at INTERP_BASE,
 *      build full auxv (chunk 9 stage A).
 *   2. ld-musl starts, reads its own program headers, applies its
 *      relocations, calls the constructors.
 *   3. ld-musl jumps to _start in the main exe (Scrt1.o), which
 *      calls __libc_start_main, which calls main below.
 *   4. write(1, ...) goes through ld-musl's libc — SYS_WRITE syscall.
 *
 * If we see "hello dyn\n", every step worked end to end. The check
 * is simple by design — extending to printf/fwrite/etc. comes later
 * once basic dynamic execution is proven.
 */
int main(void) {
    const char msg[] = "hello dyn\n";
    write(1, msg, sizeof(msg) - 1);
    return 0;
}
