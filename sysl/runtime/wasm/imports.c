/* WASI snapshot-preview1 imports + entry stub for sysl's wasm32 LLVM backend.
 *
 * Two imports cover the entire surface needed by the libc port:
 *
 *   - fd_write(fd, iovs, iovs_len, nwritten)  — writes scatter/gather buffers
 *     to a file descriptor; we use it for stdout (fd=1) and stderr (fd=2).
 *   - proc_exit(code)                          — terminates the wasm instance
 *     with the given exit code; the host (wasmtime / scala interp / browser)
 *     propagates it as the process exit code.
 *
 * Everything else — putchar, printf, malloc, etc. — lives in libc.c and routes
 * through the two `sbi_console_putchar` / `sbi_system_reset` shims declared
 * here. Using the same shim names as the RV runtime lets libc.c be a near-
 * verbatim copy of `runtime/rv/libc.c` (the only real divergence is the heap
 * arena, which can't reference a linker symbol on wasm).
 *
 * The `_start` export below is what wasmtime calls by default for a "command"
 * module: run `main()`, then propagate its return code via `proc_exit`.
 * `--export=_start` in the wasm-ld invocation re-exports the symbol; otherwise
 * `-Wl,--no-entry` would strip it out.
 */

#include <stdint.h>
#include <stddef.h>

/* `WASI_IMPORT(name)` — paired pragma + attribute that places the next decl
 * into the `wasi_snapshot_preview1` import module with the given import name.
 * Newer clangs accept `__import_module__` as both pragma and attribute; we
 * use the attribute form for compatibility with LLVM 14+ (which is what the
 * pin in the roadmap targets). */
#define WASI_IMPORT(name) \
    __attribute__((import_module("wasi_snapshot_preview1"), import_name(name)))

/* WASI `ciovec_t`: a {pointer, length} pair in linear memory. Both fields are
 * 4-byte i32 on wasm32 (the only ABI we target — wasm64 is experimental). */
struct __wasi_ciovec_t {
    const void *buf;
    uint32_t buf_len;
};

WASI_IMPORT("fd_write")
extern uint16_t __wasi_fd_write(uint32_t fd,
                                const struct __wasi_ciovec_t *iovs,
                                uint32_t iovs_len,
                                uint32_t *nwritten);

WASI_IMPORT("proc_exit")
__attribute__((noreturn))
extern void __wasi_proc_exit(uint32_t code);

/* Internal helper: best-effort write of `len` bytes from `buf` to fd. WASI
 * may return a partial nwritten; we retry once. If WASI signals an errno (16-
 * bit return != 0) we silently drop — the only call sites are stdout/stderr
 * inside libc, and there is no useful errno propagation available. */
static void wasi_write(uint32_t fd, const void *buf, uint32_t len) {
    struct __wasi_ciovec_t iov;
    uint32_t written = 0;
    iov.buf = buf;
    iov.buf_len = len;
    if (__wasi_fd_write(fd, &iov, 1, &written) != 0) return;
    if (written < len) {
        iov.buf = (const char *)buf + written;
        iov.buf_len = len - written;
        __wasi_fd_write(fd, &iov, 1, &written);
    }
}

/* SBI-compat shims. The RV libc.c calls these for every byte of console
 * output and once at process exit. By giving them the same names + signatures
 * here, libc.c is portable verbatim. */
void sbi_console_putchar(int c) {
    unsigned char ch = (unsigned char)c;
    wasi_write(1, &ch, 1);
}

__attribute__((noreturn))
void sbi_system_reset(uint32_t type, uint32_t reason) {
    (void)type;  /* on wasm there is no shutdown vs cold-reboot distinction */
    __wasi_proc_exit(reason);
    /* `proc_exit` is noreturn; the loop satisfies the compiler when the
     * import is dropped at link-time (e.g. a future static-analysis pass). */
    for (;;) {}
}

/* WASI write(fd, buf, len) exposure — libc's `write()` falls through to
 * `sbi_console_putchar` byte-at-a-time which is acceptably slow for std/
 * but wasteful for bulk output. A future optimization is to have libc's
 * `write()` call `wasi_write` directly; for now keep the SBI symmetry. */

/* Entry stub. wasm-ld emits `_start` as the default entry for command modules;
 * `--export=_start` keeps it after `--no-entry` (which prevents clang from
 * pulling in the WASI libc's own _start). */
extern int main(void);

__attribute__((export_name("_start")))
void _start(void) {
    int rc = main();
    __wasi_proc_exit((uint32_t)rc);
}
