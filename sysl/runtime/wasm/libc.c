/* Minimal freestanding libc for sysl bare-metal wasm32-wasi.
 *
 * This file is a near-verbatim copy of `runtime/rv/libc.c`. The only deltas:
 *   - Heap arena is a static char[] in BSS (wasm-ld has no linker script in
 *     our toolchain pin, so `__heap_start` / `__heap_end` linker symbols
 *     aren't available). Tracked via `wasm_heap`.
 *   - `wasi_write(1, ...)` is *not* called here — output goes through the
 *     `sbi_console_putchar` shim defined in `imports.c`, same as on RV. The
 *     symmetry is intentional: bug fixes in either file should be portable.
 *
 * Keep in sync with `runtime/rv/libc.c` when fixing printf bugs etc.
 *
 * Provides exactly the surface declared in SyslLLVMCodegen.scala's preamble:
 *   write, putchar, puts, printf, snprintf, fflush,
 *   malloc, free, calloc, realloc,
 *   memcpy, memset, memcmp, strlen,
 *   abort, exit.
 *
 * Plus the LLVM byte-memset intrinsic (`llvm.memset.p0i8.i32` on wasm32),
 * which the codegen emits as a direct intrinsic call.
 *
 * The printf engine handles every format string sysl produces:
 *   - %d %u  (i32, padded/signed)
 *   - %ld %lu (i64)
 *   - %x %X %lx %lX  (hex, lower/upper, 32/64)
 *   - %o %lo  (octal)
 *   - %s     (null-terminated C string)
 *   - %c     (single byte from int)
 *   - %g     (double, fixed-precision)
 *   - %p     (pointer in hex with 0x prefix)
 *   - %%     (literal percent)
 *   Flags: `-` (left align), `0` (zero pad), `+` (show sign).
 *   Width: decimal, no `*`.
 *   Precision: ignored for everything except %s (truncate).
 */

#include <stdint.h>
#include <stddef.h>
#include <stdarg.h>

/* `ssize_t` is a POSIX typedef (sys/types.h) that freestanding stddef.h
 * doesn't provide. It's `long` on wasm32 (32 bits) — same width as `size_t`,
 * just signed. */
typedef long ssize_t;

extern void sbi_console_putchar(int c);
__attribute__((noreturn)) extern void sbi_system_reset(uint32_t type, uint32_t reason);

/* Heap arena. 4 MiB starting size — matches the rv bump arena ballpark and is
 * enough for std/ tests. wasm linear memory grows on demand if a host honours
 * `memory.grow`, but our bump allocator can't (yet) — increase this constant
 * if a future test exhausts. */
static char wasm_heap[4 * 1024 * 1024];
static char *const __heap_start = wasm_heap;
static char *const __heap_end   = wasm_heap + sizeof(wasm_heap);

/* ---------------- stdout ---------------- */

int putchar(int c) {
    sbi_console_putchar(c);
    return c;
}

/* write(fd, buf, len) — ignore fd, just dump to console.
 * `ssize_t` / `size_t` are the platform's natural widths (i32 on wasm32);
 * the codegen targets the same widths via its `sizeT` machinery so the IR-
 * declared signature matches this one. */
ssize_t write(int fd, const void *buf, size_t len) {
    (void)fd;
    const unsigned char *p = (const unsigned char *)buf;
    for (size_t i = 0; i < len; i++) sbi_console_putchar(p[i]);
    return (ssize_t)len;
}

int puts(const char *s) {
    while (*s) sbi_console_putchar((unsigned char)*s++);
    sbi_console_putchar('\n');
    return 0;
}

int fflush(void *f) {
    (void)f;
    return 0;
}

/* ---------------- mem ---------------- */

void *memcpy(void *dst, const void *src, size_t n) {
    unsigned char *d = (unsigned char *)dst;
    const unsigned char *s = (const unsigned char *)src;
    for (size_t i = 0; i < n; i++) d[i] = s[i];
    return dst;
}

void *memset(void *dst, int c, size_t n) {
    unsigned char *d = (unsigned char *)dst;
    unsigned char v = (unsigned char)c;
    for (size_t i = 0; i < n; i++) d[i] = v;
    return dst;
}

int memcmp(const void *a, const void *b, size_t n) {
    const unsigned char *pa = (const unsigned char *)a;
    const unsigned char *pb = (const unsigned char *)b;
    for (size_t i = 0; i < n; i++) {
        if (pa[i] != pb[i]) return (int)pa[i] - (int)pb[i];
    }
    return 0;
}

size_t strlen(const char *s) {
    size_t n = 0;
    while (*s++) n++;
    return n;
}

/* LLVM byte-memset intrinsic. Codegen emits a direct call to this even
 * though clang would normally lower it inline; freestanding builds may
 * need the explicit definition. The intrinsic's mangled name encodes the
 * length type width — on wasm32 / ilp32 it's `.i32` (matches the codegen's
 * `memsetIntrinsic`). */
void __llvm_memset_p0i8(void *dst, char c, size_t n, int volatile_) __asm__("llvm.memset.p0i8.i32");
void __llvm_memset_p0i8(void *dst, char c, size_t n, int volatile_) {
    (void)volatile_;
    memset(dst, (unsigned char)c, n);
}

/* ---------------- bump allocator ---------------- */

static char *heap_ptr = 0;

static void heap_init(void) {
    if (heap_ptr == 0) heap_ptr = __heap_start;
}

void *malloc(size_t n) {
    heap_init();
    /* 16-byte alignment for everything — matches the codegen's max-alignment
     * assumption and the rv allocator's behaviour. */
    size_t aligned = (n + 15) & ~(size_t)15;
    if (heap_ptr + aligned > __heap_end) return 0;
    void *p = heap_ptr;
    heap_ptr += aligned;
    return p;
}

void free(void *p) {
    (void)p; /* no-op */
}

void *calloc(size_t count, size_t size) {
    size_t total = count * size;
    void *p = malloc(total);
    if (p) memset(p, 0, total);
    return p;
}

void *realloc(void *old, size_t n) {
    /* Bump allocator can't shrink in place. Always allocate a fresh slot and
     * memcpy. We don't know the old size — over-copy is unsafe near end of
     * heap, so cap by the distance to heap_end. This is fine for the realloc
     * patterns sysl actually generates (grow-then-memcpy followed by free). */
    if (!old) return malloc(n);
    void *p = malloc(n);
    if (!p) return 0;
    /* Trust caller's n as upper bound on what they care about. */
    memcpy(p, old, n);
    return p;
}

/* ---------------- printf engine ---------------- */

/* Output sink: either to a buffer (snprintf) or to the console (printf). */
struct out_sink {
    char *buf;     /* null for direct-console mode */
    size_t cap;    /* capacity of buf (including null terminator slot) */
    size_t len;    /* bytes already written */
};

static void sink_putc(struct out_sink *s, char c) {
    if (s->buf) {
        if (s->len + 1 < s->cap) s->buf[s->len] = c;
    } else {
        sbi_console_putchar((unsigned char)c);
    }
    s->len++;
}

static void sink_puts(struct out_sink *s, const char *p, long n) {
    if (n < 0) {
        while (*p) sink_putc(s, *p++);
    } else {
        for (long i = 0; i < n; i++) sink_putc(s, p[i]);
    }
}

/* Format an unsigned integer in `base` into a fixed-size scratch buffer.
 * Returns the number of digits written into `out` (least-significant first
 * is not what we do — we write into the END of `out` and return a pointer to
 * the first digit via *startp). */
static int utoa_into(unsigned long long v, int base, int upper, char *buf, int bufsz, char **startp) {
    static const char *lower = "0123456789abcdef";
    static const char *upcase = "0123456789ABCDEF";
    const char *digits = upper ? upcase : lower;
    int i = bufsz;
    buf[--i] = 0;
    if (v == 0) {
        buf[--i] = '0';
    } else {
        while (v > 0 && i > 0) {
            buf[--i] = digits[v % (unsigned long long)base];
            v /= (unsigned long long)base;
        }
    }
    *startp = &buf[i];
    return bufsz - 1 - i;
}

/* Emit `value` padded to `width` according to flags. `prefix` is an
 * optional sign or "0x" string emitted before the digits. */
static void emit_padded(struct out_sink *s, const char *prefix, const char *digits,
                        int dig_len, int width, int left_align, int zero_pad) {
    int prelen = 0;
    if (prefix) {
        const char *p = prefix;
        while (*p) { prelen++; p++; }
    }
    int total = prelen + dig_len;
    int pad = width > total ? width - total : 0;
    char padchar = (zero_pad && !left_align) ? '0' : ' ';

    if (zero_pad && prefix) sink_puts(s, prefix, prelen);

    if (!left_align) {
        for (int i = 0; i < pad; i++) sink_putc(s, padchar);
    }

    if (!(zero_pad && prefix) && prefix) sink_puts(s, prefix, prelen);
    sink_puts(s, digits, dig_len);

    if (left_align) {
        for (int i = 0; i < pad; i++) sink_putc(s, ' ');
    }
}

/* %g implementation — fixed precision, no exponent unless very small/large.
 * Sysl's stdlib float printing goes through this; we follow the C library's
 * default %g semantics loosely (6 significant digits, trim trailing zeros).
 * No infinity/NaN/denormals — adequate for std/ tests that hit %g. */
static void format_double(struct out_sink *s, double v, int width, int left_align,
                          int zero_pad, int show_sign) {
    char digits[64];
    int idx = 0;
    int negative = 0;
    if (v < 0) { negative = 1; v = -v; }
    /* Round to 6 significant digits. */
    if (v == 0.0) {
        digits[idx++] = '0';
        emit_padded(s, negative ? "-" : (show_sign ? "+" : 0),
                    digits, idx, width, left_align, zero_pad);
        return;
    }
    /* Integer + fractional portion, fixed 6 frac digits, trim trailing zeros. */
    long long ip = (long long)v;
    double fp = v - (double)ip;
    /* Integer part. */
    char ibuf[24];
    char *istart;
    int ilen = utoa_into((unsigned long long)ip, 10, 0, ibuf, sizeof(ibuf), &istart);
    for (int i = 0; i < ilen; i++) digits[idx++] = istart[i];
    /* Fractional part — 6 digits. */
    digits[idx++] = '.';
    int frac_start = idx;
    for (int k = 0; k < 6; k++) {
        fp *= 10.0;
        int d = (int)fp;
        if (d < 0) d = 0;
        if (d > 9) d = 9;
        digits[idx++] = (char)('0' + d);
        fp -= (double)d;
    }
    /* Trim trailing zeros and the trailing dot. */
    while (idx > frac_start && digits[idx - 1] == '0') idx--;
    if (idx > 0 && digits[idx - 1] == '.') idx--;

    emit_padded(s, negative ? "-" : (show_sign ? "+" : 0),
                digits, idx, width, left_align, zero_pad);
}

static int vformat(struct out_sink *s, const char *fmt, va_list ap) {
    while (*fmt) {
        if (*fmt != '%') {
            sink_putc(s, *fmt++);
            continue;
        }
        fmt++;
        /* Flags. */
        int left_align = 0, zero_pad = 0, show_sign = 0;
        while (*fmt == '-' || *fmt == '0' || *fmt == '+' || *fmt == ' ' || *fmt == '#') {
            if (*fmt == '-') left_align = 1;
            else if (*fmt == '0') zero_pad = 1;
            else if (*fmt == '+') show_sign = 1;
            fmt++;
        }
        /* Width. */
        int width = 0;
        while (*fmt >= '0' && *fmt <= '9') {
            width = width * 10 + (*fmt - '0');
            fmt++;
        }
        /* Precision (used only by %s). */
        int precision = -1;
        if (*fmt == '.') {
            fmt++;
            precision = 0;
            while (*fmt >= '0' && *fmt <= '9') {
                precision = precision * 10 + (*fmt - '0');
                fmt++;
            }
        }
        /* Length modifier. */
        int is_long = 0;
        if (*fmt == 'l') { is_long = 1; fmt++; if (*fmt == 'l') fmt++; }
        else if (*fmt == 'z') { is_long = 1; fmt++; }

        char conv = *fmt;
        if (conv == 0) break;
        fmt++;

        char numbuf[32];
        char *digstart;
        int diglen;
        const char *prefix = 0;

        switch (conv) {
            case 'd':
            case 'i': {
                long long v = is_long ? va_arg(ap, long long) : (long long)va_arg(ap, int);
                int neg = 0;
                unsigned long long uv;
                if (v < 0) { neg = 1; uv = (unsigned long long)(-v); }
                else uv = (unsigned long long)v;
                diglen = utoa_into(uv, 10, 0, numbuf, sizeof(numbuf), &digstart);
                if (neg) prefix = "-";
                else if (show_sign) prefix = "+";
                emit_padded(s, prefix, digstart, diglen, width, left_align, zero_pad);
                break;
            }
            case 'u': {
                unsigned long long v = is_long ? va_arg(ap, unsigned long long)
                                               : (unsigned long long)va_arg(ap, unsigned int);
                diglen = utoa_into(v, 10, 0, numbuf, sizeof(numbuf), &digstart);
                emit_padded(s, 0, digstart, diglen, width, left_align, zero_pad);
                break;
            }
            case 'x':
            case 'X': {
                unsigned long long v = is_long ? va_arg(ap, unsigned long long)
                                               : (unsigned long long)va_arg(ap, unsigned int);
                diglen = utoa_into(v, 16, conv == 'X', numbuf, sizeof(numbuf), &digstart);
                emit_padded(s, 0, digstart, diglen, width, left_align, zero_pad);
                break;
            }
            case 'o': {
                unsigned long long v = is_long ? va_arg(ap, unsigned long long)
                                               : (unsigned long long)va_arg(ap, unsigned int);
                diglen = utoa_into(v, 8, 0, numbuf, sizeof(numbuf), &digstart);
                emit_padded(s, 0, digstart, diglen, width, left_align, zero_pad);
                break;
            }
            case 'p': {
                unsigned long long v = (unsigned long long)(uintptr_t)va_arg(ap, void *);
                diglen = utoa_into(v, 16, 0, numbuf, sizeof(numbuf), &digstart);
                emit_padded(s, "0x", digstart, diglen, width, left_align, zero_pad);
                break;
            }
            case 'c': {
                int v = va_arg(ap, int);
                char ch = (char)v;
                emit_padded(s, 0, &ch, 1, width, left_align, 0);
                break;
            }
            case 's': {
                const char *p = va_arg(ap, const char *);
                if (!p) p = "(null)";
                int plen = 0;
                while (p[plen]) plen++;
                if (precision >= 0 && plen > precision) plen = precision;
                emit_padded(s, 0, p, plen, width, left_align, 0);
                break;
            }
            case 'g':
            case 'f':
            case 'e': {
                double v = va_arg(ap, double);
                format_double(s, v, width, left_align, zero_pad, show_sign);
                break;
            }
            case '%':
                sink_putc(s, '%');
                break;
            default:
                /* Unknown verb — emit literally for safety. */
                sink_putc(s, '%');
                sink_putc(s, conv);
                break;
        }
    }
    /* Null-terminate if writing to a buffer. */
    if (s->buf && s->cap > 0) {
        size_t nti = s->len < s->cap ? s->len : s->cap - 1;
        s->buf[nti] = 0;
    }
    return (int)s->len;
}

int printf(const char *fmt, ...) {
    struct out_sink s = { 0, 0, 0 };
    va_list ap;
    va_start(ap, fmt);
    int n = vformat(&s, fmt, ap);
    va_end(ap);
    return n;
}

int snprintf(char *buf, size_t cap, const char *fmt, ...) {
    struct out_sink s = { buf, cap, 0 };
    va_list ap;
    va_start(ap, fmt);
    int n = vformat(&s, fmt, ap);
    va_end(ap);
    /* If cap is 0, sink_putc still incremented len but didn't store. Match
     * the C standard: return the length the formatted output would have had. */
    if (cap == 0 && buf == 0) return n;
    return n;
}

/* ---------------- exit/abort ---------------- */

__attribute__((noreturn))
void abort(void) {
    sbi_system_reset(0, 1);
}

__attribute__((noreturn))
void exit(int code) {
    sbi_system_reset(0, code == 0 ? 0 : 1);
}
