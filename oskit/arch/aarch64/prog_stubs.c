/* Minimal libc stubs for standalone aarch64 SLIX programs.
 * Provides sbrk (using linker symbols) and C library functions
 * needed by LLVM-generated code. Mirrors oskit/arch/x86_64/prog_stubs.c. */

typedef unsigned long size_t;
typedef long ssize_t;
typedef unsigned char uint8_t;

/* Heap boundaries from linker script (prog.ld). */
extern char _heap_start;
extern char _heap_end;

static char *_brk = 0;

void *sbrk(int incr) {
    if (_brk == 0) _brk = &_heap_start;
    if (incr == 0) return _brk;
    char *old = _brk;
    if (_brk + incr > &_heap_end) return (void *)-1;
    _brk += incr;
    return old;
}

void *memcpy(void *dst, const void *src, size_t n) {
    uint8_t *d = dst; const uint8_t *s = src;
    for (size_t i = 0; i < n; i++) d[i] = s[i];
    return dst;
}

void *memset(void *dst, int c, size_t n) {
    uint8_t *d = dst;
    for (size_t i = 0; i < n; i++) d[i] = (uint8_t)c;
    return dst;
}

int memcmp(const void *a, const void *b, size_t n) {
    const uint8_t *pa = a, *pb = b;
    for (size_t i = 0; i < n; i++) if (pa[i] != pb[i]) return pa[i] - pb[i];
    return 0;
}

size_t strlen(const char *s) { size_t n = 0; while (s[n]) n++; return n; }

/* putchar via SYS_PUTC syscall (number 1). Uses inline aarch64 asm
 * matching SLIX's dispatcher ABI: x8 = syscall number, x0 = arg. */
static void prog_putc(int c) {
    register long x8 __asm__("x8") = 1;
    register long x0 __asm__("x0") = c;
    __asm__ volatile ("svc #0" : "+r"(x0) : "r"(x8) : "memory");
}

ssize_t write(int fd, const void *buf, size_t len) {
    (void)fd;
    const uint8_t *p = buf;
    for (size_t i = 0; i < len; i++) prog_putc(p[i]);
    return (ssize_t)len;
}

int putchar(int c) { prog_putc(c); return c; }
int printf(const char *fmt, ...) { (void)fmt; return 0; }
int snprintf(char *buf, size_t n, const char *fmt, ...) { (void)buf; (void)n; (void)fmt; return 0; }
int fflush(void *stream) { (void)stream; return 0; }

void exit(int status) {
    register long x8 __asm__("x8") = 3;
    register long x0 __asm__("x0") = status;
    __asm__ volatile ("svc #0" : : "r"(x8), "r"(x0));
    for (;;) __asm__ volatile ("wfi");
}

void abort(void) { exit(-1); }
