/* Minimal libc stubs required by Sysl's LLVM preamble.
 * These are declared by every compiled .ll file.
 * Everything else (UART, PIC, PIT, IDT) is in Sysl. */

typedef unsigned long size_t;
typedef long ssize_t;
typedef unsigned char uint8_t;

/* uart_putc is provided by Sysl runtime — forward declare */
extern void oskit_arch_x86_64__uart_putc(int c);

/* sbrk — extend heap for std.alloc's Sysl allocator.
 * Uses a static BSS array to avoid overlapping with the page
 * allocator (which starts at _heap_start after BSS). */
static char sbrk_heap[2 * 1024 * 1024];
static char *sbrk_cur = sbrk_heap;

void *sbrk(int incr) {
    if (incr == 0) return sbrk_cur;
    char *old = sbrk_cur;
    if (sbrk_cur + incr > sbrk_heap + sizeof(sbrk_heap))
        return (void *)-1;
    sbrk_cur += incr;
    return old;
}

/* malloc/free — provided by std.alloc in Sysl.
 * No C stubs needed when std.alloc is compiled in. */

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

ssize_t write(int fd, const void *buf, size_t len) {
    (void)fd;
    const uint8_t *p = buf;
    for (size_t i = 0; i < len; i++) oskit_arch_x86_64__uart_putc(p[i]);
    return (ssize_t)len;
}

int putchar(int c) { oskit_arch_x86_64__uart_putc(c); return c; }
int printf(const char *fmt, ...) { (void)fmt; return 0; }
int snprintf(char *buf, size_t n, const char *fmt, ...) { (void)buf; (void)n; (void)fmt; return 0; }
int fflush(void *stream) { (void)stream; return 0; }
void exit(int status) { (void)status; for (;;) __asm__ volatile ("hlt"); }
void abort(void) { for (;;) __asm__ volatile ("hlt"); }

