/* Runtime stubs for bare-metal aarch64 (QEMU virt).
 *
 * The sysl LLVM backend emits calls to `write()` and `abort()` from
 * generated panic/assert paths. For a freestanding kernel we redirect
 * `write` to the PL011 UART and `abort` to wfi-loop halt.
 *
 * Later, when the kernel's own log routes exist, these stubs can be
 * replaced by calls into the arch runtime. */

typedef unsigned long size_t;
typedef long ssize_t;
typedef unsigned int uint32_t;

#define UART0_BASE 0x09000000UL
#define UARTDR (*(volatile uint32_t *)(UART0_BASE + 0x000))
#define UARTFR (*(volatile uint32_t *)(UART0_BASE + 0x018))
#define UARTFR_TXFF (1U << 5)

static void uart_putc(int c) {
    while (UARTFR & UARTFR_TXFF)
        ;
    UARTDR = (uint32_t)(unsigned char)c;
}

ssize_t write(int fd, const void *buf, size_t len) {
    (void)fd;
    const unsigned char *p = buf;
    for (size_t i = 0; i < len; i++)
        uart_putc(p[i]);
    return (ssize_t)len;
}

__attribute__((noreturn)) void abort(void) {
    for (;;)
        __asm__ volatile("wfi");
}

/* Debug output for kernel tracing — matches the x86_64 plumbing in
 * oskit/arch/x86_64/stubs.c so cross-arch kernel code in oskit.kernel
 * can call debug_char / debug_hex8 without arch-specific imports. */
void debug_char(int c) { uart_putc(c); }
void debug_hex4(int v) {
    const char *h = "0123456789ABCDEF";
    debug_char(h[(v >> 12) & 0xF]);
    debug_char(h[(v >>  8) & 0xF]);
    debug_char(h[(v >>  4) & 0xF]);
    debug_char(h[ v        & 0xF]);
}
void debug_hex8(int v) {
    const char *h = "0123456789ABCDEF";
    debug_char(h[(v >> 28) & 0xF]);
    debug_char(h[(v >> 24) & 0xF]);
    debug_char(h[(v >> 20) & 0xF]);
    debug_char(h[(v >> 16) & 0xF]);
    debug_char(h[(v >> 12) & 0xF]);
    debug_char(h[(v >>  8) & 0xF]);
    debug_char(h[(v >>  4) & 0xF]);
    debug_char(h[ v        & 0xF]);
}

/* sbrk — extend heap for std.alloc's Sysl allocator.
 * Static BSS array keeps the heap safely inside the linked kernel
 * image and out of the page allocator's way (page_alloc starts at
 * _heap_start, past all BSS). 2MB matches x86/stubs.c. */
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

/* malloc/free — provided by std.alloc in Sysl (now compiled into the
 * kernel via board/virt/build.sh's SYSL_FILES list). No C stubs
 * needed. */

/* Byte-wise memcpy. Not called on hot paths — the kernel's own
 * memcpy from oskit/hal/memcpy.sysl is word-at-a-time and is what
 * the kernel prefers; this stub just satisfies LLVM's implicit
 * memcpy lowering for struct copies. */
void *memcpy(void *dst, const void *src, size_t n) {
    unsigned char *d = (unsigned char *)dst;
    const unsigned char *s = (const unsigned char *)src;
    for (size_t i = 0; i < n; i++)
        d[i] = s[i];
    return dst;
}

void *memset(void *dst, int c, size_t n) {
    unsigned char *d = (unsigned char *)dst;
    for (size_t i = 0; i < n; i++)
        d[i] = (unsigned char)c;
    return dst;
}
