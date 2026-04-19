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

/* sysl's runtime emits free() calls when refcounted strings go out of
 * scope. Bare-metal has no malloc; let the strings leak. */
void free(void *p) {
    (void)p;
}

/* Bump allocator backing malloc for the bare-metal kernel. The sysl
 * string runtime calls malloc when it needs to heap-allocate a new
 * string buffer (e.g. on process name assignment). A simple bump
 * allocator is sufficient until we wire std.alloc + sbrk properly.
 *
 * QEMU virt default RAM is 128MB starting at 0x40000000, so the top
 * of RAM is 0x47FFFFFF. Reserve the top 16MB for this bump allocator;
 * the page allocator in vm.lsysl grows up from _heap_start and won't
 * reach that far for a long time. */
static unsigned long heap_cursor = 0;
#define HEAP_BUMP_BASE 0x47000000UL
#define HEAP_BUMP_END  0x48000000UL

void *malloc(size_t len) {
    if (heap_cursor == 0)
        heap_cursor = HEAP_BUMP_BASE;
    /* 8-byte align */
    len = (len + 7UL) & ~7UL;
    if (heap_cursor + len > HEAP_BUMP_END)
        return (void *)0;
    void *p = (void *)heap_cursor;
    heap_cursor += len;
    return p;
}

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
