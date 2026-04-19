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
