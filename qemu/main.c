/* x86_64 bare-metal runtime shim for sysl programs.
 *
 * Provides:
 *   - boot_entry()  — called from startup.s, sets up UART and calls sysl main
 *   - uart_putc()   — called from sysl code
 *   - write/abort   — referenced by panic/assert in sysl's LLVM preamble;
 *                     stubbed here so the linker is satisfied
 */

#define COM1 0x3F8

typedef unsigned long size_t;
typedef long ssize_t;

static inline void outb(unsigned short port, unsigned char val) {
    __asm__ volatile ("outb %0, %1" : : "a"(val), "Nd"(port));
}

static inline unsigned char inb(unsigned short port) {
    unsigned char val;
    __asm__ volatile ("inb %1, %0" : "=a"(val) : "Nd"(port));
    return val;
}

static void uart_init(void) {
    outb(COM1 + 1, 0x00);
    outb(COM1 + 3, 0x80);
    outb(COM1 + 0, 0x01);
    outb(COM1 + 1, 0x00);
    outb(COM1 + 3, 0x03);
    outb(COM1 + 2, 0x00);
    outb(COM1 + 4, 0x00);
}

void uart_putc(int c) {
    while (!(inb(COM1 + 5) & 0x20))
        ;
    outb(COM1, (unsigned char)c);
}

/* --- libc stubs for sysl's LLVM preamble --- */

ssize_t write(int fd, const void *buf, size_t len) {
    const unsigned char *p = buf;
    for (size_t i = 0; i < len; i++)
        uart_putc(p[i]);
    return (ssize_t)len;
}

void abort(void) {
    for (;;) __asm__ volatile ("hlt");
}

/* --- entry point called by startup.s --- */

extern int main(void);

void boot_entry(void) {
    uart_init();
    (void)main();
    /* Exit via QEMU debug port (isa-debug-exit) */
    outb(0xf4, 0x00);
}
