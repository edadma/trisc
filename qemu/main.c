/* x86_64 bare-metal hello — serial output via COM1 (0x3F8) */

#define COM1 0x3F8

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

static void uart_putc(char c) {
    while (!(inb(COM1 + 5) & 0x20))
        ;
    outb(COM1, c);
}

static void uart_puts(const char *s) {
    while (*s)
        uart_putc(*s++);
}

void main(void) {
    uart_init();
    uart_puts("SLIX x86_64 — hello world\n");
    outb(0xf4, 0x00);
}
