#define UART0    ((volatile unsigned int *)0x09000000)
#define UART0_FR ((volatile unsigned int *)0x09000018)

static void uart_putc(char c) {
    *UART0 = c;
}

static void uart_puts(const char *s) {
    while (*s)
        uart_putc(*s++);
}

static char uart_getc(void) {
    while (*UART0_FR & (1 << 4))
        ;
    return *UART0 & 0xFF;
}

void main(void) {
    uart_puts("hello world\n> ");

    while (1) {
        char c = uart_getc();

        if (c == '\r') {
            uart_puts("\n> ");
        } else {
            uart_putc(c);
        }
    }
}
