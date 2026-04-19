/* Minimal C kernel_main for the aarch64 QEMU virt hello-world milestone.
 *
 * Writes to the PL011 UART at 0x09000000 (the standard virt machine
 * console). QEMU's firmware has already configured the UART; we just
 * poll UARTFR.TXFF and write UARTDR.
 */

#define UART0_BASE 0x09000000UL
#define UARTDR (*(volatile unsigned int *)(UART0_BASE + 0x000))
#define UARTFR (*(volatile unsigned int *)(UART0_BASE + 0x018))
#define UARTFR_TXFF (1U << 5)

static void uart_putc(char c) {
    while (UARTFR & UARTFR_TXFF)
        ;
    UARTDR = (unsigned int)(unsigned char)c;
}

static void uart_puts(const char *s) {
    while (*s) {
        if (*s == '\n')
            uart_putc('\r');
        uart_putc(*s++);
    }
}

void kernel_main(void) {
    uart_puts("SLIX aarch64 boot\n");
    uart_puts("Hello, aarch64!\n");
}
