/* ============================================================================
 * SLIX x86_64 Runtime — UART, PIC, PIT, IDT, libc stubs
 *
 * This is the C shim between the x86 hardware and the Sysl kernel.
 * It sets up the minimum hardware state needed for the kernel to run.
 * ============================================================================ */

#define COM1 0x3F8

typedef unsigned long  uint64_t;
typedef unsigned int   uint32_t;
typedef unsigned short uint16_t;
typedef unsigned char  uint8_t;
typedef long           int64_t;
typedef unsigned long  size_t;
typedef long           ssize_t;

/* ============================================================================
 * Port I/O
 * ============================================================================ */

static inline void outb(uint16_t port, uint8_t val) {
    __asm__ volatile ("outb %0, %1" : : "a"(val), "Nd"(port));
}

static inline uint8_t inb(uint16_t port) {
    uint8_t val;
    __asm__ volatile ("inb %1, %0" : "=a"(val) : "Nd"(port));
    return val;
}

static inline void io_wait(void) {
    outb(0x80, 0);  /* write to unused port for delay */
}

/* ============================================================================
 * UART (COM1)
 * ============================================================================ */

static void uart_init(void) {
    outb(COM1 + 1, 0x00);   /* Disable interrupts */
    outb(COM1 + 3, 0x80);   /* DLAB on */
    outb(COM1 + 0, 0x01);   /* Divisor low: 115200 baud */
    outb(COM1 + 1, 0x00);   /* Divisor high */
    outb(COM1 + 3, 0x03);   /* 8N1, DLAB off */
    outb(COM1 + 2, 0x00);   /* Disable FIFO */
    outb(COM1 + 4, 0x00);   /* No modem control */
}

void uart_putc(int c) {
    while (!(inb(COM1 + 5) & 0x20))
        ;
    outb(COM1, (uint8_t)c);
}

static void uart_puts(const char *s) {
    while (*s)
        uart_putc(*s++);
}

static void uart_puthex(uint64_t val) {
    const char *hex = "0123456789abcdef";
    uart_puts("0x");
    for (int i = 60; i >= 0; i -= 4)
        uart_putc(hex[(val >> i) & 0xf]);
}

/* ============================================================================
 * PIC — 8259 Programmable Interrupt Controller
 *
 * Remap IRQs 0-7 to vectors 32-39, IRQs 8-15 to vectors 40-47.
 * ============================================================================ */

#define PIC1_CMD  0x20
#define PIC1_DATA 0x21
#define PIC2_CMD  0xA0
#define PIC2_DATA 0xA1

static void pic_init(void) {
    /* ICW1: start init, expect ICW4 */
    outb(PIC1_CMD, 0x11);
    io_wait();
    outb(PIC2_CMD, 0x11);
    io_wait();

    /* ICW2: vector offsets */
    outb(PIC1_DATA, 0x20);   /* IRQ 0-7  -> vectors 32-39 */
    io_wait();
    outb(PIC2_DATA, 0x28);   /* IRQ 8-15 -> vectors 40-47 */
    io_wait();

    /* ICW3: cascading */
    outb(PIC1_DATA, 0x04);   /* IRQ2 has slave */
    io_wait();
    outb(PIC2_DATA, 0x02);   /* Slave ID 2 */
    io_wait();

    /* ICW4: 8086 mode */
    outb(PIC1_DATA, 0x01);
    io_wait();
    outb(PIC2_DATA, 0x01);
    io_wait();

    /* Mask all IRQs except IRQ0 (timer) */
    outb(PIC1_DATA, 0xFE);   /* bit 0 clear = IRQ0 enabled */
    outb(PIC2_DATA, 0xFF);   /* all masked */
}

/* ============================================================================
 * PIT — Programmable Interval Timer
 *
 * Channel 0, rate generator mode, ~100 Hz (10ms ticks).
 * ============================================================================ */

#define PIT_CH0 0x40
#define PIT_CMD 0x43

static void pit_init(void) {
    uint16_t div = 11932;    /* 1193182 / 100 Hz */

    outb(PIT_CMD, 0x36);     /* Channel 0, lo/hi, rate generator */
    outb(PIT_CH0, (uint8_t)(div & 0xFF));
    outb(PIT_CH0, (uint8_t)(div >> 8));
}

/* ============================================================================
 * IDT — Interrupt Descriptor Table
 *
 * 256 entries, each 16 bytes (long mode IDT gate).
 * ============================================================================ */

/* Defined in boot.s */
extern void timer_isr_entry(void);
extern void syscall_entry(void);
extern void exc_divide_error(void);
extern void exc_double_fault(void);
extern void exc_gpf(void);
extern void exc_page_fault(void);
extern void exc_generic(void);

/* IDT entry (16 bytes in long mode) */
struct idt_entry {
    uint16_t offset_lo;
    uint16_t selector;
    uint8_t  ist;          /* IST index (0 = no IST) */
    uint8_t  type_attr;    /* P(1) DPL(2) 0 Type(4) */
    uint16_t offset_mid;
    uint32_t offset_hi;
    uint32_t reserved;
} __attribute__((packed));

struct idt_ptr {
    uint16_t limit;
    uint64_t base;
} __attribute__((packed));

static struct idt_entry idt_entries[256];

static void idt_set_gate(int n, uint64_t handler, uint8_t type_attr) {
    idt_entries[n].offset_lo  = (uint16_t)(handler & 0xFFFF);
    idt_entries[n].selector   = 0x08;   /* kernel code segment */
    idt_entries[n].ist        = 0;
    idt_entries[n].type_attr  = type_attr;
    idt_entries[n].offset_mid = (uint16_t)((handler >> 16) & 0xFFFF);
    idt_entries[n].offset_hi  = (uint32_t)((handler >> 32) & 0xFFFFFFFF);
    idt_entries[n].reserved   = 0;
}

static void idt_init(void) {
    /* Fill all 256 entries with generic handler first */
    for (int i = 0; i < 256; i++) {
        idt_set_gate(i, (uint64_t)exc_generic, 0x8E);  /* P=1, DPL=0, interrupt gate */
    }

    /* CPU exceptions */
    idt_set_gate(0,  (uint64_t)exc_divide_error, 0x8E);
    idt_set_gate(8,  (uint64_t)exc_double_fault, 0x8E);
    idt_set_gate(13, (uint64_t)exc_gpf,          0x8E);
    idt_set_gate(14, (uint64_t)exc_page_fault,   0x8E);

    /* Hardware IRQs (remapped by PIC) */
    idt_set_gate(32, (uint64_t)timer_isr_entry,  0x8E);  /* IRQ0 = timer */

    /* Syscall entry — DPL=3 so user-mode can int 0x80 */
    idt_set_gate(0x80, (uint64_t)syscall_entry,  0xEE);  /* P=1, DPL=3, interrupt gate */

    /* Load IDT */
    struct idt_ptr idtp;
    idtp.limit = sizeof(idt_entries) - 1;
    idtp.base  = (uint64_t)&idt_entries;
    __asm__ volatile ("lidt %0" : : "m"(idtp));
}

/* ============================================================================
 * Exception handler (called from assembly stubs)
 * ============================================================================ */

void exception_handler(uint64_t exc_num, uint64_t error_code, uint64_t addr) {
    uart_puts("\n!!! EXCEPTION ");
    uart_puthex(exc_num);
    uart_puts(" error=");
    uart_puthex(error_code);
    if (exc_num == 14) {
        uart_puts(" addr=");
        uart_puthex(addr);
    }
    uart_puts(" !!!\n");
    /* Halt */
    for (;;) __asm__ volatile ("hlt");
}

/* ============================================================================
 * Syscall dispatch (called from assembly)
 *
 * Returns 0 for fast-path (just iretq), 1 for reschedule needed.
 * For now, all syscalls are stubbed — just putc works.
 * ============================================================================ */

int64_t syscall_dispatch(int64_t num, int64_t arg1, int64_t arg2, void *ctx) {
    (void)arg2;
    (void)ctx;

    switch (num) {
    case 1:  /* putc */
        uart_putc((int)arg1);
        return 0;  /* fast path */
    case 5:  /* thread_id — stub */
        return 0;
    default:
        uart_puts("SYSCALL ");
        uart_puthex(num);
        uart_puts(" (unhandled)\n");
        return 0;
    }
}

/* ============================================================================
 * Kernel stubs — real kernel provides these; stubbed for test_kernel
 * ============================================================================ */

int current_thread = -1;

int64_t schedule(int64_t current_ssp) {
    (void)current_ssp;
    return 0;
}

/* ============================================================================
 * libc stubs — needed by LLVM-generated code
 * ============================================================================ */

static char heap[1024 * 1024];  /* 1 MB */
static size_t heap_offset = 0;

void *malloc(size_t size) {
    heap_offset = (heap_offset + 15) & ~(size_t)15;
    if (heap_offset + size > sizeof(heap))
        return (void *)0;
    void *p = &heap[heap_offset];
    heap_offset += size;
    return p;
}

void free(void *p) { (void)p; }

void *memcpy(void *dst, const void *src, size_t n) {
    uint8_t *d = dst;
    const uint8_t *s = src;
    for (size_t i = 0; i < n; i++) d[i] = s[i];
    return dst;
}

void *memset(void *dst, int c, size_t n) {
    uint8_t *d = dst;
    for (size_t i = 0; i < n; i++) d[i] = (uint8_t)c;
    return dst;
}

int fflush(void *stream) { (void)stream; return 0; }
void exit(int status) { (void)status; for (;;) __asm__ volatile ("hlt"); }
int putchar(int c) { uart_putc(c); return c; }
int printf(const char *fmt, ...) { (void)fmt; return 0; }
int snprintf(char *buf, size_t n, const char *fmt, ...) { (void)buf; (void)n; (void)fmt; return 0; }
size_t strlen(const char *s) { size_t n = 0; while (s[n]) n++; return n; }
int memcmp(const void *a, const void *b, size_t n) {
    const uint8_t *pa = a, *pb = b;
    for (size_t i = 0; i < n; i++) { if (pa[i] != pb[i]) return pa[i] - pb[i]; }
    return 0;
}

ssize_t write(int fd, const void *buf, size_t len) {
    (void)fd;
    const uint8_t *p = buf;
    for (size_t i = 0; i < len; i++) uart_putc(p[i]);
    return (ssize_t)len;
}

void abort(void) {
    uart_puts("ABORT\n");
    for (;;) __asm__ volatile ("hlt");
}

/* ============================================================================
 * runtime_init — called from boot.s before kernel entry
 * ============================================================================ */

void runtime_init(void) {
    uart_init();
    uart_puts("SLIX x86_64: UART ok\n");

    pic_init();
    uart_puts("SLIX x86_64: PIC ok\n");

    pit_init();
    uart_puts("SLIX x86_64: PIT ok\n");

    idt_init();
    uart_puts("SLIX x86_64: IDT ok\n");

    uart_puts("SLIX x86_64: runtime ready\n");
}
