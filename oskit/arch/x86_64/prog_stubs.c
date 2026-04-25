/* Minimal libc stubs for standalone x86_64 SLIX programs.
 * Provides sbrk (using linker symbols) and C library functions
 * needed by LLVM-generated code. */

typedef unsigned long size_t;
typedef long ssize_t;
typedef unsigned char uint8_t;

/* Heap boundaries from linker script */
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

/* putchar via SYS_PUTC syscall (number 1) */
static void prog_putc(int c) {
    __asm__ volatile (
        "movq $1, %%rdi\n"  /* SYS_PUTC */
        "movq %0, %%rsi\n"
        "int $0x80\n"
        : : "r"((long)c) : "rdi", "rsi", "rax", "memory"
    );
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
    __asm__ volatile (
        "movq $3, %%rdi\n"  /* SYS_EXIT */
        "movq %0, %%rsi\n"
        "int $0x80\n"
        : : "r"((long)status) : "rdi", "rsi"
    );
    for (;;) __asm__ volatile ("hlt");
}

void abort(void) { exit(-1); }

/* I/O port externs used by pci.lsysl. The nic server links
 * virtio_transport_pci for v_attach_pci but never calls v_find
 * (kernel does the probe at boot, server just takes the cached
 * cap addresses), so these are unreachable in practice — but the
 * linker still resolves them. Ring 3 code can't do port I/O
 * anyway, so abort loudly if something does call through.
 */
unsigned char inb(int port) { (void)port; abort(); return 0; }
void outb(int port, unsigned char v) { (void)port; (void)v; abort(); }
unsigned short inw(int port) { (void)port; abort(); return 0; }
void outw(int port, unsigned short v) { (void)port; (void)v; abort(); }
unsigned int inl(int port) { (void)port; abort(); return 0; }
void outl(int port, unsigned int v) { (void)port; (void)v; abort(); }
void io_wait(void) {}
