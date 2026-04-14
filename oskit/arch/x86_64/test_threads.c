/* ============================================================================
 * test_threads.c — Prove context switching works on x86_64
 *
 * Two threads, each printing a character in a loop. The PIT timer
 * preempts them via timer_isr_entry → do_schedule → schedule().
 *
 * Expected output: interleaved 'A' and 'B' characters.
 *
 * This file provides kernel_init() and kernel_main() so it REPLACES
 * test_kernel.sysl (don't link both).
 * ============================================================================ */

typedef unsigned long  uint64_t;
typedef long           int64_t;

extern void uart_putc(int c);

/* ============================================================================
 * Mini scheduler — just enough for two threads
 * ============================================================================ */

#define MAX_THREADS 4
#define STATE_READY      0
#define STATE_RUNNING    1
#define STATE_TERMINATED 3

struct thread {
    int64_t ssp;
    int     state;
};

struct thread threads[MAX_THREADS];
int current_thread = -1;
int thread_count = 0;

/* Thread stacks — 8KB each, 16-byte aligned */
static char thread_stacks[MAX_THREADS][8192] __attribute__((aligned(16)));

/* ============================================================================
 * build_stack_frame — Create a fake interrupt frame for a new thread
 *
 * Must match what restore_context in boot.s pops:
 *   15 GP regs (R15..RAX) then iretq (RIP, CS, RFLAGS, RSP, SS)
 * ============================================================================ */

static int64_t build_stack_frame(void (*entry)(void), int stack_idx) {
    int64_t *stack_top = (int64_t *)&thread_stacks[stack_idx][8192];
    int64_t *sp = stack_top;

    /* iretq frame (CPU would push these on interrupt) */
    *--sp = 0x10;                   /* SS  (kernel data segment) */
    *--sp = (int64_t)stack_top;     /* RSP (top of this stack) */
    *--sp = 0x202;                  /* RFLAGS (IF=1, reserved bit 1) */
    *--sp = 0x08;                   /* CS  (kernel code segment) */
    *--sp = (int64_t)entry;         /* RIP */

    /* GP registers: RAX, RBX, RCX, RDX, RSI, RDI, RBP, R8..R15 */
    for (int i = 0; i < 15; i++)
        *--sp = 0;

    return (int64_t)sp;
}

/* ============================================================================
 * schedule — Simple round-robin, called from boot.s do_schedule
 *
 * Takes current RSP (saved context), returns next thread's RSP.
 * Returns 0 if no runnable threads (idle).
 * ============================================================================ */

int64_t schedule(int64_t current_ssp) {
    /* Save current thread's stack pointer */
    if (current_thread >= 0) {
        threads[current_thread].ssp = current_ssp;
        if (threads[current_thread].state == STATE_RUNNING)
            threads[current_thread].state = STATE_READY;
    }

    /* Find next runnable thread (round-robin) */
    int start = (current_thread + 1) % thread_count;
    for (int i = 0; i < thread_count; i++) {
        int idx = (start + i) % thread_count;
        if (threads[idx].state == STATE_READY) {
            current_thread = idx;
            threads[idx].state = STATE_RUNNING;
            return threads[idx].ssp;
        }
    }

    /* No runnable threads */
    current_thread = -1;
    return 0;
}

/* ============================================================================
 * Thread entry points
 * ============================================================================ */

static volatile int stop_flag = 0;

static void thread_a(void) {
    for (;;) {
        uart_putc('A');
        /* Busy wait — timer will preempt us */
        for (volatile int i = 0; i < 500000; i++)
            ;
    }
}

static void thread_b(void) {
    for (;;) {
        uart_putc('B');
        for (volatile int i = 0; i < 500000; i++)
            ;
    }
}

/* ============================================================================
 * Kernel entry points — called from boot.s
 * ============================================================================ */

void kernel_init(void) {
    uart_putc('[');
    uart_putc('i');
    uart_putc(']');
}

int64_t kernel_main(void) {
    /* Create thread A */
    threads[0].ssp = build_stack_frame(thread_a, 0);
    threads[0].state = STATE_READY;
    thread_count = 1;

    /* Create thread B */
    threads[1].ssp = build_stack_frame(thread_b, 1);
    threads[1].state = STATE_READY;
    thread_count = 2;

    /* Return first thread's SSP — boot.s will jump into it */
    current_thread = 0;
    threads[0].state = STATE_RUNNING;
    return threads[0].ssp;
}
