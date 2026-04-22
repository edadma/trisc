// aarch64 exception vector table + crash reporter (QEMU virt).
//
// Layout required by VBAR_ELx: 16 vectors, each 128 bytes, table
// 2KB-aligned. Groups by exception source × 4 exception kinds:
//
//     +0x000  Current EL with SP_EL0   sync / irq / fiq / serror
//     +0x200  Current EL with SP_ELx   sync / irq / fiq / serror   (kernel)
//     +0x400  Lower EL, AArch64        sync / irq / fiq / serror   (user)
//     +0x600  Lower EL, AArch32        sync / irq / fiq / serror
//
// Until the kernel has real handlers, every vector funnels into
// `exception_report`, which writes an identifying message to the
// PL011 UART and halts. The identifier is the vector index 0..15
// encoded as a single hex digit so you can read it from the log.

.section .text.vectors

.macro VECTOR label
    b \label
    .balign 0x80, 0
.endm

.align 11                  // VBAR_ELx must be 2KB (2^11) aligned
.global vector_table
vector_table:
    VECTOR el0sp_sync       // 0x000
    VECTOR el0sp_irq        // 0x080
    VECTOR el0sp_fiq        // 0x100
    VECTOR el0sp_serror     // 0x180

    VECTOR elxsp_sync       // 0x200
    VECTOR elxsp_irq        // 0x280
    VECTOR elxsp_fiq        // 0x300
    VECTOR elxsp_serror     // 0x380

    VECTOR low64_sync       // 0x400
    VECTOR low64_irq        // 0x480
    VECTOR low64_fiq        // 0x500
    VECTOR low64_serror     // 0x580

    VECTOR low32_sync       // 0x600
    VECTOR low32_irq        // 0x680
    VECTOR low32_fiq        // 0x700
    VECTOR low32_serror     // 0x780

// Each stub loads its vector index into x0 and jumps to the reporter,
// except `low64_sync` which handles SVC from EL0 and only routes to
// the crash reporter for non-SVC synchronous exceptions.
el0sp_sync:   mov x0, #0;  b exception_report
el0sp_irq:    mov x0, #1;  b exception_report
el0sp_fiq:    mov x0, #2;  b exception_report
el0sp_serror: mov x0, #3;  b exception_report

elxsp_sync:   mov x0, #4;  b exception_report
elxsp_irq:    mov x0, #5;  b exception_report
elxsp_fiq:    mov x0, #6;  b exception_report
elxsp_serror: mov x0, #7;  b exception_report

// low64_sync is implemented as a real dispatcher further down.
low64_irq:    mov x0, #9;  b exception_report
low64_fiq:    mov x0, #10; b exception_report
low64_serror: mov x0, #11; b exception_report

low32_sync:   mov x0, #12; b exception_report
low32_irq:    mov x0, #13; b exception_report
low32_fiq:    mov x0, #14; b exception_report
low32_serror: mov x0, #15; b exception_report

// ============================================================================
// low64_sync — EL0 synchronous exception entry (AArch64)
// ============================================================================
//
// Saves full context onto SP_EL1 (272 bytes, layout matches
// oskit/arch/aarch64/cpu.lsysl: x0..x30, SP_EL0, ELR_EL1, SPSR_EL1),
// decodes ESR_EL1.EC, and either dispatches the SVC or falls through
// to the crash reporter for other synchronous faults (data abort,
// instruction abort, …).
//
// At trap entry: SP_EL1 is the current per-CPU kernel stack (the
// kernel leaves SP_EL1 at the thread's kstack_top after the previous
// eret popped the context frame). After stp-ing the full frame the
// saved frame base equals the new SP, and `syscall_return()` in
// kernel.lsysl writes into the x0 slot at that base.
//
// The full syscall_table dispatch (bounds/privilege/handler) is
// deferred to the next commit — this pass just proves the save/decode
// /restore/eret mechanics without depending on the kernel build.
//
//   Save order:
//     stp x0,x1   [sp,#  0]    x8  is the syscall number (musl / ARM64 ABI)
//     stp x2,x3   [sp,# 16]    x0..x5 carry the syscall args
//     stp x4,x5   [sp,# 32]
//     stp x6,x7   [sp,# 48]
//     stp x8,x9   [sp,# 64]
//     stp x10-29  [sp,# 80..#232]
//     stp x30,SP_EL0 [sp,#240]
//     stp ELR,SPSR   [sp,#256]
// ============================================================================

.global low64_sync
low64_sync:
    sub sp, sp, #272
    stp x0,  x1,  [sp, #0]
    stp x2,  x3,  [sp, #16]
    stp x4,  x5,  [sp, #32]
    stp x6,  x7,  [sp, #48]
    stp x8,  x9,  [sp, #64]
    stp x10, x11, [sp, #80]
    stp x12, x13, [sp, #96]
    stp x14, x15, [sp, #112]
    stp x16, x17, [sp, #128]
    stp x18, x19, [sp, #144]
    stp x20, x21, [sp, #160]
    stp x22, x23, [sp, #176]
    stp x24, x25, [sp, #192]
    stp x26, x27, [sp, #208]
    stp x28, x29, [sp, #224]
    mrs x9,  sp_el0
    stp x30, x9,  [sp, #240]
    mrs x9,  elr_el1
    mrs x10, spsr_el1
    stp x9,  x10, [sp, #256]

    // Decode ESR_EL1.EC (bits [31:26]); 0x15 = SVC from AArch64 EL0.
    mrs x9, esr_el1
    lsr x9, x9, #26
    cmp x9, #0x15
    b.ne .low64_sync_fault

    // SVC path — placeholder. Prints 'Y' to UART so round-trip is
    // visible, then falls through to the restore path. A follow-up
    // commit will replace this with bounds/privilege check and a
    // syscall_table lookup.
    mov w0, #'Y'
    bl  uart_putc_asm

.low64_sync_restore:
    // Restore ELR_EL1 / SPSR_EL1 first (uses x9/x10 as scratch — their
    // saved values come back when we restore x8-x11 below).
    ldp x9,  x10, [sp, #256]
    msr elr_el1, x9
    msr spsr_el1, x10

    // Restore x30 and SP_EL0.
    ldp x30, x9,  [sp, #240]
    msr sp_el0, x9

    // Restore x0..x29.
    ldp x0,  x1,  [sp, #0]
    ldp x2,  x3,  [sp, #16]
    ldp x4,  x5,  [sp, #32]
    ldp x6,  x7,  [sp, #48]
    ldp x8,  x9,  [sp, #64]
    ldp x10, x11, [sp, #80]
    ldp x12, x13, [sp, #96]
    ldp x14, x15, [sp, #112]
    ldp x16, x17, [sp, #128]
    ldp x18, x19, [sp, #144]
    ldp x20, x21, [sp, #160]
    ldp x22, x23, [sp, #176]
    ldp x24, x25, [sp, #192]
    ldp x26, x27, [sp, #208]
    ldp x28, x29, [sp, #224]
    add sp, sp, #272
    eret

.low64_sync_fault:
    // Not an SVC — drop the saved frame and fall through to the crash
    // reporter so ESR/FAR get printed just like the old stub did.
    add sp, sp, #272
    mov x0, #8
    b   exception_report

// exception_report(vec_id)
//   x0: vector index 0..15
// Reads ESR_EL1 and FAR_EL1, writes "EXC V=<hex> ESR=<16hex> FAR=<16hex>\n"
// to the PL011 UART, then halts.
//
// Printing is destructive to x1..x4 and does not preserve state. This
// is a crash reporter — we don't return. Spin on UART TXFF so early
// panics don't drop characters.

.section .text

.equ UART0_DR, 0x09000000
.equ UART0_FR, 0x09000018
.equ UARTFR_TXFF, (1 << 5)

.global exception_report
exception_report:
    mov x19, x0                  // save vector id
    mrs x20, esr_el1
    mrs x21, far_el1

    adr x22, msg_exc
    bl  uart_puts_asm

    mov x0, x19
    bl  uart_hex1

    adr x22, msg_esr
    bl  uart_puts_asm
    mov x0, x20
    bl  uart_hex16

    adr x22, msg_far
    bl  uart_puts_asm
    mov x0, x21
    bl  uart_hex16

    mov w0, #'\r'
    bl  uart_putc_asm
    mov w0, #'\n'
    bl  uart_putc_asm

1:  wfi
    b   1b

// uart_putc_asm(w0=byte) — clobbers x1, w2
uart_putc_asm:
    ldr x1, =UART0_FR
1:  ldr w2, [x1]
    tst w2, #UARTFR_TXFF
    b.ne 1b
    ldr x1, =UART0_DR
    str w0, [x1]
    ret

// uart_puts_asm(x22=ptr to NUL-terminated string) — clobbers x0..x3, x22
uart_puts_asm:
    mov x3, x30                  // save link (we call uart_putc_asm)
1:  ldrb w0, [x22], #1
    cbz  w0, 2f
    bl   uart_putc_asm
    b    1b
2:  mov x30, x3
    ret

// uart_hex1(w0=nibble 0..15) — print one hex digit
uart_hex1:
    and w0, w0, #0xF
    cmp w0, #10
    b.lt 1f
    add w0, w0, #('A' - 10)
    b   uart_putc_asm
1:  add w0, w0, #'0'
    b   uart_putc_asm

// uart_hex16(x0=u64) — print 16 hex digits, high nibble first
uart_hex16:
    mov x23, x30
    mov x24, x0
    mov x25, #60
1:  lsr x0, x24, x25
    bl  uart_hex1
    subs x25, x25, #4
    b.ge 1b
    mov x30, x23
    ret

.section .rodata
msg_exc: .asciz "\nEXC V="
msg_esr: .asciz " ESR="
msg_far: .asciz " FAR="
