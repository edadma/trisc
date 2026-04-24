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

// Each stub loads its vector index into x0 and jumps to the reporter.
el0sp_sync:   mov x0, #0;  b exception_report
el0sp_irq:    mov x0, #1;  b exception_report
el0sp_fiq:    mov x0, #2;  b exception_report
el0sp_serror: mov x0, #3;  b exception_report

// elxsp_sync — sync exception taken from EL1 (SPSel=1). Reuses the
// same save + dispatch + restore path as low64_sync so a kernel-mode
// SVC or fault routes through arch_dispatch_sync. Frame layout is
// identical, SP_EL0 is still saved even though it's meaningless at
// EL1 (the sysl layer just ignores it).
elxsp_sync:
    sub sp, sp, #0x110
    stp x0,  x1,  [sp, #0x000]
    stp x2,  x3,  [sp, #0x010]
    stp x4,  x5,  [sp, #0x020]
    stp x6,  x7,  [sp, #0x030]
    stp x8,  x9,  [sp, #0x040]
    stp x10, x11, [sp, #0x050]
    stp x12, x13, [sp, #0x060]
    stp x14, x15, [sp, #0x070]
    stp x16, x17, [sp, #0x080]
    stp x18, x19, [sp, #0x090]
    stp x20, x21, [sp, #0x0A0]
    stp x22, x23, [sp, #0x0B0]
    stp x24, x25, [sp, #0x0C0]
    stp x26, x27, [sp, #0x0D0]
    stp x28, x29, [sp, #0x0E0]
    mrs x0, sp_el0
    stp x30, x0,  [sp, #0x0F0]
    mrs x0, spsr_el1
    mrs x1, elr_el1
    stp x0,  x1,  [sp, #0x100]
    mov x0, sp
    bl  oskit_arch__handle_sync_lower
    b   arch_resume_process

// elxsp_irq — async IRQ taken while at EL1. Needed so the timer
// can preempt a running kernel thread and let the scheduler pick a
// different one. Same save/dispatch/restore as low64_irq.
elxsp_irq:
    sub sp, sp, #0x110
    stp x0,  x1,  [sp, #0x000]
    stp x2,  x3,  [sp, #0x010]
    stp x4,  x5,  [sp, #0x020]
    stp x6,  x7,  [sp, #0x030]
    stp x8,  x9,  [sp, #0x040]
    stp x10, x11, [sp, #0x050]
    stp x12, x13, [sp, #0x060]
    stp x14, x15, [sp, #0x070]
    stp x16, x17, [sp, #0x080]
    stp x18, x19, [sp, #0x090]
    stp x20, x21, [sp, #0x0A0]
    stp x22, x23, [sp, #0x0B0]
    stp x24, x25, [sp, #0x0C0]
    stp x26, x27, [sp, #0x0D0]
    stp x28, x29, [sp, #0x0E0]
    mrs x0, sp_el0
    stp x30, x0,  [sp, #0x0F0]
    mrs x0, spsr_el1
    mrs x1, elr_el1
    stp x0,  x1,  [sp, #0x100]
    mov x0, sp
    bl  oskit_arch__handle_irq
    b   arch_resume_process

elxsp_fiq:    mov x0, #6;  b exception_report
elxsp_serror: mov x0, #7;  b exception_report

// low64_sync — synchronous exception from a lower EL (EL0 user code).
// Saves a 272-byte frame on SP_EL1, calls the arch-level handler,
// and tail-jumps to arch_resume_process with the returned SSP. The
// handler's return value is the frame the scheduler picked (same
// thread = same frame, different thread = a different SSP whose
// frame matches the same layout). Frame layout:
//   [sp, #0x000] x0,  x1
//   [sp, #0x010] x2,  x3
//   ...
//   [sp, #0x0E0] x28, x29
//   [sp, #0x0F0] x30, SP_EL0
//   [sp, #0x100] SPSR_EL1, ELR_EL1
// Total: 0x110 bytes (16-byte aligned).
low64_sync:
    sub sp, sp, #0x110
    stp x0,  x1,  [sp, #0x000]
    stp x2,  x3,  [sp, #0x010]
    stp x4,  x5,  [sp, #0x020]
    stp x6,  x7,  [sp, #0x030]
    stp x8,  x9,  [sp, #0x040]
    stp x10, x11, [sp, #0x050]
    stp x12, x13, [sp, #0x060]
    stp x14, x15, [sp, #0x070]
    stp x16, x17, [sp, #0x080]
    stp x18, x19, [sp, #0x090]
    stp x20, x21, [sp, #0x0A0]
    stp x22, x23, [sp, #0x0B0]
    stp x24, x25, [sp, #0x0C0]
    stp x26, x27, [sp, #0x0D0]
    stp x28, x29, [sp, #0x0E0]
    mrs x0, sp_el0
    stp x30, x0,  [sp, #0x0F0]
    mrs x0, spsr_el1
    mrs x1, elr_el1
    stp x0,  x1,  [sp, #0x100]

    mov x0, sp
    bl  oskit_arch__handle_sync_lower
    b   arch_resume_process

// low64_irq — asynchronous IRQ from EL0. Same save path as above;
// dispatches to the arch-level IRQ handler and resumes via the
// scheduler's chosen frame.
low64_irq:
    sub sp, sp, #0x110
    stp x0,  x1,  [sp, #0x000]
    stp x2,  x3,  [sp, #0x010]
    stp x4,  x5,  [sp, #0x020]
    stp x6,  x7,  [sp, #0x030]
    stp x8,  x9,  [sp, #0x040]
    stp x10, x11, [sp, #0x050]
    stp x12, x13, [sp, #0x060]
    stp x14, x15, [sp, #0x070]
    stp x16, x17, [sp, #0x080]
    stp x18, x19, [sp, #0x090]
    stp x20, x21, [sp, #0x0A0]
    stp x22, x23, [sp, #0x0B0]
    stp x24, x25, [sp, #0x0C0]
    stp x26, x27, [sp, #0x0D0]
    stp x28, x29, [sp, #0x0E0]
    mrs x0, sp_el0
    stp x30, x0,  [sp, #0x0F0]
    mrs x0, spsr_el1
    mrs x1, elr_el1
    stp x0,  x1,  [sp, #0x100]

    mov x0, sp
    bl  oskit_arch__handle_irq
    b   arch_resume_process
low64_fiq:    mov x0, #10; b exception_report
low64_serror: mov x0, #11; b exception_report

low32_sync:   mov x0, #12; b exception_report
low32_irq:    mov x0, #13; b exception_report
low32_fiq:    mov x0, #14; b exception_report
low32_serror: mov x0, #15; b exception_report

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
