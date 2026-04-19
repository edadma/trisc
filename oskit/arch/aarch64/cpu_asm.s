// aarch64 CPU primitives (arch-level, board-independent).
//
// Exposes arch_cli/arch_sti used by the kernel to mask and unmask
// asynchronous exceptions (IRQ, FIQ, SError) when holding a lock or
// entering a critical section. Bit D (debug) is left alone — we
// only toggle the three async interrupts and watch for aborts.
//
// `msr daifset, #imm` ORs `imm` into the masked bits of PSTATE.DAIF
// and `msr daifclr, #imm` clears them. #0x7 covers A (SError), I,
// and F; debug traps are controlled separately via MDSCR_EL1.

.section .text

.global arch_cli
arch_cli:
    msr daifset, #0x7
    ret

.global arch_sti
arch_sti:
    msr daifclr, #0x7
    ret
