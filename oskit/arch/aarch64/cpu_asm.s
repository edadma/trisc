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

// drop_to_el0(entry: i64, sp: i64)
//   Transition from EL1 to EL0 and start executing at `entry` with
//   SP_EL0 = `sp`. Does not return. The caller is responsible for
//   having a page table active that maps the entry point and stack
//   EL0-accessible. SPSR_EL1 = 0 means target mode EL0t (stack is
//   SP_EL0), DAIF unmasked; an exception from EL0 back to EL1 is
//   the normal way out.
.global drop_to_el0
drop_to_el0:
    msr sp_el0, x1
    msr elr_el1, x0
    mov x2, #0
    msr spsr_el1, x2
    isb
    eret

// user_svc_test — tiny EL0 entry used to sanity-check the EL1->EL0
// transition. Issues `svc #0x42` so the exception reporter in
// vectors.s fires `low64_sync` (V=8) with ESR_EL1 carrying EC=0x15
// (SVC64) and the immediate 0x42 in ISS[15:0]. Spins if SVC ever
// returns, which it currently never will (exception_report halts).
.global user_svc_test
user_svc_test:
    svc #0x42
1:  b    1b
