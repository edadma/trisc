/* sysl bare-metal RV entry point.
 *
 * OpenSBI on the qemu virt machine hands control to us with:
 *   - a0 = hartid (which hart we're booting on)
 *   - a1 = pointer to FDT (device tree blob)
 *   - mstatus / supervisor-mode setup already done; we run in S-mode.
 *
 * We don't use either argument. Procedure:
 *   1. Park every hart except hartid 0 in a wfi loop. The qemu virt default
 *      is one hart, but `-smp N` would otherwise have all of them stomp on
 *      the same stack.
 *   2. Set sp to __stack_top (top of RAM, grows down).
 *   3. Zero BSS (__bss_start .. __bss_end).
 *   4. Call main().
 *   5. Pass main's return value to _sysl_rv_exit() — which calls SBI
 *      SystemReset.SHUTDOWN. If that ever returns (it shouldn't), spin in wfi.
 *
 * Same source compiles for rv32 and rv64 — we use ld/sd-aware macros via
 * the .ifdef trick on __riscv_xlen (defined by the toolchain).
 */

#if __riscv_xlen == 64
# define LREG ld
# define SREG sd
# define REGSZ 8
#else
# define LREG lw
# define SREG sw
# define REGSZ 4
#endif

    .section .text._start, "ax", @progbits
    .globl _start
_start:
    /* OpenSBI hands us off in S-mode with a0=hartid, a1=fdt. mhartid is
     * M-mode-only and would trap if we read it. Park every hart except 0. */
    bnez    a0, .Lpark

    /* Set up the stack. */
    la      sp, __stack_top

    /* Enable the FPU. OpenSBI hands off with sstatus.FS = Off, so the first
     * f-register access would trap as Illegal Instruction. Setting FS to
     * Initial (binary 01, mask 0x2000) makes F/D legal. We never touch the
     * fcsr otherwise — default rounding mode (RNE) is fine. */
    li      t0, 0x2000
    csrs    sstatus, t0

    /* Zero BSS. */
    la      t0, __bss_start
    la      t1, __bss_end
.Lbss_loop:
    bgeu    t0, t1, .Lbss_done
    SREG    zero, 0(t0)
    addi    t0, t0, REGSZ
    j       .Lbss_loop
.Lbss_done:

    /* Hand off to C. main() returns i32 in a0; pass it to _sysl_rv_exit. */
    call    main
    call    _sysl_rv_exit

.Lpark:
    wfi
    j       .Lpark
