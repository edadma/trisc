---
title: Registers
description: The TRISC register file and processor status register.
---

## General-purpose registers

TRISC has 8 general-purpose 64-bit registers:

| Register | ABI name | Purpose | Saved by |
|----------|----------|---------|----------|
| r0 | zero | Hardwired to zero (writes ignored) | -- |
| r1 | a1/rv | First argument, return value | Caller |
| r2 | t1 | Temporary / scratch | Caller |
| r3 | t2 | Temporary / scratch | Caller |
| r4 | t3 | Call target / temporary | Caller |
| r5 | fp | Frame pointer | Callee |
| r6 | lr | Link register (return address) | Callee |
| r7 | sp | Stack pointer (active) | Callee |

**r0** always reads as zero. Writing to r0 is silently ignored. This eliminates special cases — clearing a register is `mov r1, r0`, comparing against zero is `beq r1, r0, label`.

**r7** is the active stack pointer. In user mode it holds the user stack pointer (USP); in supervisor mode it holds the supervisor stack pointer (SSP). The inactive pointer is saved in a hidden register and swapped automatically on exception entry/exit.

## Processor Status Register (PSR)

The PSR is a 5-bit register controlling processor state:

| Bit | Name | Description |
|-----|------|-------------|
| 0 | Ind | Interrupt disable (1 = interrupts masked) |
| 1 | Mode | Privilege mode (0 = user, 1 = supervisor) |
| 2 | C | Carry / borrow flag |
| 4 | T | Trace (single-step) flag |
| 5 | V | Signed overflow flag |

### Accessing the PSR

| Instruction | Operation |
|-------------|-----------|
| `gpsr rd` | Read PSR into rd |
| `spsr rs` | Write rs to PSR (supervisor only) |

### Flag behavior

- **C** (Carry) — set by `add`, `sub`, `adc`, `sbc` for unsigned overflow/borrow
- **V** (Overflow) — set by `add`, `sub` for signed overflow; `trapv` traps if V is set
- **T** (Trace) — when set, a Trace exception fires after every instruction (single-step debugging)

## Special registers

| Register | Access | Description |
|----------|--------|-------------|
| USP | `gusp rd` / `susp rs` | User stack pointer (supervisor only) |
| PTBR | `gptbr rd` / `sptbr rs` | Page table base register (MMU) |
| ASID | `gasid rd` / `sasid rs` | Address space identifier (MMU TLB) |
| FAULTADDR | `gfault rd` | Faulting virtual address (last MMU fault) |
| FAULTCAUSE | `gfcause rd` | Cause of last MMU fault |
