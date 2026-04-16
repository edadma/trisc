---
title: Exceptions & Interrupts
description: The TRISC exception model, vector table, and interrupt handling.
---

## Vector table

TRISC uses a 68000-style vector table at address 0x00. The table has 20 slots, each 8 bytes (one 64-bit address), totaling 160 bytes:

| Slot | Address | Exception |
|------|---------|-----------|
| 0 | 0x00 | Initial SSP (stack pointer value, not a handler) |
| 1 | 0x08 | Initial PC (reset entry point) |
| 2 | 0x10 | Interrupt (external, from interrupt controller) |
| 3 | 0x18 | InstructionAccess (bad fetch address) |
| 4 | 0x20 | DataAccess (bad load/store, page fault) |
| 5 | 0x28 | MisalignedAccess |
| 6 | 0x30 | UnimplementedOpcode |
| 7 | 0x38 | PrivilegeViolation |
| 8 | 0x40 | IllegalDivide (division by zero) |
| 9-16 | 0x48-0x80 | Trap 0 - Trap 7 (software traps / syscalls) |
| 17 | 0x88 | Trace (single-step) |
| 18 | 0x90 | Overflow (`trapv` when V flag set) |
| 19 | 0x98 | BoundsCheck (`chk` instruction) |

## Exception entry

When an exception occurs, the CPU:

1. **Switches to supervisor mode** — sets PSR.Mode = 1
2. **Swaps stack pointers** — r7 becomes SSP, old r7 saved as USP
3. **Saves state** — pushes PSR and PC onto the supervisor stack
4. **Disables interrupts** — sets PSR.Ind = 1
5. **Clears trace** — sets PSR.T = 0 (prevents trace loops)
6. **Clears reservations** — invalidates any LL/SC reservation
7. **Loads handler** — reads the handler address from the vector table and jumps to it

The exception handler runs with interrupts disabled in supervisor mode. It can re-enable interrupts with `sti` if needed.

## Returning from exceptions

The `rte` instruction reverses the exception entry:

1. Pops PC from supervisor stack
2. Pops PSR from supervisor stack (restoring mode, interrupt state, flags)
3. If returning to user mode, swaps r7 back to USP

## External interrupts

External interrupts come from the [interrupt controller](/devices/intc/). The INTC aggregates IRQ lines from all devices and signals the CPU when an enabled interrupt is pending.

The CPU checks for pending interrupts **between instructions** (not mid-instruction). An interrupt is taken only if:

- PSR.Ind = 0 (interrupts enabled)
- The CPU is in Run or Wfi state

When in **Wfi** (wait for interrupt) state, the CPU idles but continues running device tick callbacks, so it can wake immediately when an interrupt arrives.

## Double fault

If an exception occurs during exception entry (e.g., the supervisor stack pointer is invalid), the CPU enters a **DoubleFault** state and halts permanently. This is unrecoverable — it indicates a fundamental kernel bug.

## Software traps

The `trap0` through `trap7` instructions trigger software exceptions, used as syscall entry points. SLIX uses `trap0` for all system calls, with the syscall number in r1.

## Trace mode

Setting PSR.T = 1 enables single-step mode. After every instruction, a Trace exception fires (vector slot 17). This is used for debugger integration. The CPU clears T on exception entry to prevent infinite trace loops.
