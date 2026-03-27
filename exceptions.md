Exception Vectors
=================

Each vector entry is 8 bytes (one 64-bit value). Reset occupies slots 0-1 (initial SSP and PC).
Other exception handlers start at slot 2.

Slot | Address | Purpose
---- | ------- | -------
0    | 0x00    | Initial SSP (reset only)
1    | 0x08    | Initial PC (reset only)
2    | 0x10    | Interrupt
3    | 0x18    | Instruction Access (fetch from unmapped address)
4    | 0x20    | Data Access (load/store to unmapped address)
5    | 0x28    | Misaligned Access
6    | 0x30    | Unimplemented Opcode (illegal instruction)
7    | 0x38    | Privilege Violation (privileged instruction in user mode)
8    | 0x40    | Illegal Integer Divide (div/rem by zero)
9    | 0x48    | Trap 0 (supervisor call)
10   | 0x50    | Trap 1
11   | 0x58    | Trap 2
12   | 0x60    | Trap 3
13   | 0x68    | Trap 4
14   | 0x70    | Trap 5
15   | 0x78    | Trap 6
16   | 0x80    | Trap 7
17   | 0x88    | Trace (single-step)
18   | 0x90    | Overflow (trapv with V flag set)
19   | 0x98    | Bounds Check (chk out of range)

Total: 20 slots × 8 bytes = 160 bytes (0x00–0x9F)

Reset
-----

Reset is handled specially (68000-style):
1. SSP loaded from vector slot 0
2. PC loaded from vector slot 1
3. Mode and Ind set, C and T cleared
4. No stack frame is pushed

Exception Entry (all except Reset)
-----------------------------------

1. If already in exception entry → DoubleFault (CPU halts)
2. If coming from user mode: swap r7 ↔ usp (switch to supervisor stack)
3. Push PSR onto supervisor stack (8 bytes, r7 -= 8)
4. Push PC onto supervisor stack (8 bytes, r7 -= 8)
5. Load PC from vector table: slot = (exception ordinal + 1)
6. Set Mode and Ind in PSR (supervisor mode, interrupts disabled)
7. Clear T in PSR (prevents infinite trace loops)
8. Invalidate LL/SC reservation

Stack frame layout (16 bytes):
    r7 → [ saved PC  ] (8 bytes)
         [ saved PSR ] (8 bytes)

RTE (Return from Exception)
----------------------------

1. Pop PC from supervisor stack (r7 += 8)
2. Pop PSR from supervisor stack (r7 += 8)
3. If restored PSR has user mode (Mode=0): swap r7 ↔ usp (switch to user stack)

Privileged Instructions
-----------------------

These trigger Privilege Violation if executed in user mode (Mode=0):
- spsr (set PSR)
- rte (return from exception)
- halt
- wfi (wait for interrupt)
- gusp (get user stack pointer)
- susp (set user stack pointer)

Dual Stack Pointers (68000-style)
---------------------------------

- r7 is the active stack pointer in both modes
- usp is a hidden register holding the inactive stack pointer
- In supervisor mode: r7 = SSP, usp = USP
- In user mode: r7 = USP, usp = SSP
- On exception entry from user mode: r7 and usp are swapped
- On RTE to user mode: r7 and usp are swapped back
- gusp/susp allow the supervisor to read/write the user stack pointer

PSR (Processor Status Register)
-------------------------------

Bit | Name | Description
--- | ---- | -----------
0   | Ind  | Interrupt Disable (1 = interrupts masked)
1   | Mode | Privilege Mode (0 = user, 1 = supervisor)
2   | C    | Carry/Borrow flag (set by add, sub, adc, sbc)
3   | Irq  | Interrupt pending
4   | T    | Trace (fires Trace exception after each instruction)
5   | V    | Overflow (signed overflow from add, sub, adc, sbc, neg)

Trace Mode
----------

When T is set in PSR, a Trace exception fires after every instruction.
This enables single-step debugging (68k-style semantics).

Key details:
- Trace fires based on T state BEFORE the instruction executes
- spsr that sets T does NOT itself trigger trace — the next instruction does
- rte in the handler does NOT trigger trace (T was clear before rte) even
  though it restores T — the next instruction at the return point does
- Exception entry automatically clears T to prevent infinite trace loops
- The saved PSR retains T=1, so rte naturally continues single-stepping

Overflow Flag
-------------

V is set when a signed arithmetic operation overflows:
- add/adc: both operands have same sign but result has different sign
- sub/sbc: operands have different signs and result differs from first operand
- neg: only overflows when negating the minimum value (Long.MinValue)

The trapv instruction traps to the Overflow vector if V is set.
The chk instruction traps to the BoundsCheck vector if the value is
out of range (ra < 0 or ra > rb, signed comparison).

Double Fault
------------

If an exception occurs during exception entry (e.g., supervisor stack points
to unmapped memory), the CPU enters DoubleFault state and halts. This is not
recoverable — it indicates a kernel bug (invalid supervisor stack pointer).
