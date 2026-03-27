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

Total: 17 slots × 8 bytes = 136 bytes (0x00–0x87)

Reset
-----

Reset is handled specially (68000-style):
1. SSP loaded from vector slot 0
2. PC loaded from vector slot 1
3. Mode and Ind set, C cleared
4. No stack frame is pushed

Exception Entry (all except Reset)
-----------------------------------

1. If already in exception entry → DoubleFault (CPU halts)
2. If coming from user mode: swap r7 ↔ usp (switch to supervisor stack)
3. Push PSR onto supervisor stack (8 bytes, r7 -= 8)
4. Push PC onto supervisor stack (8 bytes, r7 -= 8)
5. Load PC from vector table: slot = (exception ordinal + 1)
6. Set Mode and Ind in PSR (supervisor mode, interrupts disabled)
7. Invalidate LL/SC reservation

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

Double Fault
------------

If an exception occurs during exception entry (e.g., supervisor stack points
to unmapped memory), the CPU enters DoubleFault state and halts. This is not
recoverable — it indicates a kernel bug (invalid supervisor stack pointer).
