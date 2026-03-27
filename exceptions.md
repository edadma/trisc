Exception Vectors
=================

Each vector entry is 8 bytes (one 64-bit address). Vector address = index × 8.

Index | Address | Exception
----- | ------- | ---------
0     | 0x00    | Reset
1     | 0x08    | Interrupt
2     | 0x10    | Instruction Access (fetch from unmapped address)
3     | 0x18    | Data Access (load/store to unmapped address)
4     | 0x20    | Misaligned Access
5     | 0x28    | Unimplemented Opcode (illegal instruction)
6     | 0x30    | Privilege Violation (spsr/rte in user mode)
7     | 0x38    | Illegal Integer Divide (div/rem by zero)
8     | 0x40    | Trap 0 (supervisor call)
9     | 0x48    | Trap 1
10    | 0x50    | Trap 2
11    | 0x58    | Trap 3
12    | 0x60    | Trap 4
13    | 0x68    | Trap 5
14    | 0x70    | Trap 6
15    | 0x78    | Trap 7

Total: 16 vectors × 8 bytes = 128 bytes (0x00–0x7F)

On exception entry:
1. Registers r1–r7 saved to shadow registers (sr[])
2. PC and PSR saved to spc/spsr
3. PC loaded from vector table entry
4. Mode set to supervisor, state set to Run
5. LL/SC reservation invalidated
