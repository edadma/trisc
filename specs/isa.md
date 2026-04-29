# TRISC Instruction Set Architecture

TRISC is a 64-bit RISC teaching ISA with fixed-width 16-bit instructions, 8 general-purpose registers, and a clean supervisor/user mode split.

## Registers

| Register | Name | Purpose |
|----------|------|---------|
| r0 | zero | Hardwired to 0 |
| r1-r6 | | General purpose |
| r7 / sp | | Stack pointer |

All registers are 64 bits wide. r0 always reads as zero; writes to r0 are discarded.

## Instruction Formats

All instructions are 16 bits (2 bytes), always halfword-aligned.

### RRR — Three-register (two blocks)

    [ooo] [ddd] [aaa] [bbb] [oooo]
     3      3     3     3     4     = 16 bits

- `000 ddd aaa bbb oooo` — Load/store, arithmetic, logic (16 ops)
- `001 ddd aaa bbb oooo` — Shifts, comparisons, carry, unsigned, float (16 ops)

### RRI — Register-register-immediate

    [ooo] [aaa] [bbb] [iiiiiii]
     3      3     3      7       = 16 bits

- `010` — beq (branch if equal)
- `011` — blu (branch if less-than, unsigned)
- `100` — bls (branch if less-than, signed)
- `101` — addi (add immediate)

The 7-bit immediate is sign-extended. Branch offsets are in halfwords.

### RR — Two-register with sub-opcode

    [110] [aaa] [bbb] [oo] [ooooo]
      3     3     3    2      5     = 16 bits

- `110 aaa bbb 00 ooooo` — 32 unary/binary register ops (jalr, extensions, bit ops, etc.)
- `110 aaa bbb 01 ooooo` — Extended ops (MMU instructions, f32/f64 conversion)
- `110 aaa bbb 10 iiiii` — ld (load with 5-bit offset)
- `110 aaa bbb 11 iiiii` — st (store with 5-bit offset)

### RI — Register-immediate (r != 0)

    [111] [rrr] [oo] [iiiiiiii]
      3     3    2      8        = 16 bits

- `00` — ldi (load immediate)
- `01` — auipc (add upper immediate to PC)
- `10` — sli (shift left and insert)
- `11` — sti (store immediate to address in register)

### R — Single-register / no-register

    [111] [000] [rrr] [ooooooo]
      3     3     3      7       = 16 bits

Push/pop, system control, traps.

## Addressing Modes

- **Register**: `add rd, ra, rb`
- **Immediate**: `addi rd, ra, imm7` (signed 7-bit)
- **Register+offset**: `ld ra, rb, imm5` / `st ra, rb, imm5`
- **Register+register**: `ldw rd, ra, rb` (for sized loads/stores)
- **PC-relative**: branch instructions, `auipc`

## Branch Pseudos

The assembler provides pseudo-instructions built from native branches:

| Pseudo | Meaning | Implementation |
|--------|---------|----------------|
| bgs ra, rb, target | branch if ra > rb (signed) | bls rb, ra (swap operands) |
| bgu ra, rb, target | branch if ra > rb (unsigned) | blu rb, ra (swap operands) |
| bne ra, rb, target | branch if ra != rb | skip on beq, then branch |
| bge ra, rb, target | branch if ra >= rb (signed) | skip on bls, then branch |
| bgeu ra, rb, target | branch if ra >= rb (unsigned) | skip on blu, then branch |
| ble ra, rb, target | branch if ra <= rb (signed) | skip on bls rb,ra, then branch |
| bleu ra, rb, target | branch if ra <= rb (unsigned) | skip on blu rb,ra, then branch |
| bra target | unconditional branch | beq r0, r0, target |
| nop | no operation | addi r0, r0, 0 |
| ret | return | jalr r0, r7 |

## Data Types

| Type | Size | Load/Store |
|------|------|------------|
| byte | 8-bit | ldb/stb |
| short | 16-bit | lds/sts |
| word | 32-bit | ldw/stw |
| double | 64-bit | ldd/std |
| float | 64-bit IEEE 754 | ldd/std (shared with double) |

## Supervisor Mode

Supervisor mode is entered via exceptions/traps and exited via `rte`. Supervisor-only instructions: `spsr`, `rte`, `wfi`, `gusp`, `susp`, `cli`, `sti`, `swsp`, and MMU instructions.

## Vector Table

20 slots x 8 bytes = 160 bytes at address 0:

| Slot | Purpose |
|------|---------|
| 0 | Initial SSP |
| 1 | Initial PC |
| 2-19 | Exception/trap handlers |

## Memory Model

- Big-endian byte ordering
- Misaligned accesses trigger a data access exception
- `fence` instruction for memory ordering
- `ll`/`sc` for atomic read-modify-write sequences
