# TRISC16 Instruction Set Architecture

TRISC16 is a 16-bit subset of TRISC: same instruction encoding, same instruction semantics, but with 16-bit registers and a 16-bit address space. Programs using only the TRISC16 instruction subset run unmodified on a full TRISC implementation, modulo arithmetic that overflows 16 bits.

## Registers

| Register | Name | Purpose |
|----------|------|---------|
| r0       | zero | Hardwired to 0 |
| r1–r6    |      | General purpose |
| r7 / sp  |      | Stack pointer |

All registers are 16 bits wide. r0 always reads as zero; writes to r0 are discarded.

## Address Space

- 16-bit byte addresses → 64 KB total
- Big-endian byte ordering
- Halfword-aligned instruction fetch
- Misaligned data accesses raise a misaligned-access exception

## Instruction Formats

All instructions are 16 bits (2 bytes), always halfword-aligned. Formats are identical to TRISC.

### RRR — Three-register (two blocks)

```
[ooo] [ddd] [aaa] [bbb] [oooo]
 3      3     3     3     4
```

- `000 ddd aaa bbb oooo` — Load/store, arithmetic, logic
- `001 ddd aaa bbb oooo` — Shifts, comparisons, carry, unsigned

### RRI — Register-register-immediate

```
[ooo] [aaa] [bbb] [iiiiiii]
 3      3     3      7
```

- `010` — beq
- `011` — blu
- `100` — bls
- `101` — addi

The 7-bit immediate is sign-extended. Branch offsets are in halfwords (range ±256 bytes).

### RR — Two-register with sub-opcode

```
[110] [aaa] [bbb] [oo] [ooooo]
  3     3     3    2      5
```

- `110 aaa bbb 00 ooooo` — unary/binary register ops
- `110 aaa bbb 01 ooooo` — extended ops (multiply-high, remainder)
- `110 aaa bbb 10 iiiii` — ld (16-bit load with 5-bit offset)
- `110 aaa bbb 11 iiiii` — st (16-bit store with 5-bit offset)

### RI — Register-immediate (r != 0)

```
[111] [rrr] [oo] [iiiiiiii]
  3     3    2      8
```

- `00` — ldi
- `01` — auipc
- `10` — sli
- `11` — sti

### R — Single-register / no-register

```
[111] [000] [rrr] [ooooooo]
  3     3     3      7
```

Push/pop, exception control, traps.

## Boot / Reset

At reset, the CPU loads its initial state from a 2-slot vector table at address 0:

| Address | Contents |
|---------|----------|
| 0x0000  | Initial SP (loaded into r7) |
| 0x0002  | Initial PC |

After loading these values the CPU begins instruction fetch at the new PC. The exception handler address is fixed at 0x0004 (see Exceptions below).

## Exceptions

A minimal exception mechanism. On any exception:

1. EPC ← PC of faulting instruction
2. ECAUSE ← cause code (see below)
3. PSR.E ← 1 (in-exception bit set)
4. PC ← 0x0004

Three internal supervisor registers, accessed via dedicated instructions:

- **EPC** — saved PC at time of exception
- **ECAUSE** — cause code
- **PSR** — processor status (one defined bit: PSR.E, set while in handler)

### Cause Codes

| Code | Meaning |
|------|---------|
| 0    | Reserved |
| 1    | Illegal/unimplemented instruction |
| 2    | Misaligned access |
| 3    | Divide by zero |
| 4    | Overflow (`trapv`) or bounds check (`chk`) |
| 8–15 | Software trap (`trap0`–`trap7`) |

Returning from a handler is via `rte`, which restores PC ← EPC and clears PSR.E.

## Native Instructions

### RRR Block 0 — `000 ddd aaa bbb oooo`

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 0000 | ldb  | rd = mem[ra + rb] (byte, zero-extended to 16) |
| 0001 | stb  | mem[rb + rc] = ra (low byte) |
| 0010 | lds  | rd = mem[ra + rb] (16-bit) |
| 0011 | sts  | mem[rb + rc] = ra (16-bit) |
| 1000 | add  | rd = ra + rb (sets carry) |
| 1001 | sub  | rd = ra - rb (sets borrow) |
| 1010 | mul  | rd = low16(ra * rb) (signed/unsigned identical) |
| 1011 | div  | rd = ra / rb (signed; div-by-zero traps) |
| 1101 | and  | rd = ra & rb |
| 1110 | or   | rd = ra \| rb |
| 1111 | xor  | rd = ra ^ rb |

Reserved opcodes (`0100`–`0111`, `1100`) raise illegal-instruction.

### RRR Block 1 — `001 ddd aaa bbb oooo`

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 0000 | asr  | rd = ra >>s rb (arithmetic; rb taken mod 16) |
| 0001 | lsr  | rd = ra >>u rb (logical; rb taken mod 16) |
| 0010 | lsl  | rd = ra << rb (rb taken mod 16) |
| 0011 | slt  | rd = (ra <s rb) ? 1 : 0 |
| 0100 | sltu | rd = (ra <u rb) ? 1 : 0 |
| 0101 | adc  | rd = ra + rb + carry (sets carry) |
| 0110 | sbc  | rd = ra - rb - borrow (sets borrow) |
| 1000 | divu | rd = ra / rb (unsigned; div-by-zero traps) |

All other opcodes reserved.

### RRI — `ooo aaa bbb iiiiiii`

| Top 3 | Mnemonic | Operation |
|-------|----------|-----------|
| 010 | beq  | if ra == rb then PC += sign_ext(imm) * 2 |
| 011 | blu  | if ra <u rb then PC += sign_ext(imm) * 2 |
| 100 | bls  | if ra <s rb then PC += sign_ext(imm) * 2 |
| 101 | addi | ra = rb + sign_ext(imm) |

### RR Block 00 — `110 aaa bbb 00 ooooo`

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 00000 | halt | stop execution (encoded as `jalr r0, r0`) |
| 00000 | jalr | ra = PC + 2; PC = rb (when rb != r0) |
| 00001 | zeb  | ra = rb & 0xFF |
| 00100 | seb  | ra = sign_ext_8(rb) |
| 00111 | neg  | ra = -rb |
| 01000 | not  | ra = ~rb |
| 10001 | clz  | ra = count_leading_zeros(rb) (0..16) |
| 10010 | ctz  | ra = count_trailing_zeros(rb) (0..16) |
| 10011 | chk  | trap (cause 4) if ra <s 0 or ra >s rb |
| 10100 | btst | ra = (ra >> rb) & 1 |
| 10101 | bset | ra = ra \| (1 << rb) |
| 10110 | bclr | ra = ra & ~(1 << rb) |
| 10111 | rol  | ra = rotate_left_16(ra, rb) |
| 11000 | ror  | ra = rotate_right_16(ra, rb) |
| 11001 | cnt  | ra = popcount(rb) (0..16) |
| 11010 | rev  | ra = byte_swap_16(rb) |
| 11011 | sext | ra = sign_extend(ra, low rb bits) |
| 11100 | mov  | ra = rb |
| 11101 | min  | ra = min_signed(ra, rb) |
| 11110 | max  | ra = max_signed(ra, rb) |
| 11111 | exg  | swap ra and rb |

All other opcodes reserved.

### RR Block 01 — `110 aaa bbb 01 ooooo`

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 00000 | mulh   | ra = high16(ra *s rb) (destructive) |
| 01001 | mulhu  | ra = high16(ra *u rb) (destructive) |
| 01010 | mulhsu | ra = high16(ra_signed *u rb_unsigned) (destructive) |
| 01011 | rem    | ra = ra %s rb (destructive; div-by-zero traps) |
| 01100 | remu   | ra = ra %u rb (destructive; div-by-zero traps) |

All other opcodes reserved.

### RR Load/Store — `110 aaa bbb mm iiiii`

| Mode | Mnemonic | Operation |
|------|----------|-----------|
| 10 | ld | ra = mem[rb + sign_ext(imm) * 2] (16-bit) |
| 11 | st | mem[rb + sign_ext(imm) * 2] = ra (16-bit) |

5-bit signed offset gives ±32 halfwords (±64 bytes) reach.

### RI — `111 rrr oo iiiiiiii` (rrr != 000)

| Op | Mnemonic | Operation |
|----|----------|-----------|
| 00 | ldi   | rr = sign_ext_16(imm) |
| 01 | auipc | rr = PC + sign_ext(imm) * 2 |
| 10 | sli   | rr = (rr << 8) \| imm |
| 11 | sti   | mem[rr] = imm (byte) |

Note: `auipc` and `ld`/`st` use scaling factors appropriate for 16-bit registers (×2 instead of TRISC's ×256 and ×8). This is the only place TRISC16 semantics differ from TRISC; the encoding bits are identical.

A full 16-bit absolute address is built with `ldi` + `sli` (two instructions cover the entire address space).

### R — `111 000 rrr ooooooo`

| Opcode  | Mnemonic | Operation |
|---------|----------|-----------|
| 0000000 | pshb     | push byte (low 8 bits of rr; SP -= 1) |
| 0000001 | popb     | pop byte (zero-extended into rr; SP += 1) |
| 0000010 | pshs     | push 16-bit (rr; SP -= 2) |
| 0000011 | pops     | pop 16-bit (rr; SP += 2) |
| 0001001 | gpsr     | rr = PSR |
| 0001010 | rte      | PC = EPC; PSR.E = 0 |
| 0001011 | fence    | memory fence (no-op on single-core) |
| 0001100 | gepc     | rr = EPC |
| 0001101 | gcause   | rr = ECAUSE |
| 0001111 | trapv    | trap (cause 4) if last add/sub/adc/sbc overflowed |
| 0010000 | pshr rN  | push r1..rN (2 bytes each, r1 deepest, rN on top); N = `rrr`, range 1..6 |
| 0010001 | popr rN  | pop rN..r1 (rN first, r1 last); N = `rrr`, range 1..6 |

`pshr`/`popr` with `rrr` = 000 or 111 raise illegal-instruction.

### Trap — `111 000 rrr 0011 iii`

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 000 | trap0 | software trap, cause 8 |
| 001 | trap1 | software trap, cause 9 |
| 010 | trap2 | software trap, cause 10 |
| 011 | trap3 | software trap, cause 11 |
| 100 | trap4 | software trap, cause 12 |
| 101 | trap5 | software trap, cause 13 |
| 110 | trap6 | software trap, cause 14 |
| 111 | trap7 | software trap, cause 15 |

## Branch Pseudo-Instructions

The assembler synthesizes the missing comparisons from native branches:

| Pseudo                   | Implementation |
|--------------------------|----------------|
| bgs ra, rb, target       | bls rb, ra, target (operands swapped) |
| bgu ra, rb, target       | blu rb, ra, target (operands swapped) |
| bne ra, rb, target       | beq ra, rb, .skip; bra target; .skip: |
| bge ra, rb, target       | bls ra, rb, .skip; bra target; .skip: |
| bgeu ra, rb, target      | blu ra, rb, .skip; bra target; .skip: |
| ble ra, rb, target       | bls rb, ra, .skip; bra target; .skip: |
| bleu ra, rb, target      | blu rb, ra, .skip; bra target; .skip: |
| bra target               | beq r0, r0, target |
| nop                      | addi r0, r0, 0 |
| ret                      | jalr r0, r7 |

## Data Types

| Type   | Size  | Load / Store |
|--------|-------|--------------|
| byte   | 8-bit | ldb / stb / sti |
| short  | 16-bit| lds / sts / ld / st |

TRISC16 has no native 32-bit or 64-bit data type. Wider arithmetic is built in software using `adc`/`sbc` for multi-precision add and subtract.

## Compatibility With TRISC

A program built using only the TRISC16 instruction subset runs unmodified on a full TRISC implementation, with the following caveats:

1. Arithmetic results that overflow 16 bits behave differently — TRISC will produce the full 64-bit result; TRISC16 will wrap at 16 bits.
2. `auipc`, `ld`, and `st` use different scaling factors. A TRISC16 binary using these instructions will not behave correctly on TRISC unless the assembler/loader rewrites them, or the binary deliberately avoids them.
3. The TRISC16 boot vector is 4 bytes (SP, PC); TRISC's is 160 bytes. A TRISC16 image will boot on TRISC if the TRISC vector table's first two slots match.

Instructions present in TRISC but absent in TRISC16 (floats, 32/64-bit loads/stores, atomics, MMU, full supervisor mode) raise illegal-instruction on TRISC16.

## Implementation Order (Suggested)

1. **Core datapath** (~23 instructions): addi, add, sub, and, or, xor, lsl, lsr, asr, slt, sltu, mov, ldi, sli, beq, bls, blu, jalr, lds, sts, ldb, stb, halt
2. **Stack, convenience ops, exception infrastructure**: auipc, ld, st, pshs, pops, pshr, popr, neg, not, min, max, exg, seb, zeb; EPC/ECAUSE/PSR; gepc, gcause, gpsr, rte; illegal-instruction trap; trap0–trap7
3. **Multiply/divide** (multi-cycle): mul, div, divu, mulh, mulhu, mulhsu, rem, remu (with div-by-zero now raising a real exception)
4. **Bit manipulation**: clz, ctz, cnt, rev, rol, ror, btst, bset, bclr, sext, chk
5. **Misc**: adc, sbc, fence (nop), trapv, sti, pshb, popb
