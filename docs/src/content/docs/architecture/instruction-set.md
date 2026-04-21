---
title: Instruction Set
description: Complete TRISC instruction encoding and reference.
---

All TRISC instructions are 16 bits wide. The top 3 bits determine the instruction format.

## RRR — Register-Register-Register

### Block 0: `000 ddd aaa bbb oooo`

Three register operands with a 4-bit opcode. Used for load/store and core arithmetic.

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 0000 | `ldb` | rd = mem[ra + rb] (byte, zero-extended) |
| 0001 | `stb` | mem[rb + rc] = ra (byte) |
| 0010 | `lds` | rd = mem[ra + rb] (short, zero-extended) |
| 0011 | `sts` | mem[rb + rc] = ra (short) |
| 0100 | `ldw` | rd = mem[ra + rb] (32-bit, zero-extended) |
| 0101 | `stw` | mem[rb + rc] = ra (32-bit) |
| 0110 | `ldd` | rd = mem[ra + rb] (64-bit) |
| 0111 | `std` | mem[rb + rc] = ra (64-bit) |
| 1000 | `add` | rd = ra + rb |
| 1001 | `sub` | rd = ra - rb |
| 1010 | `mul` | rd = ra * rb (low 64 bits) |
| 1011 | `div` | rd = ra / rb, r((d+1)&7) = ra % rb |
| 1100 | `cas` | atomic CAS: if mem[ra] == rd then mem[ra] = rb; rd = old |
| 1101 | `and` | rd = ra & rb |
| 1110 | `or` | rd = ra \| rb |
| 1111 | `xor` | rd = ra ^ rb |

:::note
`div` writes both quotient and remainder as a register pair. `mul` clobbers r((d+1)&7) — avoid using r4/r5/r6 as the destination if the next register is live.
:::

### Block 1: `001 ddd aaa bbb oooo`

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 0000 | `asr` | rd = ra >> rb (arithmetic) |
| 0001 | `lsr` | rd = ra >>> rb (logical) |
| 0010 | `lsl` | rd = ra << rb |
| 0011 | `slt` | rd = (ra < rb) ? 1 : 0 (signed) |
| 0100 | `sltu` | rd = (ra < rb) ? 1 : 0 (unsigned) |
| 0101 | `adc` | rd = ra + rb + C |
| 0110 | `sbc` | rd = ra - rb - C |
| 0111 | `mulu` | rd = ra * rb (unsigned) |
| 1000 | `divu` | rd = ra / rb (unsigned), r((d+1)&7) = ra % rb |
| 1001 | `fslt` | rd = (fa < fb) ? 1 : 0 (float) |
| 1010 | `fadd` | fd = fa + fb |
| 1011 | `fsub` | fd = fa - fb |
| 1100 | `fmul` | fd = fa * fb |
| 1101 | `fdiv` | fd = fa / fb |
| 1110 | `fseq` | rd = (fa == fb) ? 1 : 0 (float) |

## RRI — Register-Register-Immediate

`0mm rrr sss iiiiiii`

Two registers and a 7-bit signed immediate. The top bits (mm) select the operation:

| mm | Mnemonic | Operation |
|----|----------|-----------|
| 01 | `beq` | if rs == rr then PC += imm*2 |
| 10 | `blu` | if rs < rr (unsigned) then PC += imm*2 |
| 11 | `bls` | if rs < rr (signed) then PC += imm*2 |

`addi` uses a separate encoding: `110 rrr sss 10 iiiiiii` (7-bit signed immediate).

:::note
The assembler automatically handles branches to labels. `bne`, `bge`, `ble`, `bgeu`, `bleu` are pseudo-instructions that emit a conditional skip + unconditional branch. Far branches are automatically relaxed with a long jump.
:::

## RR — Register-Register

`110 aaa bbb xx ooooo`

Two registers with a 5-bit opcode. The `xx` field selects the sub-block.

### Block 00: General operations

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 00000 | `jalr` | rd = PC+2, PC = rs (jump and link register) |
| 00001 | `halt` | Stop execution |
| 00010 | `zeb` | rd = rs & 0xFF (zero-extend byte) |
| 00011 | `zes` | rd = rs & 0xFFFF (zero-extend short) |
| 00100 | `zew` | rd = rs & 0xFFFFFFFF (zero-extend word) |
| 00101 | `seb` | rd = sign-extend byte |
| 00110 | `ses` | rd = sign-extend short |
| 00111 | `sew` | rd = sign-extend word |
| 01000 | `neg` | rd = -rs |
| 01001 | `not` | rd = ~rs |
| 01010 | `cvt` | rd = int-to-float(rs) or float-to-int(rs) |
| 01011-10000 | | Float unary ops (`fneg`, `finv`, `fint`, `fsqrt`, `fabs`) |
| 10001 | `ll` | rd = load-linked (8-byte, for LL/SC atomics) |
| 10010 | `sc` | store-conditional (succeeds only if reservation valid) |
| 10011-11111 | | Bit ops, min/max, exchange, sign-extend |

### Block 01: Transcendental and MMU

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 00000-01001 | | Float transcendental (`fpow`, `fsin`, `fcos`, ...) |
| 01010 | `tlbi` | TLB invalidate (single entry) |
| 01011 | `tlbia` | TLB invalidate all |
| 01100 | `sptbr` | Set page table base register |
| 01101 | `gptbr` | Get page table base register |
| 01110 | `gfault` | Get faulting address |
| 01111 | `sasid` | Set address space ID |
| 10000 | `gasid` | Get address space ID |
| 10001 | `gfcause` | Get fault cause |
| 10010 | `f32tof64` | Convert single-precision to double-precision |
| 10011 | `f64tof32` | Convert double-precision to single-precision |

### Load/Store with offset

`110 aaa bbb 1x iiiii`

| x | Mnemonic | Operation |
|---|----------|-----------|
| 0 | `ld` | ra = mem[rb + imm*2] (32-bit) |
| 1 | `st` | mem[rb + imm*2] = ra (32-bit) |

The 5-bit signed immediate is scaled by 2, giving a range of -32 to +30 bytes.

## RI — Register-Immediate

`111 rrr oo iiiiiiii`

One register and an 8-bit immediate:

| oo | Mnemonic | Operation |
|----|----------|-----------|
| 00 | `ldi` | rd = sign-extend(imm8) |
| 01 | `auipc` | rd = PC + (imm8 << 8) |
| 10 | `sli` | rd = (rd << 8) \| imm8 |
| 11 | `sti` | rd = (rd << 8) \| imm8 (with sign extension) |

`movi rd, addr` is a pseudo-instruction that emits `auipc` + `addi` to load a full address.

## R — Single register / no operand

`111 000 rrr ooooooo`

Stack operations, system instructions, and traps:

| Mnemonic | Operation |
|----------|-----------|
| `pshb/popb` | Push/pop byte to/from stack |
| `pshs/pops` | Push/pop short |
| `pshw/popw` | Push/pop word (32-bit) |
| `pshd/popd` | Push/pop double (64-bit) |
| `pshr/popr` | Push/pop r1-r5 (5 registers) |
| `rte` | Return from exception |
| `fence` | Memory fence |
| `wfi` | Wait for interrupt |
| `cli/sti` | Clear/set interrupt disable (supervisor only) |
| `swsp` | Swap USP and SSP |
| `trap0`-`trap7` | Software trap (syscall) |
| `gpsr/spsr` | Get/set PSR |
| `gusp/susp` | Get/set user stack pointer |
