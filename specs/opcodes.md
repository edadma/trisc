# TRISC Opcode Map

Complete opcode listing for all native instructions. Pseudo-instructions are documented in [isa.md](isa.md).

## RRR Block 0 — `000 ddd aaa bbb oooo`

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 0000 | ldb | rd = mem[ra + rb] (byte, zero-extended) |
| 0001 | stb | mem[rb + rc] = ra (byte) |
| 0010 | lds | rd = mem[ra + rb] (short, zero-extended) |
| 0011 | sts | mem[rb + rc] = ra (short) |
| 0100 | ldw | rd = mem[ra + rb] (word, zero-extended) |
| 0101 | stw | mem[rb + rc] = ra (word) |
| 0110 | ldd | rd = mem[ra + rb] (double) |
| 0111 | std | mem[rb + rc] = ra (double) |
| 1000 | add | rd = ra + rb (sets carry) |
| 1001 | sub | rd = ra - rb (sets carry/borrow) |
| 1010 | mul | rd = ra * rb (low), rd+1 = high |
| 1011 | div | rd = ra / rb (signed), rd+1 = ra % rb |
| 1100 | cas | compare-and-swap: old = mem[ra]; if old == rd then mem[ra] = rb; rd = old |
| 1101 | and | rd = ra & rb |
| 1110 | or | rd = ra \| rb |
| 1111 | xor | rd = ra ^ rb |

## RRR Block 1 — `001 ddd aaa bbb oooo`

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 0000 | asr | rd = ra >> rb (arithmetic) |
| 0001 | lsr | rd = ra >>> rb (logical) |
| 0010 | lsl | rd = ra << rb |
| 0011 | slt | rd = (ra < rb) ? 1 : 0 (signed) |
| 0100 | sltu | rd = (ra < rb) ? 1 : 0 (unsigned) |
| 0101 | adc | rd = ra + rb + carry (sets carry) |
| 0110 | sbc | rd = ra - rb - borrow (sets carry) |
| 0111 | mulu | rd = ra * rb (unsigned, low), rd+1 = high |
| 1000 | divu | rd = ra / rb (unsigned), rd+1 = ra % rb |
| 1001 | — | (reserved) |
| 1010 | fslt | rd = (fa < fb) ? 1 : 0 (float compare) |
| 1011 | fadd | fd = fa + fb |
| 1100 | fsub | fd = fa - fb |
| 1101 | fmul | fd = fa * fb |
| 1110 | fdiv | fd = fa / fb |
| 1111 | fseq | rd = (fa == fb) ? 1 : 0 (float equal) |

## RRI — `ooo aaa bbb iiiiiii`

| Top 3 | Mnemonic | Operation |
|-------|----------|-----------|
| 010 | beq | if ra == rb then PC += imm * 2 |
| 011 | blu | if ra < rb (unsigned) then PC += imm * 2 |
| 100 | bls | if ra < rb (signed) then PC += imm * 2 |
| 101 | addi | ra = rb + sign_ext(imm) |

## RR Block 00 — `110 aaa bbb 00 ooooo`

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 00000 | halt | stop execution (jalr r0, r0) |
| 00000 | jalr | ra = PC + 2; PC = rb (when rb != 0) |
| 00001 | zeb | ra = rb & 0xFF |
| 00010 | zes | ra = rb & 0xFFFF |
| 00011 | zew | ra = rb & 0xFFFFFFFF |
| 00100 | seb | ra = sign_ext_8(rb) |
| 00101 | ses | ra = sign_ext_16(rb) |
| 00110 | sew | ra = sign_ext_32(rb) |
| 00111 | neg | ra = -rb |
| 01000 | not | ra = ~rb |
| 01001 | cvt | ra = int_to_float(rb) or float_to_int(rb) |
| 01010 | fneg | fa = -fb |
| 01011 | finv | fa = 1.0 / fb |
| 01100 | fint | fa = trunc(fb) |
| 01101 | fsqrt | fa = sqrt(fb) |
| 01110 | fabs | fa = abs(fb) |
| 01111 | ll | ra = mem[rb] (load-linked, sets reservation) |
| 10000 | sc | mem[ra] = rb if reservation valid; ra = success (store-conditional) |
| 10001 | clz | ra = count_leading_zeros(rb) |
| 10010 | ctz | ra = count_trailing_zeros(rb) |
| 10011 | chk | trap if ra < 0 or ra > rb |
| 10100 | btst | ra = (ra >> rb) & 1 |
| 10101 | bset | ra = ra \| (1 << rb) |
| 10110 | bclr | ra = ra & ~(1 << rb) |
| 10111 | rol | ra = rotate_left(ra, rb) |
| 11000 | ror | ra = rotate_right(ra, rb) |
| 11001 | cnt | ra = popcount(rb) |
| 11010 | rev | ra = byte_reverse(rb) |
| 11011 | sext | ra = sign_extend(ra, rb bits) |
| 11100 | mov | ra = rb |
| 11101 | min | ra = min(ra, rb) signed |
| 11110 | max | ra = max(ra, rb) signed |
| 11111 | exg | swap ra and rb |

## RR Block 01 — `110 aaa bbb 01 ooooo`

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 00000 | fpow | ra = pow(ra, rb) (destructive) |
| 00001 | tlbi | invalidate TLB entry for virtual address rb (supervisor) |
| 00010 | tlbia | invalidate all TLB entries (supervisor) |
| 00011 | sptbr | set page table base register to rb (supervisor) |
| 00100 | gptbr | ra = page table base register (supervisor) |
| 00101 | gfault | ra = faulting virtual address (supervisor) |
| 00110 | sasid | set address space ID to rb (supervisor) |
| 00111 | gasid | ra = current address space ID (supervisor) |
| 01000 | gfcause | ra = page fault cause (supervisor) |

## RR Load/Store — `110 aaa bbb mm iiiii`

| Mode | Mnemonic | Operation |
|------|----------|-----------|
| 10 | ld | ra = mem[rb + sign_ext(imm) * 8] (double) |
| 11 | st | mem[rb + sign_ext(imm) * 8] = ra (double) |

## RI — `111 rrr oo iiiiiiii` (r != 0)

| Op | Mnemonic | Operation |
|----|----------|-----------|
| 00 | ldi | rr = sign_ext(imm) |
| 01 | auipc | rr = PC + sign_ext(imm) * 256 |
| 10 | sli | rr = (rr << 8) \| imm |
| 11 | sti | mem[rr] = imm (byte) |

## R — `111 000 rrr ooooooo`

| Opcode | Mnemonic | Operation | Notes |
|--------|----------|-----------|-------|
| 0000000 | pshb | push byte | |
| 0000001 | popb | pop byte | |
| 0000010 | pshs | push short | |
| 0000011 | pops | pop short | |
| 0000100 | pshw | push word | |
| 0000101 | popw | pop word | |
| 0000110 | pshd | push double | |
| 0000111 | popd | pop double | |
| 0001000 | spsr | set PSR | supervisor |
| 0001001 | gpsr | get PSR | |
| 0001010 | rte | return from exception | supervisor |
| 0001011 | fence | memory fence | |
| 0001100 | wfi | wait for interrupt | supervisor |
| 0001101 | gusp | get user stack pointer | supervisor |
| 0001110 | susp | set user stack pointer | supervisor |
| 0001111 | trapv | trap if overflow | |
| 0010000 | pshr | push r1-rN | |
| 0010001 | popr | pop rN-r1 | |
| 0010010 | cli | disable interrupts | supervisor |
| 0010011 | sti | enable interrupts | supervisor |
| 0010100 | swsp | swap r7 and USP | supervisor |
| 0010101 | tsr | read cycle counter | |

## Trap — `111 000 rrr 0011 iii`

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 000 | trap0 | system call 0 |
| 001 | trap1 | system call 1 |
| 010 | trap2 | system call 2 |
| 011 | trap3 | system call 3 |
| 100 | trap4 | system call 4 |
| 101 | trap5 | system call 5 |
| 110 | trap6 | system call 6 |
| 111 | trap7 | system call 7 |
