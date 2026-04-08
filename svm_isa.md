# SysL Virtual Machine (SVM)
## Bytecode ISA — Complete Instruction Set Reference
*Version 0.1 — 64-bit Cells, Software Interpreter*

---

## Overview

A software stack machine ISA targeting fast bytecode interpretation. All design choices are driven by interpreter performance on modern hardware.

### Core Design Principles

- **64-bit cells** — Every stack slot is exactly 8 bytes. Operations are implicitly 64-bit. Width suffixes only appear in memory access and literal encoding.
- **Dense bytecodes** — 1-byte opcodes. A 64-byte cache line holds 64 zero-operand instructions. Code density is the dominant performance factor.
- **Computed goto dispatch** — Each opcode dispatches via `&&label` in GCC/Clang. Eliminates one branch mispredict per instruction vs. switch.
- **TOS-in-register** — The top of stack is kept in a C local variable so the compiler can register-allocate it. The in-memory stack holds depth-2 and below.
- **Superinstructions** — Common 2–3 opcode sequences encoded as single opcodes to halve dispatch overhead on hot paths.

### Instruction Encoding

Most instructions are 1 byte. Instructions with operands append the operand bytes immediately after the opcode with no padding or alignment.

```
┌──────────┐
│  opcode  │   1 byte — zero-operand instructions
└──────────┘

┌──────────┬──────────────────┐
│  opcode  │  imm8/16/32/64   │  literals, branches, locals
└──────────┴──────────────────┘
```

> Branch offsets are signed 16-bit relative (±32 KB). `JUMP_WIDE` provides a 32-bit offset for large functions.

### Stack Notation

Stack effects are written as `( before -- after )` where the rightmost item is TOS (top of stack).

`( a b -- b a )` means: consume `a` and `b`, push `b` then `a`. `b` ends up on top.

- `TOS` = top of stack (depth-0)
- `NOS` = next on stack (depth-1)

> For STORE ops the convention is `( val addr -- )`. Push the value first, then the destination address, then STORE.

---

## 0x00–0x0F  Stack Operations

Core stack manipulation. No operands; all are 1 byte.

| Op | Mnemonic | Stack Effect | Notes |
|----|----------|--------------|-------|
| 0x00 | `NOP` | ( -- ) | no operation |
| 0x01 | `DROP` | ( a -- ) | discard TOS |
| 0x02 | `DUP` | ( a -- a a ) | duplicate TOS |
| 0x03 | `SWAP` | ( a b -- b a ) | swap top two |
| 0x04 | `OVER` | ( a b -- a b a ) | copy NOS to top |
| 0x05 | `ROT` | ( a b c -- b c a ) | rotate up |
| 0x06 | `NROT` | ( a b c -- c a b ) | rotate down |
| 0x07 | `NIP` | ( a b -- b ) | drop NOS |
| 0x08 | `TUCK` | ( a b -- b a b ) | copy TOS under NOS |
| 0x09 | `DROP2` | ( a b -- ) | drop top two |
| 0x0A | `DUP2` | ( a b -- a b a b ) | duplicate top pair |
| 0x0B | `SWAP2` | ( a b c d -- c d a b ) | swap top two pairs |
| 0x0C | `OVER2` | ( a b c d -- a b c d a b ) | copy lower pair to top |
| 0x0D | `DEPTH` | ( -- n ) | push current stack depth |
| 0x0E–0x0F | — | | reserved |

---

## 0x10–0x1F  Literals

Push constant values onto the stack. Small common constants are 1 byte; larger values carry inline operands.

| Op | Mnemonic | Pushed Value | Notes |
|----|----------|--------------|-------|
| 0x10 | `PUSH_0` | 0 | 1 byte |
| 0x11 | `PUSH_1` | 1 | 1 byte |
| 0x12 | `PUSH_2` | 2 | 1 byte |
| 0x13 | `PUSH_M1` | -1 (0xFFFFFFFFFFFFFFFF) | all bits set, 1 byte |
| 0x14 | `PUSH_i8 <i8>` | sign-extend to i64 | 2 bytes total |
| 0x15 | `PUSH_u8 <u8>` | zero-extend to u64 | 2 bytes total |
| 0x16 | `PUSH_i16 <i16>` | sign-extend to i64 | 3 bytes total |
| 0x17 | `PUSH_i32 <i32>` | sign-extend to i64 | 5 bytes — covers ~95% of constants |
| 0x18 | `PUSH_i64 <i64>` | full 64-bit value | 9 bytes; use for addresses & f64 bit patterns |
| 0x19–0x1F | — | | reserved |

> `PUSH_i64` is the universal float literal: push the IEEE 754 bit pattern of any f64 constant.

---

## 0x20–0x2F  Integer Arithmetic

All arithmetic operates on 64-bit integers. Signed/unsigned distinction only for division, modulo, and right shift. Two's complement makes ADD/SUB identical for both signedness.

| Op | Mnemonic | Stack Effect | Notes |
|----|----------|--------------|-------|
| 0x20 | `ADD` | ( a b -- a+b ) | |
| 0x21 | `SUB` | ( a b -- a-b ) | NOS minus TOS |
| 0x22 | `MUL` | ( a b -- a*b ) | |
| 0x23 | `DIV` | ( a b -- a/b ) | signed, truncate toward zero |
| 0x24 | `MOD` | ( a b -- a%b ) | signed remainder |
| 0x25 | `DIVMOD` | ( a b -- rem quot ) | signed; pushes remainder then quotient |
| 0x26 | `DIVU` | ( a b -- a/b ) | unsigned |
| 0x27 | `MODU` | ( a b -- a%b ) | unsigned |
| 0x28 | `NEG` | ( a -- -a ) | two's complement negate |
| 0x29 | `ABS` | ( a -- \|a\| ) | absolute value |
| 0x2A | `INC` | ( a -- a+1 ) | avoids PUSH_1 ADD |
| 0x2B | `DEC` | ( a -- a-1 ) | avoids PUSH_1 SWAP SUB |
| 0x2C–0x2F | — | | reserved |

---

## 0x30–0x3F  Bitwise Operations

Bitwise and shift operations on 64-bit values. Shift amount is TOS; value being shifted is NOS.

| Op | Mnemonic | Stack Effect | Notes |
|----|----------|--------------|-------|
| 0x30 | `AND` | ( a b -- a&b ) | |
| 0x31 | `OR` | ( a b -- a\|b ) | |
| 0x32 | `XOR` | ( a b -- a^b ) | |
| 0x33 | `NOT` | ( a -- ~a ) | bitwise complement |
| 0x34 | `SHL` | ( val shift -- result ) | shift left |
| 0x35 | `SHR` | ( val shift -- result ) | logical shift right (unsigned) |
| 0x36 | `SAR` | ( val shift -- result ) | arithmetic shift right (signed) |
| 0x37 | `CLZ` | ( a -- n ) | count leading zeros |
| 0x38 | `CTZ` | ( a -- n ) | count trailing zeros |
| 0x39 | `POPCNT` | ( a -- n ) | population count (set bits) |
| 0x3A | `ROTL` | ( val shift -- result ) | rotate left |
| 0x3B | `ROTR` | ( val shift -- result ) | rotate right |
| 0x3C | `BSWAP` | ( a -- a' ) | byte-swap (endian flip) |
| 0x3D–0x3F | — | | reserved |

---

## 0x40–0x4F  Integer Comparison

All comparison ops produce 0 (false) or 1 (true) as a 64-bit cell. Binary comparisons consume two values; zero-test ops consume only TOS.

| Op | Mnemonic | Stack Effect | Notes |
|----|----------|--------------|-------|
| 0x40 | `EQ` | ( a b -- flag ) | |
| 0x41 | `NEQ` | ( a b -- flag ) | |
| 0x42 | `LT` | ( a b -- flag ) | signed a < b |
| 0x43 | `GT` | ( a b -- flag ) | signed a > b |
| 0x44 | `LE` | ( a b -- flag ) | signed a <= b |
| 0x45 | `GE` | ( a b -- flag ) | signed a >= b |
| 0x46 | `LTU` | ( a b -- flag ) | unsigned a < b |
| 0x47 | `GTU` | ( a b -- flag ) | unsigned a > b |
| 0x48 | `LEU` | ( a b -- flag ) | unsigned a <= b |
| 0x49 | `GEU` | ( a b -- flag ) | unsigned a >= b |
| 0x4A | `EQZ` | ( a -- flag ) | TOS == 0; single-operand, very hot |
| 0x4B | `NEZ` | ( a -- flag ) | TOS != 0 |
| 0x4C | `LTZ` | ( a -- flag ) | TOS < 0 (sign test) |
| 0x4D | `GTZ` | ( a -- flag ) | TOS > 0 |
| 0x4E | `LEZ` | ( a -- flag ) | TOS <= 0 |
| 0x4F | `GEZ` | ( a -- flag ) | TOS >= 0 |

> The zero-test variants (EQZ, NEZ, LTZ…) touch only TOS and are among the most frequently emitted opcodes. Keeping them 1 byte is critical for code density.

---

## 0x50–0x5F  Memory

Load and store with explicit width. Loads zero- or sign-extend to 64 bits. Stores truncate to the target width.

> Store convention: `( val addr -- )`. Push the value first, then the destination address, then STORE.

| Op | Mnemonic | Stack Effect | Notes |
|----|----------|--------------|-------|
| 0x50 | `LOAD8` | ( addr -- u8 ) | zero-extend |
| 0x51 | `LOAD8S` | ( addr -- i8 ) | sign-extend |
| 0x52 | `LOAD16` | ( addr -- u16 ) | zero-extend |
| 0x53 | `LOAD16S` | ( addr -- i16 ) | sign-extend |
| 0x54 | `LOAD32` | ( addr -- u32 ) | zero-extend |
| 0x55 | `LOAD32S` | ( addr -- i32 ) | sign-extend |
| 0x56 | `LOAD64` | ( addr -- u64 ) | natural case |
| 0x57 | — | | reserved |
| 0x58 | `STORE8` | ( val addr -- ) | truncate to 8 bits |
| 0x59 | `STORE16` | ( val addr -- ) | truncate to 16 bits |
| 0x5A | `STORE32` | ( val addr -- ) | truncate to 32 bits |
| 0x5B | `STORE64` | ( val addr -- ) | natural case |
| 0x5C–0x5F | — | | reserved |

---

## 0x60–0x6F  Control Flow

Branches use signed 16-bit relative offsets from the byte immediately following the full instruction. `JUMP_WIDE` and `CALL` use 32-bit offsets. `CALL_ABS` uses a full 64-bit absolute address for FFI.

| Op | Mnemonic | Stack Effect | Notes |
|----|----------|--------------|-------|
| 0x60 | `JUMP <rel16>` | ( -- ) | unconditional relative jump |
| 0x61 | `JUMPZ <rel16>` | ( cond -- ) | jump if TOS == 0, consume TOS |
| 0x62 | `JUMPNZ <rel16>` | ( cond -- ) | jump if TOS != 0, consume TOS |
| 0x63 | `CALL <rel32>` | ( -- ) | push return addr, jump |
| 0x64 | `RET` | ( -- ) | pop return addr, jump |
| 0x65 | `TAIL <rel32>` | ( -- ) | tail call; reuse current frame |
| 0x66 | `CALLR` | ( addr -- ) | call address from TOS (closures/vtables) |
| 0x67 | `TAILR` | ( addr -- ) | tail call address from TOS |
| 0x68 | `JUMP_WIDE <rel32>` | ( -- ) | 32-bit unconditional jump for large functions |
| 0x69 | `CALL_ABS <abs64>` | ( -- ) | call absolute address; FFI into host |
| 0x6A | `TRAP <u8>` | ( -- ) | host syscall; 256 call numbers |
| 0x6B | `HALT` | ( -- ) | stop VM |
| 0x6C–0x6F | — | | reserved |

---

## 0x70–0x7F  Local Variables

Local variables occupy a side array per frame, separate from the data stack. `FRAME` must be the first instruction of every function that uses locals.

| Op | Mnemonic | Stack Effect | Notes |
|----|----------|--------------|-------|
| 0x70 | `FRAME <u8>` | ( -- ) | allocate N local slots; function prologue |
| 0x71 | `LOCAL_GET <u8>` | ( -- val ) | push local[i] onto data stack |
| 0x72 | `LOCAL_SET <u8>` | ( val -- ) | pop TOS into local[i] |
| 0x73 | `LOCAL_TEE <u8>` | ( val -- val ) | copy TOS into local[i]; leave on stack |
| 0x74–0x7F | — | | reserved |

> `LOCAL_TEE` is identical to `LOCAL_SET` followed by `LOCAL_GET` but avoids the round-trip for the common pattern of assigning and immediately using a value.

---

## 0x80–0x8F  Return Stack

A secondary stack separate from the data stack. Primarily useful for loop counters and Forth-style iteration, keeping loop state out of the data stack.

| Op | Mnemonic | Stack Effect | Notes |
|----|----------|--------------|-------|
| 0x80 | `R_PUSH` | ( val -- ) [R: -- val] | move TOS to return stack |
| 0x81 | `R_POP` | ( -- val ) [R: val --] | move return stack top to data stack |
| 0x82 | `R_PEEK` | ( -- val ) [R: val -- val] | copy return stack top; non-destructive |
| 0x83–0x8F | — | | reserved |

---

## 0x90–0xAF  Floating Point (f64)

IEEE 754 double-precision operations. Cells are 64 bits so floats are stored as raw bit patterns — no boxing, no allocation. Use `PUSH_i64` with the IEEE 754 bit pattern to push float constants.

### Arithmetic

| Op | Mnemonic | Stack Effect | Notes |
|----|----------|--------------|-------|
| 0x90 | `FADD` | ( a b -- a+b ) | |
| 0x91 | `FSUB` | ( a b -- a-b ) | NOS minus TOS |
| 0x92 | `FMUL` | ( a b -- a*b ) | |
| 0x93 | `FDIV` | ( a b -- a/b ) | |
| 0x94 | `FMOD` | ( a b -- a%b ) | |
| 0x95 | `FNEG` | ( a -- -a ) | negate |
| 0x96 | `FABS` | ( a -- \|a\| ) | absolute value |
| 0x97 | `FSQRT` | ( a -- sqrt(a) ) | square root |
| 0x98 | `FFLOOR` | ( a -- floor(a) ) | round toward -inf |
| 0x99 | `FCEIL` | ( a -- ceil(a) ) | round toward +inf |
| 0x9A | `FROUND` | ( a -- round(a) ) | round to nearest even |
| 0x9B | `FTRUNC` | ( a -- trunc(a) ) | truncate toward zero |
| 0x9C | `FMIN` | ( a b -- min ) | |
| 0x9D | `FMAX` | ( a b -- max ) | |

### Comparison

| Op | Mnemonic | Stack Effect | Notes |
|----|----------|--------------|-------|
| 0x9E | `FEQ` | ( a b -- flag ) | |
| 0x9F | `FNEQ` | ( a b -- flag ) | |
| 0xA0 | `FLT` | ( a b -- flag ) | a < b |
| 0xA1 | `FGT` | ( a b -- flag ) | a > b |
| 0xA2 | `FLE` | ( a b -- flag ) | a <= b |
| 0xA3 | `FGE` | ( a b -- flag ) | a >= b |

### Conversions

| Op | Mnemonic | Stack Effect | Notes |
|----|----------|--------------|-------|
| 0xA4 | `F2I` | ( f -- i64 ) | f64 → i64, truncate toward zero |
| 0xA5 | `I2F` | ( i -- f64 ) | i64 → f64 |
| 0xA6 | `F2U` | ( f -- u64 ) | f64 → u64, truncate toward zero |
| 0xA7 | `U2F` | ( u -- f64 ) | u64 → f64 |

### Float Constants

| Op | Mnemonic | Stack Effect | Notes |
|----|----------|--------------|-------|
| 0xA8 | `PUSH_F0` | ( -- 0.0 ) | push IEEE 754 positive zero |
| 0xA9 | `PUSH_F1` | ( -- 1.0 ) | push IEEE 754 1.0 |
| 0xAA–0xAF | — | | reserved |

---

## 0xB0–0xCF  Superinstructions

Compound opcodes encoding common 2–3 instruction sequences as a single dispatch. These halve dispatch overhead on hot paths. Profile a real workload and add the top-N 2-grams and 3-grams by frequency to fill the remaining reserved slots.

### Stack + Arithmetic

| Op | Mnemonic | Expands To | Notes |
|----|----------|------------|-------|
| 0xB0 | `DUP_ADD` | DUP ADD | multiply by 2 |
| 0xB1 | `DUP_MUL` | DUP MUL | square TOS |
| 0xB2 | `OVER_ADD` | OVER ADD | running sum |
| 0xB3 | `OVER_SUB` | OVER SUB | delta |

### Immediate Arithmetic

| Op | Mnemonic | Expands To | Notes |
|----|----------|------------|-------|
| 0xB4 | `ADD_IMM8 <i8>` | PUSH_i8 ADD | pointer offset, loop step |
| 0xB5 | `SUB_IMM8 <i8>` | PUSH_i8 SWAP SUB | |
| 0xB6 | `MUL_IMM8 <i8>` | PUSH_i8 MUL | array stride scaling |

### Branch Fusions

| Op | Mnemonic | Expands To | Notes |
|----|----------|------------|-------|
| 0xB7 | `EQZ_JUMPZ <rel16>` | EQZ JUMPZ | branch if nonzero; very hot |
| 0xB8 | `EQZ_JUMPNZ <rel16>` | EQZ JUMPNZ | branch if zero |
| 0xB9 | `INC_JUMPNZ <rel16>` | INC JUMPNZ | loop counter up |
| 0xBA | `DEC_JUMPNZ <rel16>` | DEC JUMPNZ | loop countdown; extremely common |

### Local Variable Fusions

| Op | Mnemonic | Expands To | Notes |
|----|----------|------------|-------|
| 0xBB | `LOCAL_GET_ADD <u8>` | LOCAL_GET ADD | accumulator update |
| 0xBC | `LOCAL_GET_SUB <u8>` | LOCAL_GET SUB | |
| 0xBD | `LOCAL_GET_EQZ <u8>` | LOCAL_GET EQZ | test local variable |
| 0xBE | `LOCAL_GET_JUMPZ <u8> <rel16>` | LOCAL_GET JUMPZ | conditional on local |
| 0xBF | `LOCAL_GET_JUMPNZ <u8> <rel16>` | LOCAL_GET JUMPNZ | loop on local |

### Memory Fusions

| Op | Mnemonic | Expands To | Notes |
|----|----------|------------|-------|
| 0xC0 | `DUP_LOAD64` | DUP LOAD64 | peek through pointer, keep address |
| 0xC1 | `DROP_JUMP <rel16>` | DROP JUMP | clean stack then branch |
| 0xC2 | `PUSH_i8_ADD <i8>` | PUSH_i8 ADD | struct field address |
| 0xC3 | `PUSH_i8_LOAD64 <i8>` | PUSH_i8 ADD LOAD64 | field load by offset |
| 0xC4–0xCF | — | | reserved for profiled additions |

---

## 0xD0–0xFF  Extended and Debug

| Op | Mnemonic | Stack Effect | Notes |
|----|----------|--------------|-------|
| 0xD0–0xFE | — | | reserved for future extension |
| 0xFF | `BREAKPOINT` | ( -- ) | debugger trap; halt and notify host |

---

## Calling Convention

### Call Sequence

Arguments are pushed in order (first argument pushed first, so TOS holds the first argument at the point of CALL). The callee allocates locals with FRAME, executes, and leaves return values on the stack before RET.

```
; caller
  PUSH_i32  42      ; arg 1
  PUSH_i32  7       ; arg 2  (TOS = first arg)
  CALL      target

; callee
target:
  FRAME     2       ; allocate 2 locals
  LOCAL_SET 0       ; pop arg 1 into local[0]
  LOCAL_SET 1       ; pop arg 2 into local[1]
  ...
  PUSH_i32  result  ; push return value(s)
  RET
```

### Multiple Return Values

Return values are left on the stack before RET. The number of return values is part of the function's type signature tracked by the compiler; it is not encoded in the bytecode itself.

### Tail Calls

`TAIL` and `TAILR` reuse the current call frame. The compiler is responsible for ensuring the callee's argument count matches the tail-call convention.

---

## Interpreter Loop

Canonical implementation in C using GCC/Clang computed goto for minimal dispatch overhead.

```c
typedef uint64_t u64;
typedef uint8_t  u8;

#define NEXT   goto *dispatch[*ip++]
#define TOS    stack[sp]
#define NOS    stack[sp - 1]

static void *dispatch[256] = {
    &&op_nop, &&op_drop, &&op_dup, /* ... */
};

u64  stack[STACK_DEPTH];
u64 *locals;
u8  *ip;
int  sp = -1;

op_add:
    NOS += TOS;  sp--;  NEXT;

op_dup:
    stack[sp + 1] = TOS;  sp++;  NEXT;

op_push_i32: {
    int32_t imm;
    memcpy(&imm, ip, 4);  ip += 4;
    stack[++sp] = (int64_t)imm;   // sign-extend
    NEXT;
}

op_load64:
    TOS = *(u64 *)TOS;  NEXT;

op_store64:
    *(u64 *)TOS = NOS;  sp -= 2;  NEXT;
```

> For maximum performance, declare an explicit `register u64 tos` and keep TOS there, spilling to `stack[]` only when depth changes. GCC will often do this automatically for `stack[sp]` when `sp` is a local, but explicit is more reliable.

---

## Opcode Map Summary

| Range | Category | Ops Defined |
|-------|----------|-------------|
| 0x00–0x0F | Stack Operations | 14 |
| 0x10–0x1F | Literals | 9 |
| 0x20–0x2F | Integer Arithmetic | 12 |
| 0x30–0x3F | Bitwise Operations | 13 |
| 0x40–0x4F | Integer Comparison | 16 |
| 0x50–0x5F | Memory | 11 |
| 0x60–0x6F | Control Flow | 12 |
| 0x70–0x7F | Local Variables | 4 |
| 0x80–0x8F | Return Stack | 3 |
| 0x90–0xAF | Floating Point (f64) | 26 |
| 0xB0–0xCF | Superinstructions | 20 |
| 0xD0–0xFE | Reserved | — |
| 0xFF | Debug / Breakpoint | 1 |

**141 opcodes defined. 115 slots reserved** for future use or profiled superinstruction additions.
