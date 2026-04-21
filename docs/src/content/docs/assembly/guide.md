---
title: Assembler Guide
description: Writing TRISC assembly programs.
---

The TRISC assembler is a two-pass assembler that produces TOF (TRISC Object Format) files. It supports labels, segments, data directives, pseudo-instructions, and symbolic constants.

## Basic syntax

```asm
; This is a comment
label_name
  movi r1, 42        ; load 42 into r1
  halt
```

Labels are on their own line (no colon). Instructions are indented. Comments start with `;`.

## Data directives

| Directive | Size | Description |
|-----------|------|-------------|
| `db` | 1B | Define byte(s) |
| `ds` | 2B | Define short(s) |
| `dd` | 4B | Define word(s) |
| `dl` | 8B | Define long (double-word) |
| `rb N` | NB | Reserve N bytes (zeroed) |
| `align N` | -- | Align to N-byte boundary |

Data directives automatically align to their natural size. Strings are supported in `db`:

```asm
message
  db "Hello, world!", 10, 0   ; string + newline + null terminator
```

## Pseudo-instructions

| Pseudo | Expansion | Description |
|--------|-----------|-------------|
| `movi rd, addr` | `auipc` + `addi` | Load full address into register |
| `ldi rd, imm` | RI format | Load 8-bit signed immediate |
| `ret` | `jalr r0, r6` | Return from subroutine |
| `nop` | `add r0, r0, r0` | No operation |
| `bne ra, rb, label` | `beq` + `bra` | Branch if not equal |
| `bge ra, rb, label` | `bls` + `bra` | Branch if greater or equal (signed) |
| `ble ra, rb, label` | Swapped `bge` | Branch if less or equal (signed) |
| `bra label` | `beq r0, r0, label` | Unconditional branch |

## Segments

The assembler supports multiple segments with explicit origins:

```asm
.segment code org=0
  ; code here

.segment data org=0x4000
  ; data here
```

## Extern and global

```asm
extern printf       ; symbol defined in another file
global my_function  ; export symbol for linking
```

## Expressions

Labels and constants can be used in expressions:

```asm
ldi r1, buffer_end - buffer   ; size of buffer
movi r1, handler + 8          ; offset from label
```

## TOF object format

The assembler outputs relocatable TOF files. The linker resolves symbols across files and produces an executable TOF with an entry point. The runtime boot module (vector table + I/O stubs) is automatically prepended.
