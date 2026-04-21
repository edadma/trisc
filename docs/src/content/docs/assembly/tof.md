---
title: TOF Object Format
description: The TRISC Object Format — relocatable and executable object files.
---

TOF (TRISC Object Format) is a text-based object file format used by the assembler, compiler, and linker. It carries code, data, symbols, relocations, and metadata in a human-readable format. A companion binary format (TRB) is used for fast loading.

## File structure

A TOF file is plain text with one directive per line:

```
TOF v1
TYPE:relocatable
SEGMENT:code,0
SYMBOL:main,0,func
EXTERN:printf
RELOC:MOVI2,4,printf
DATA:0102030405060708
RES:100
```

### Header

Every TOF file starts with a version header:

```
TOF v1
```

### Type

```
TYPE:object        (default if omitted)
TYPE:relocatable   (assembled with relocation info)
TYPE:executable    (fully linked, has entry point)
```

| Type | Description |
|------|-------------|
| `object` | Raw assembled output, no relocation info |
| `relocatable` | Contains symbols, externs, and relocations for linking |
| `executable` | Fully linked, all symbols resolved, has an entry point |

### Entry point

```
ENTRY:main
```

Names the symbol where execution begins. Required for executables. The runtime boot module reads the initial SSP from vector slot 0 and jumps to the entry address.

## Segments

```
SEGMENT:code,0
SEGMENT:data,4000
```

Format: `SEGMENT:name,org` where `org` is a hex address. Each segment has its own origin, symbols, externs, and relocations. Multiple segments can coexist in one TOF file.

## Data and reservations

```
DATA:48656c6c6f     (hex-encoded bytes: "Hello")
RES:100             (reserve 0x100 bytes, zeroed)
```

`DATA` lines contain hex-encoded bytes (2 hex chars per byte). Long data is split into multiple `DATA` lines (max 2048 hex chars each). `RES` reserves zeroed space.

## Symbols

```
SYMBOL:main,0,func
SYMBOL:buffer,a0,data,100
SYMBOL:MAX_SIZE,0,const
```

Format: `SYMBOL:name,offset,type[,size][,typeinfo]`

| Field | Description |
|-------|-------------|
| name | Symbol name |
| offset | Hex offset from segment origin |
| type | `func`, `data`, or `const` |
| size | Optional hex size (for data symbols) |
| typeinfo | Optional type signature string |

## Externs

```
EXTERN:printf
EXTERN:malloc
```

Declares symbols that must be resolved by the linker from another TOF file.

## Relocations

```
RELOC:MOVI2,4,printf
RELOC:ABS64,10,buffer
```

Format: `RELOC:type,offset,symbol`

| Type | Description |
|------|-------------|
| `MOVI2` | Patch 2-instruction `movi` sequence (16-bit address) |
| `MOVI3` | Patch 3-instruction `movi` sequence (24-bit address) |
| `MOVI4` | Patch 4-instruction `movi` sequence (32-bit address) |
| `ABS32` | Patch 32-bit absolute address |
| `ABS64` | Patch 64-bit absolute address |

The `MOVI` relocations patch the immediate bytes in `auipc` + `sli`/`addi` instruction sequences that the assembler emits for `movi rd, symbol`.

## Comments

```
# This is a comment
```

Comments are preserved through assembly and linking for debugging.

## Linking

The linker (`trisc link`) takes multiple TOF files and:

1. Merges segments by name (concatenating data, adjusting offsets)
2. Resolves externs against symbols from all input files
3. Applies relocations (patching addresses in the merged output)
4. Sets the entry point
5. Produces an executable TOF

The runtime boot module is automatically prepended when running, providing the vector table and I/O stubs.

## TRB — TRISC Binary

TRB is a compact binary format for fast loading by the OS loader and ramdisk prefill. It is produced from a fully linked executable TOF.

### Layout (little-endian)

| Offset | Size | Field |
|--------|------|-------|
| 0 | 4B | Magic: `TRB\x01` |
| 4 | 4B | Entry point (u32, absolute address) |
| 8 | 4B | Record count (u32) |
| 12+ | var | Records |

### Record format

| Field | Size | Description |
|-------|------|-------------|
| org | 4B | Load address (u32) |
| kind | 4B | 0 = PROGBITS, 1 = NOBITS |
| size | 4B | Byte count |
| data | size | Payload (PROGBITS only; NOBITS has no payload) |

PROGBITS records contain code/data to load at the specified address. NOBITS records represent zeroed BSS — the loader zeros `size` bytes at `org` without reading any payload.

TRB files are used for:
- Boot module server binaries (loaded by RS)
- Ramdisk-embedded executables (nsh, cat, echo, login)
- Any context where text TOF parsing overhead is undesirable
