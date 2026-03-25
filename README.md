# TRISC

![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/trisc_sjs1_3)
[![Last Commit](https://img.shields.io/github/last-commit/edadma/trisc)](https://github.com/edadma/trisc/commits)
![GitHub](https://img.shields.io/github/license/edadma/trisc)
![Scala Version](https://img.shields.io/badge/Scala-3.8.2-blue.svg)
![ScalaJS Version](https://img.shields.io/badge/Scala.js-1.20.2-blue.svg)
![Scala Native Version](https://img.shields.io/badge/Scala_Native-0.5.10-blue.svg)

A 16-bit RISC CPU emulator and assembler written in Scala 3.

TRISC has a fixed-width 16-bit instruction set with 8 general-purpose registers, memory-mapped I/O, interrupt handling, and a two-pass assembler with segments, symbols, and pseudo-instructions. It compiles to JVM, JavaScript (Scala.js), and native executables (Scala Native), and is suitable for educational use, hobby OS experiments, or as a platform for exploring CPU design.

## Architecture

- **Word size:** 16-bit instructions, 64-bit registers
- **Registers:** r0–r7 (r0 is hardwired to zero)
- **Endianness:** Big-endian
- **Memory:** Composable address space with RAM, ROM, and memory-mapped devices

### Instruction Set

All instructions are 16 bits wide, encoded in five formats:

#### RRR — Register-Register-Register

```
000 ddd aaa bbb oooo
```

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 0000 | ldb | rd = mem[ra + rb] (byte) |
| 0001 | stb | mem[rb + rc] = ra (byte) |
| 0010 | lds | rd = mem[ra + rb] (short) |
| 0011 | sts | mem[rb + rc] = ra (short) |
| 0100 | ldw | rd = mem[ra + rb] (word) |
| 0101 | stw | mem[rb + rc] = ra (word) |
| 0110 | ldd | rd = mem[ra + rb] (double) |
| 0111 | std | mem[rb + rc] = ra (double) |
| 1000 | add | rd = ra + rb |
| 1001 | sub | rd = ra - rb |
| 1010 | mul | rd = ra * rb |
| 1011 | div | rd = ra / rb |
| 1100 | rem | rd = ra % rb |
| 1101 | and | rd = ra & rb |
| 1110 | or | rd = ra \| rb |
| 1111 | xor | rd = ra ^ rb |

#### RRI — Register-Register-Immediate (branch/arithmetic)

```
010 aaa bbb iiiiiii    beq  — branch if ra == rb
011 aaa bbb iiiiiii    blu  — branch if ra < rb (unsigned)
100 aaa bbb iiiiiii    bls  — branch if ra < rb (signed)
101 aaa bbb iiiiiii    addi — ra = rb + sign_extend(imm7)
```

Branch offsets are in units of 2 bytes (halfwords), sign-extended from 7 bits.

#### RR — Register-Register

```
110 aaa bbb 00 ooooo   — 32 RR-format instructions
110 000 000 01 iiiii   trap  — supervisor call
110 aaa bbb 10 iiiii   ld    — ra = mem[rb + imm*2] (word, offset)
110 aaa bbb 11 iiiii   st    — mem[rb + imm*2] = ra (word, offset)
```

RR instructions include `jalr` (jump and link register), zero/sign extension (`zeb`, `zes`, `zew`, `seb`, `ses`, `sew`), `neg`, `not`, and floating-point conversions.

`jalr r0, r0` is decoded as `halt`.

#### RI — Register-Immediate

```
111 rrr oo iiiiiiii    (r != 0)
```

| Opcode | Mnemonic | Operation |
|--------|----------|-----------|
| 00 | ldi | rr = imm8 |
| 10 | sli | rr = (rr << 8) \| imm8 |
| 11 | sti | mem[rr] = imm8 (byte) |

#### R — Single Register

```
111 000 rrr ooooooo
```

Includes stack operations (`pshb`, `popb`, `pshs`, `pops`, `pshw`, `popw`, `pshd`, `popd`), status register access (`spsr`, `gpsr`), and return from exception (`rte`).

### Pseudo-Instructions

The assembler supports several pseudo-instructions that expand to real instructions:

| Pseudo | Expansion | Description |
|--------|-----------|-------------|
| `halt` | `jalr r0, r0` | Stop execution |
| `nop` | `addi r0, r0, 0` | No operation |
| `bra label` | `beq r0, r0, label` | Unconditional branch |
| `mov rd, rs` | `addi rd, rs, 0` | Copy register |
| `movi rd, imm` | `ldi` + `sli` sequence | Load wide immediate |

### Exception Vectors

The first 8 words of memory hold exception vectors:

| Vector | Address | Purpose |
|--------|---------|---------|
| 0 | 0x00 | Reset |
| 1 | 0x04 | Interrupt |
| 2 | 0x08 | (reserved) |
| 3 | 0x0C | Trap 0 |
| 4–7 | 0x10–0x1C | Trap 1–4 |

On reset or interrupt, registers are saved, the PC is loaded from the vector table, and execution continues in supervisor mode. `rte` restores registers and returns to the interrupted code.

### Memory-Mapped Devices

- **Stdout** — single byte write-only device for character output
- **Timer** — programmable interval timer with delay registers and interrupt generation
- **RTC** — real-time clock (read-only, BCD-encoded: second, minute, hour, day, month, day-of-week, year)

## Assembly Language

```asm
STDOUT = 0xFF8

dw reset          ; reset vector
dw 0              ; interrupt vector
dw 0              ; reserved
dw 0              ; trap 0 vector

reset
  ldi r1, 1       ; counter = 1
  movi r3, STDOUT
loop
  addi r4, r1, '0'  ; convert to ASCII
  stb r4, r3, r0    ; output character
  sti r3, '\n'       ; newline
  addi r1, r1, 1
  ldi r2, 5
  bls r2, r1, done   ; if 5 < counter, exit
  bra loop
done
  halt
```

### Directives

| Directive | Description |
|-----------|-------------|
| `db` *values* | Define bytes |
| `ds` *values* | Define shorts (2 bytes) |
| `dw` *values* | Define words (4 bytes) |
| `dl` *values* | Define longs (8 bytes) |
| `dd` *values* | Define doubles (8 bytes, float) |
| `resb` *n* | Reserve *n* bytes |
| `ress` *n* | Reserve *n* shorts |
| `resw` *n* | Reserve *n* words |
| `resl` *n* | Reserve *n* longs |
| `resd` *n* | Reserve *n* doubles |
| `segment` *name* | Switch to named segment |
| *name* `=` *expr* | Define constant (equate) |
| `.label` | Local label (scoped to preceding global label) |

### Segments

Code and data can be placed in named segments with explicit origins:

```asm
segment code
  ; ... code here ...

segment bss
buf resb 20
```

Origins are passed to the assembler:

```scala
assemble(source, orgs = Map("bss" -> 0x1000))
```

## Usage

### As a Library

```scala
import io.github.edadma.trisc._

val tof = assemble("""
  dw 8
  dw 0
  dw 0
  dw 0
  ldi r1, 42
  halt
""")

val mem = new Memory("mem", new RAM(0, 0x1000), new Stdout(0xFF8))
tof.load(mem)

val cpu = new CPU(mem, Nil)
cpu.reset()
cpu.run()

println(cpu.r(1).read) // 42
```

### TOF (TRISC Object Format)

The assembler produces a `TOF` object that can be serialized to a text format and deserialized later:

```
TOF v1
SEGMENT:_default_,0
DATA:0000000800000000...
```

## Building

Requires [sbt](https://www.scala-sbt.org/).

```
sbt compile    # compile
sbt test       # run tests
sbt run        # run CLI
```

## License

ISC
