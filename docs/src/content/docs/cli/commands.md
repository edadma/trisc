---
title: CLI Commands
description: TRISC command-line tool reference.
---

The `trisc` CLI provides commands for assembling, linking, running, and disassembling TRISC programs.

## `trisc run`

Load and execute a TOF file.

```bash
trisc run [options] <file.tof>
```

| Option | Description |
|--------|-------------|
| `--mem SIZE` | Memory size in bytes (default 65536) |
| `--limit N` | Instruction execution limit (0 = unlimited) |
| `--trace` | Trace instruction execution to stderr |
| `--gui` | Open GUI emulator window (JVM only) |
| `--smp N` | Number of CPU cores, 1-8 (default 1) |

### Headless mode

```bash
trisc run program.tof
```

Runs to completion, prints to stdout. Exit code in r1 is reported to stderr if nonzero.

### GUI mode

```bash
trisc run --gui program.tof
```

Opens a Swing window with:
- Terminal display (ANSI escape support)
- Framebuffer display (switchable)
- Run / Step / Reset buttons
- Status bar (PC, r1, CPU state, display mode)
- Keyboard and mouse input

### Multi-core mode

```bash
trisc run --smp 4 --gui program.tof
```

Creates 4 CPU cores with per-core interrupt controllers and IPI devices. Core 0 handles the timer and all device interrupts.

## `trisc asm`

Assemble a `.asm` file to a relocatable TOF.

```bash
trisc asm [options] <file.asm>
```

| Option | Description |
|--------|-------------|
| `-o FILE` | Output TOF file (default: input with `.tof` extension) |

## `trisc link`

Link one or more TOF files into an executable.

```bash
trisc link [options] <file.tof>...
```

| Option | Description |
|--------|-------------|
| `-o FILE` | Output file (default: `out.tof`) |
| `-s FILE` | Linker script file |

The linker resolves symbols, merges segments, and sets the entry point. The runtime boot module (vector table + I/O stubs) is automatically included when running.

## `trisc disasm`

Disassemble a TOF file.

```bash
trisc disasm [options] <file.tof>
```

| Option | Description |
|--------|-------------|
| `--from HEX` | Start address (hex) |
| `--to HEX` | End address (hex, exclusive) |

Without `--from`/`--to`, disassembles all segments. With `--from` only, disassembles 256 bytes starting at that address.
