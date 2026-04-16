---
title: Architecture Overview
description: High-level view of the TRISC CPU architecture.
---

TRISC is a load-store RISC architecture with 16-bit fixed-width instructions and 64-bit registers. It draws inspiration from several real architectures:

| Influence | What TRISC borrows |
|-----------|-------------------|
| RISC-V | Zero register (r0), load-store design, Sv32-style MMU |
| Motorola 68000 | Vector table exception model, dual stack pointers (USP/SSP) |
| ARM Cortex-M | Memory-mapped peripherals, MPU region model |
| RP2040 | DMA controller, GPIO design |
| STM32 | Timer with prescaler and capture/compare channels |

## Key properties

- **16-bit instructions** — every instruction is exactly 2 bytes, simplifying fetch and decode
- **64-bit registers** — 8 general-purpose registers (r0 hardwired to zero)
- **Big-endian** byte ordering
- **Dual stack pointers** — user (USP) and supervisor (SSP), automatically swapped on exception entry/exit
- **Memory-mapped I/O** — no special I/O instructions; devices are addressable like RAM
- **5 instruction formats** — RRR, RRI, RR, RI, R

## Processor modes

TRISC has two privilege levels, controlled by the Mode bit in the PSR:

| Mode | PSR.Mode | Stack | Description |
|------|----------|-------|-------------|
| User | 0 | USP (r7) | Application code, restricted access |
| Supervisor | 1 | SSP (r7) | Kernel code, full access to all instructions |

Privileged instructions (`cli`, `sti`, `rte`, `spsr`, `susp`, MMU instructions) are only available in supervisor mode. Attempting them in user mode triggers a PrivilegeViolation exception.

## Instruction encoding

All instructions are 16 bits. The top 3 bits select the format:

| Bits [15:13] | Format | Description |
|-------------|--------|-------------|
| `000` | RRR block 0 | Load/store, arithmetic (3 registers + 4-bit opcode) |
| `001` | RRR block 1 | Shifts, compare, unsigned, float (3 registers + 4-bit opcode) |
| `010`-`101` | RRI | Branch and add-immediate (2 registers + 7-bit immediate) |
| `110` | RR | Two-register ops, load/store with offset |
| `111` | RI / R | Immediate load, stack ops, system instructions |

See [Instruction Set](/architecture/instruction-set/) for the complete encoding tables.
