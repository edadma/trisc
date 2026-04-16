---
title: Memory Model
description: TRISC memory organization, addressing, and alignment.
---

## Address space

TRISC uses a flat 64-bit address space. In practice, the emulator provides 8 MB of RAM with devices mapped above:

```
0x000000  ┌─────────────────────┐
          │ Vector table (160B) │
          │ Code + data + BSS   │
          │         ...         │
          │ Heap (sbrk ↑)       │
          ├─────────────────────┤
0x7FE000  │ Kernel page table   │
0x7FFFF8  │ Initial SSP         │
          ├─────────────────────┤
0x800000  │ Devices (I/O)       │
          ├─────────────────────┤
0x900000  │ Framebuffer         │
          └─────────────────────┘
```

## Byte ordering

TRISC is **big-endian**. A 64-bit value `0x0102030405060708` stored at address A appears in memory as:

| Address | A+0 | A+1 | A+2 | A+3 | A+4 | A+5 | A+6 | A+7 |
|---------|-----|-----|-----|-----|-----|-----|-----|-----|
| Byte    | 01  | 02  | 03  | 04  | 05  | 06  | 07  | 08  |

## Alignment

Unaligned accesses trigger a **MisalignedAccess** exception:

| Access size | Required alignment |
|-------------|-------------------|
| Byte (1B) | None |
| Short (2B) | 2-byte aligned |
| Word (4B) | 4-byte aligned |
| Double (8B) | 8-byte aligned |

The stack pointer (r7) must always be 8-byte aligned.

## Memory-mapped I/O

There are no special I/O instructions. All device interaction is through load/store to device addresses. The composable `Memory` class routes accesses to the correct device based on address ranges.

See [Device Map](/devices/device-map/) for the complete address layout.

## Memory protection

TRISC supports two memory protection mechanisms (both optional):

### MPU (Memory Protection Unit)

Simple flat-address region-based protection, similar to ARM Cortex-M. Eight configurable regions with per-privilege read/write/execute permissions. Higher-numbered regions override lower ones.

### MMU (Memory Management Unit)

Hardware page table walk with TLB, modeled after RISC-V Sv32:

- Two-level page tables (L1 directory + L2 page table)
- 4 KB pages
- Per-page read/write/execute/valid bits
- Superpage support (4 MB, single L1 entry)
- ASID-tagged TLB entries
- Configurable identity-mapped range for kernel addresses

The MMU translates virtual addresses to physical before every memory access. On a page fault, the CPU saves the faulting address in the FAULTADDR register and enters the DataAccess exception handler.

## Atomics

TRISC provides two atomic memory access mechanisms:

### LL/SC (Load-Linked / Store-Conditional)

- `ll rd, rs` — Load 8 bytes from [rs] into rd, set reservation
- `sc rd, rs` — Store rd to [rs] if reservation still valid; rd = 1 on success, 0 on failure

The reservation is cleared by any intervening store to the same address (including from other cores in SMP mode) or by an exception.

### CAS (Compare-and-Swap)

- `cas rd, ra, rb` — Atomically: if mem[ra] == rd, write rb to mem[ra]; rd = old value

CAS is a single instruction, inherently atomic. In multi-core mode, CAS is synchronized across all cores via the reservation monitor.
