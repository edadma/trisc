---
title: MMU
description: Hardware page table walk with TLB, modeled after RISC-V Sv32.
---

The MMU provides virtual-to-physical address translation with a hardware page table walk, TLB caching, and ASID tagging. It is modeled after RISC-V Sv32 but adapted for TRISC's 64-bit addresses.

## Page table structure

Two-level page tables with 4 KB pages:

```
Virtual address:  [L1 index (10 bits)] [L2 index (10 bits)] [page offset (12 bits)]

L1 table (1024 entries × 8 bytes = 8 KB) → pointed to by PTBR
  └─ each entry points to an L2 table (or is a superpage)
     └─ L2 table (1024 entries × 8 bytes = 8 KB)
        └─ each entry maps one 4 KB page
```

### Page table entry format

| Bits | Field | Description |
|------|-------|-------------|
| 0 | V | Valid |
| 1 | R | Readable |
| 2 | W | Writable |
| 3 | X | Executable |
| 12+ | PPN | Physical page number |

## Superpages

An L1 entry with R, W, or X bits set (and V=1) is a **superpage** — it maps 4 MB directly without an L2 table. This is used for kernel identity mapping.

## TLB

The TLB caches recent translations to avoid page table walks on every access:

- Fully associative, 64 entries (configurable)
- ASID-tagged — entries from different address spaces coexist
- **Last-translation cache** — single-entry fast path for sequential accesses to the same page
- Invalidation: `tlbi` (single entry by virtual address) or `tlbia` (flush all)

## Identity range

The MMU supports a configurable **identity-mapped range** where virtual addresses equal physical addresses, bypassing the page table walk entirely. This is used for kernel code and the page table region itself:

```scala
mmu.setIdentityRange(0x7FE000L, 0xC00000L)  // kernel PTBR through devices
```

## Control registers

| Instruction | Register | Description |
|-------------|----------|-------------|
| `sptbr rs` | PTBR | Set page table base register (physical address of L1 table) |
| `gptbr rd` | PTBR | Get page table base register |
| `sasid rs` | ASID | Set address space identifier |
| `gasid rd` | ASID | Get address space identifier |
| `gfault rd` | FAULTADDR | Get faulting virtual address (last page fault) |
| `gfcause rd` | FAULTCAUSE | Get fault cause code |
| `tlbi ra, rb` | -- | Invalidate TLB entry for virtual address |
| `tlbia ra, rb` | -- | Invalidate all TLB entries |

## Fault causes

When a page fault occurs, the CPU enters the DataAccess exception handler. The fault cause is available via `gfcause`:

| Code | Cause |
|------|-------|
| 0 | None |
| 1 | Page not present (V=0) |
| 2 | Permission denied (R/W/X check failed) |
| 3 | Superpage misaligned |

## DMA and MMU

The DMA controller can operate in two modes:
- **Virtual mode** (default) — DMA addresses go through the MMU
- **Physical mode** (PHYS_MODE bit) — DMA bypasses the MMU, using physical addresses directly
