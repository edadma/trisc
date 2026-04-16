---
title: Interrupt Controller
description: 8-source priority interrupt controller.
---

The interrupt controller (INTC) aggregates up to 8 IRQ sources and signals the CPU when an enabled interrupt is pending. It uses a simple priority scheme — lowest IRQ number has highest priority.

**Base address:** `0x800080` | **Size:** 4 bytes

## Register map

| Offset | Size | Name | Access | Description |
|--------|------|------|--------|-------------|
| +0 | 1B | PENDING | R | Bitmask of pending interrupts |
| +1 | 1B | ENABLED | R/W | Bitmask of enabled interrupts (default: 0xFF) |
| +2 | 1B | CLAIM | R | Claim highest-priority pending interrupt |
| +3 | 1B | ACK | W | Acknowledge (clear) an interrupt |

## IRQ assignments

| IRQ | Default source |
|-----|---------------|
| 0 | Timer |
| 1 | Keyboard |
| 2 | Mouse |
| 3 | Ramdisk |
| 4 | DMA |
| 5 | (available) |
| 6 | (available) |
| 7 | IPI (inter-processor interrupt) |

## Operation

### Raising an interrupt

Devices call `intc.raise(irq)` when they need attention. This sets the corresponding bit in PENDING and signals the CPU via a volatile flag.

### Handling an interrupt

1. CPU enters interrupt handler (vector slot 2)
2. Handler reads **CLAIM** — returns the IRQ number of the highest-priority pending interrupt and clears it from PENDING
3. Handler services the interrupt
4. Handler writes the IRQ number to **ACK** to fully clear the interrupt
5. Handler returns via `rte`

If CLAIM returns `0xFF`, no interrupt is pending (spurious).

### Fast path

The INTC uses a volatile `irqSignal` flag for cross-thread communication. The CPU's tick callback checks this flag first — a single volatile read with no lock. Only when the flag is set does it enter the synchronized block to check pending/enabled/delivered state. This minimizes overhead in the common case (no interrupt).

## Multi-core

In SMP configurations, each core has its own INTC instance at a different address (base + coreId * 16). IRQ sources are wired to specific cores — typically core 0 handles all device interrupts, while the IPI device can target any core.
