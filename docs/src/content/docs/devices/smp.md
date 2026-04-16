---
title: Multi-Core & IPI
description: Symmetric multi-processing support and inter-processor interrupts.
---

TRISC supports symmetric multi-processing (SMP) with up to 8 cores. Each core is a full CPU instance sharing the same memory, with its own register file, MMU, and interrupt controller.

## Multi-core architecture

| Component | Shared | Per-core |
|-----------|--------|----------|
| RAM | Yes | -- |
| Devices | Yes | -- |
| Register file | -- | Yes |
| MMU (page tables) | -- | Yes |
| Interrupt controller | -- | Yes |
| IPI device | -- | Yes |
| Tick sequence | -- | Yes |

### Shared state synchronization

The **ReservationMonitor** provides cross-core synchronization for LL/SC and CAS:

- **LL/SC**: When core A writes to an address that core B has a reservation on, core B's reservation is invalidated
- **CAS**: Atomic compare-and-swap is synchronized across all cores via a shared monitor

All store instructions call `notifyWrite()` to invalidate other cores' reservations.

## IPI device

Each core has its own IPI (Inter-Processor Interrupt) device instance, allowing any core to interrupt any other core.

**Base address:** `0x800300 + coreId * 16` | **Size:** 16 bytes per core

### Register map

| Offset | Size | Name | Access | Description |
|--------|------|------|--------|-------------|
| +0 | 4B | TARGET | R/W | Destination core ID |
| +4 | 4B | VECTOR | R/W | Interrupt vector (reserved) |
| +8 | 1B | COMMAND | W | Write 1 to send IPI |
| +9 | 1B | STATUS | R | 1 = success, 2 = invalid target |
| +12 | 4B | SELF_ID | R | This core's ID (read-only) |

### Sending an IPI

1. Write the target core ID to **TARGET**
2. Write 1 to **COMMAND**
3. The target core's interrupt controller receives IRQ 7

### Reading core ID

Each core reads its own ID from its IPI device's SELF_ID register:

```c
// Sysl
core_id() -> int
    val addr = 0x800300 + 12  // IPI_BASE + SELF_ID offset
    *(*int(addr))
```

## Spinlocks

CAS-based spinlocks are provided for protecting shared kernel data structures:

```c
// Sysl — arch-specific (TRISC)
arch_spinlock_acquire(lock: *i64)
    // CAS loop: while CAS(lock, 0, 1) fails, retry
    // Then disable interrupts
    asm("ldi r3, 1\n_spin_acq\nldi r2, 0\ncas r2, r1, r3\nbne r2, r0, _spin_acq\ncli")

arch_spinlock_release(lock: *i64)
    // Enable interrupts, then store 0
    asm("sti\nstd r0, r1, r0")
```

The acquire/release pair brackets a critical section: interrupts are disabled while the lock is held to prevent deadlock from nested interrupt handlers.

## Running multi-core

Use the `--smp` flag:

```bash
trisc run --smp 4 --gui program.tof
```

Core 0 receives the timer tick and handles all device interrupts. Other cores run with only their interrupt controller in the tick sequence, waking on IPI.
