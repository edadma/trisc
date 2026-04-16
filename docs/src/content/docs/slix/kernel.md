---
title: Kernel
description: SLIX kernel internals — scheduler, threads, processes.
---

## Scheduler

The SLIX scheduler is a preemptive priority scheduler with per-priority FIFO run queues, inspired by Minix 3.

### Priority levels

| Priority | Level | Typical use |
|----------|-------|-------------|
| 0 | Highest | Kernel threads, RS |
| 1 | High | System servers (disk, tfs, tty, pm, vfs) |
| 2 | Normal | User programs |
| 3 | Low | Background tasks |

Within the same priority, threads are scheduled round-robin with a configurable time quantum (default: 5 timer ticks).

### Ready mask

A bitmask tracks which priority levels have runnable threads. Finding the highest-priority non-empty queue is a single scan from bit 0 — O(1) scheduling.

### Sleep queue

Sleeping threads are stored in a min-heap ordered by wake tick, providing O(log n) insertion and O(1) peek for the next wakeup.

## Threads

Each thread has:

| Field | Description |
|-------|-------------|
| ssp | Saved supervisor stack pointer (at offset 0 — boot.asm depends on this) |
| state | READY, RUNNING, BLOCKED, TERMINATED, SUSPENDED, SEND_BLOCKED, RECV_BLOCKED |
| priority | Current priority level |
| quantum | Remaining ticks in this time slice |
| pid | Owning process ID |
| ptbr | Page table base register |
| notify_pending | Bitfield of pending async notifications |

Maximum 32 threads (`MAX_THREADS`). Thread stacks are statically allocated in BSS for kernel threads (8 slots), dynamically allocated for user threads.

## Processes

A process wraps one or more threads with an isolated address space:

| Field | Description |
|-------|-------------|
| state | FREE, RUNNING, ZOMBIE |
| ptbr | Page table base (physical address) |
| parent_tid | Thread that created this process |
| exit_code | Exit status (set on termination) |
| syscall_allow | Per-syscall privilege bitmask (Minix 3 style) |

Maximum 16 processes (`MAX_PROCESSES`). PID 0 is the kernel; all kernel threads belong to it.

## Syscall dispatch

All syscalls enter through `trap 0`. The syscall number is in r1, arguments on the stack. The kernel validates the syscall number against the process's `syscall_allow` bitmask before dispatching.

## Context switching

Context is saved/restored in boot assembly:

1. **Save**: Push all registers (PSR, PC, r1-r6, USP) onto supervisor stack
2. **Switch**: Update `current_thread`, load new thread's SSP
3. **Restore**: Pop all registers from new thread's supervisor stack, `rte`

The SSP is at offset 0 in the Thread struct so boot assembly can access it without knowing the struct layout.

## Watchdog

An optional watchdog kills threads that consume too many consecutive full quanta without yielding. This prevents infinite loops from starving the system.
