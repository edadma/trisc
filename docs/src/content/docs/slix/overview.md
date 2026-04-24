---
title: SLIX Overview
description: A Minix 3-style microkernel OS for TRISC, x86_64, and aarch64.
---

SLIX is a microkernel operating system that runs on the TRISC emulator and under QEMU for x86_64 and aarch64 (virt machine, cortex-a72, GICv2). It follows the Minix 3 architecture: a small kernel handles scheduling and IPC, while system services run as isolated user-space processes.

## Architecture

```
┌──────────────────────────────────────────────────────┐
│ User programs: nsh (shell), cat, echo, login, ...    │
├──────────────────────────────────────────────────────┤
│ Servers: VFS │ PM │ TTY │ TFS │ Disk │ DS │ RS       │
├──────────────────────────────────────────────────────┤
│ Kernel: scheduler, IPC, page tables, syscalls        │
├──────────────────────────────────────────────────────┤
│ HAL: arch-specific (TRISC, x86_64, or aarch64)       │
└──────────────────────────────────────────────────────┘
```

The kernel is intentionally minimal. It provides:

- **Preemptive scheduling** with per-priority run queues
- **Synchronous IPC** (send/receive/reply) with async notifications
- **Page table management** for process isolation
- **Syscall dispatch** via trap 0

Everything else — filesystem, process management, terminal I/O, device drivers — runs in isolated server processes with their own address spaces.

## Boot sequence

1. CPU reads the vector table or arch entry stub (TRISC vector table; x86 long-mode jump; aarch64 EL1 exception vectors)
2. Boot assembly sets up exception handlers, page tables, and the timer
3. Kernel initializes the scheduler, IPC, and syscall table
4. RS (Restart Server) reads the boot-module header (`0x600000` on TRISC, `0x44000000` on aarch64, passed via Multiboot on x86)
5. RS loads servers as isolated processes: disk, tfs, tty, pm, vfs, ds, init
6. Servers register IPC ports and complete the handshake with RS
7. Init reads `/etc/ttytab` and spawns login on each terminal
8. Login authenticates against `/etc/shadow` (PBKDF2-SHA256)
9. Shell (nsh) starts

## Written in Sysl

The entire OS — kernel, servers, shell, utilities — is written in Sysl, a systems language that compiles to TRISC assembly and to x86_64 and aarch64 via LLVM. This means the same source code runs on the TRISC emulator and on both QEMU platforms without target-specific OS code.

## Three targets, one codebase

| Target | Boot | Kernel | Servers | Shell |
|--------|------|--------|---------|-------|
| TRISC emulator | boot.asm (TRISC) | oskit/kernel/ | oskit/servers/ | oskit/bin/ |
| x86_64 QEMU | boot.s (x86) | same kernel | same servers | same shell |
| aarch64 QEMU (virt, cortex-a72) | boot.s (aarch64) | same kernel | same servers | same shell |

Architecture-specific code lives in `oskit/arch/trisc/`, `oskit/arch/x86_64/`, and `oskit/arch/aarch64/`. The kernel, servers, and userspace are fully portable — the arch layer exposes a common interface (VM, page tables, exception frames, syscall entry) consumed by shared code.

## Key design decisions

- **No POSIX in the kernel.** The kernel and servers are idiomatic Sysl. POSIX is a separate compatibility layer for user programs.
- **Handles, not file descriptors.** Per-process handle tables with explicit passing at process creation.
- **Design for scale.** Kernel data structures use heaps, trees, and hash tables — never linear scans.
- **Crash recovery.** RS detects server crashes and transparently restarts them, transferring IPC port ownership.
