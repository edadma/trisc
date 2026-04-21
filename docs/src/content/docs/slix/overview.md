---
title: SLIX Overview
description: A Minix 3-style microkernel OS for TRISC.
---

SLIX is a microkernel operating system that runs on the TRISC emulator and on real x86_64 hardware (via QEMU). It follows the Minix 3 architecture: a small kernel handles scheduling and IPC, while system services run as isolated user-space processes.

## Architecture

```
┌──────────────────────────────────────────────────┐
│ User programs: nsh (shell), cat, echo, login     │
├──────────────────────────────────────────────────┤
│ Servers: VFS │ PM │ TTY │ TFS │ Disk             │
├──────────────────────────────────────────────────┤
│ Kernel: scheduler, IPC, page tables, syscalls    │
├──────────────────────────────────────────────────┤
│ HAL: arch-specific (TRISC or x86_64)             │
└──────────────────────────────────────────────────┘
```

The kernel is intentionally minimal. It provides:

- **Preemptive scheduling** with per-priority run queues
- **Synchronous IPC** (send/receive/reply) with async notifications
- **Page table management** for process isolation
- **Syscall dispatch** via trap 0

Everything else — filesystem, process management, terminal I/O, device drivers — runs in isolated server processes with their own address spaces.

## Boot sequence

1. CPU reads vector table: SSP from slot 0, PC from slot 1
2. Boot assembly sets up exception handlers and timer
3. Kernel initializes scheduler, IPC, page tables
4. RS (Restart Server) reads boot module info at `0x600000`
5. RS loads servers as isolated processes: disk, tfs, tty, pm, vfs
6. Servers register IPC ports and complete handshake with RS
7. Init reads `/etc/ttytab` and spawns login on each terminal
8. Login authenticates against `/etc/shadow` (PBKDF2-SHA256)
9. Shell (nsh) starts

## Written in Sysl

The entire OS — kernel, servers, shell, utilities — is written in Sysl, a systems language that compiles to both TRISC assembly and x86_64 via LLVM. This means the same source code runs on the TRISC emulator and on real hardware.

## Two targets, one codebase

| Target | Boot | Kernel | Servers | Shell |
|--------|------|--------|---------|-------|
| TRISC emulator | boot.asm (TRISC) | oskit/kernel/ | oskit/servers/ | oskit/bin/ |
| x86_64 QEMU | boot.s (x86) | same kernel | same servers | same shell |

Architecture-specific code lives in `oskit/arch/trisc/` and `oskit/arch/x86_64/`. The kernel, servers, and userspace are fully portable.

## Key design decisions

- **No POSIX in the kernel.** The kernel and servers are idiomatic Sysl. POSIX is a separate compatibility layer for user programs.
- **Handles, not file descriptors.** Per-process handle tables with explicit passing at process creation.
- **Design for scale.** Kernel data structures use heaps, trees, and hash tables — never linear scans.
- **Crash recovery.** RS detects server crashes and transparently restarts them, transferring IPC port ownership.
