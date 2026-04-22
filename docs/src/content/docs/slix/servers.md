---
title: Servers
description: SLIX system servers — isolated processes providing OS services.
---

SLIX runs seven system servers as isolated processes, each with its own page table and address space. They communicate with the kernel and each other exclusively through IPC.

## Server overview

| Server | IPC Port | Role |
|--------|----------|------|
| **RS** | `rs` | Restart server / boot orchestration |
| **Disk** | `disk` | Block device driver |
| **TFS** | `tfs` | Filesystem (inode-based) |
| **TTY** | `tty` | Terminal input/output |
| **PM** | `pm` | Process management |
| **VFS** | `vfs` | Virtual filesystem layer |
| **DS** | `ds` | Data store (shared key-value config) |
| **init** | (none) | Reads `/etc/ttytab` and spawns login |

### RS (Restart Server)

RS runs as the first userspace process on every target. It:

- Reads the boot-module header (address is arch-specific: `0x600000` on TRISC, `0x44000000` on aarch64, and passed via Multiboot on x86)
- Loads each server binary into its own address space
- Manages handshake-based ordered boot
- Detects server crashes and transparently restarts them with IPC port transfer

## Disk server

Stateless block device driver. Receives read/write requests from TFS, programs the ramdisk registers, and returns data.

## TFS (TRISC Filesystem)

Inode-based filesystem providing:

- `open`, `read`, `write`, `close`
- `create`, `unlink`, `rename`
- `stat`, `chmod`
- `mkdir`, `readdir`
- Character devices (`/dev/tty0`, `/dev/null`) and block devices (`/dev/disk0`)

TFS stores metadata in inodes with direct and indirect block pointers. The block allocator uses a bitmap.

## TTY server

Terminal server handling:

- Keyboard input (scancode translation, line buffering)
- Console output (character-at-a-time to stdout device)
- Multiple virtual terminals via `/etc/ttytab`

## PM (Process Manager)

Process lifecycle management:

- `spawn` — create new process with handle inheritance
- `waitpid` — wait for child exit
- `kill` — terminate a process
- `exit` — process self-termination with exit code
- Orphan reparenting to init
- Zombie tracking for exit code retrieval

## VFS (Virtual Filesystem)

Abstraction layer between user programs and TFS:

- Open file table (32 entries) with reference counting
- Per-process handle tables (16 handles per process)
- Handle inheritance during `spawn`
- Mount table for future multi-filesystem support
- Pipe support for shell pipelines

## DS (Data Store)

Shared key-value configuration service modelled on Minix 3's DS:

- `publish` / `retrieve` / `delete` typed entries (int or string)
- `subscribe` to a key prefix for change notifications
- Fixed-size table (64 entries) — no heap allocation
- Used for cross-server configuration that isn't a good fit for the filesystem (e.g., future network MTU, DHCP-learned addresses)

## Boot module system

Servers are compiled as standalone binaries (`.trb` on TRISC, ELF on x86_64, flat on aarch64) and loaded as boot modules:

```
Boot info header:
  +0: "SLIX" magic (4 bytes)
  +4: module count (4 bytes)
  +8: per module (24 bytes each):
    +0: name (8 bytes, NUL-padded)
    +8: offset / load address (8 bytes)
   +16: size (8 bytes)
```

The header lives at a known physical address per target: `0x600000` on TRISC, `0x44000000` on aarch64 (loaded via QEMU `-device loader`), or passed as a Multiboot module on x86. RS converts module offsets to absolute physical addresses before loading.

Each server links at a fixed virtual address for its target (`0xD0000` on TRISC/x86, `0x60000000` on aarch64) — since each server gets its own page table, the same virtual address maps to different physical memory. This simplifies linking while maintaining full isolation.

## Isolation guarantees

- Servers cannot read/write each other's memory
- A server crash does not bring down the kernel
- RS can restart a crashed server and transfer its IPC port to the new instance
- Syscall privilege masks limit what each server can do
