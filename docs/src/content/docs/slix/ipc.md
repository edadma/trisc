---
title: IPC
description: Inter-process communication in SLIX.
---

SLIX uses synchronous message passing for IPC, following the Minix 3 model. Servers register named ports; clients look up ports by name and exchange messages.

## Message model

| Operation | Description |
|-----------|-------------|
| `send(port, msg, len)` | Send message to port, block until received |
| `recv(port, buf, maxlen)` | Block until a message arrives on port |
| `reply(tid, msg, len)` | Reply to a specific thread |
| `notify(tid, bits)` | Async notification (non-blocking) |

Messages are copied between address spaces via the kernel. When sender and receiver are in different page tables, the kernel uses a bounce buffer with `vm_copy_from` / `vm_copy_to`.

## Ports

Each server registers a named port (up to 8 ports total):

```c
// Server registers a port
ipc_port_register(port_id, "vfs")

// Client looks up port by name
val vfs_port = ipc_port_lookup("vfs")
ipc_send(vfs_port, &msg, sizeof(msg))
```

Port names are copied into kernel-owned storage during registration — the registering thread's pointer may be invalid in a different address space.

## Async notifications

Notifications are lightweight, non-blocking signals. Each thread has a `notify_pending` bitfield. The kernel ORs in new bits without blocking the sender. The receiver can check for pending notifications during a receive call.

## Cross-address-space IPC

When sender and receiver have different page table base registers (PTBRs), the kernel cannot simply memcpy between them. Instead:

1. **Send path**: `vm_copy_from(sender_ptbr, src_vaddr, kernel_buf, len)` copies the message into a kernel bounce buffer
2. **Deliver path**: `vm_copy_to(receiver_ptbr, kernel_buf, dst_vaddr, len)` copies from the bounce buffer into the receiver's address space

Reply uses the same mechanism in reverse.

## Syscall enforcement

Each process has a per-syscall privilege bitmask (`syscall_allow_lo` / `syscall_allow_hi`). The kernel checks this bitmask before dispatching any syscall. Servers get broad permissions; user programs get restricted access. This prevents a compromised user process from directly calling kernel internals.
