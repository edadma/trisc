# Slix 0.0.1 Spec Analysis & Rework Plan

## What Works Well

- **Explicit handle table in spawn** — no implicit inheritance, genuinely modern, better than fork+exec
- **Handles-as-capabilities** — possessing a handle confers authority; the right model for a modern OS
- **Dual desktop/embedded profiles** with the same logical model — smart, allows single-binary embedded and multi-process desktop from the same API
- **Clean, small API surface** — 13 syscalls is manageable
- **Service-owns-resources invariant** — the kernel never interprets resource-specific semantics; all behavior lives in services
- **Scheme-based connect()** — clean resource naming (`vfs:/path`, `tty:0`, `dev:disk0`)

## Serious Gaps

### 1. IPC Is Informative, Not Normative (Critical)

The spec defines the userspace API (read/write/connect/spawn) but treats message passing as "informative" (Section 13). In a microkernel, **message passing IS the kernel**. MINIX 3's core primitives are `send`/`receive`/`sendrec`. L4's entire kernel is IPC + scheduling + address spaces.

The current high-level calls (read, write, connect) should be **built on top of** normative IPC primitives, not defined alongside hand-waved IPC. Without normative IPC:

- You can't build new services that communicate with each other
- You can't implement the described service/endpoint model
- The kernel/service boundary is undefined
- There's no way for userspace to implement a new resource type

**Recommendation:** Define kernel-level IPC as the foundation. At minimum:
- `send(endpoint, msg)` — synchronous send (blocks until received)
- `receive(endpoint, msg)` — synchronous receive (blocks until message arrives)
- `sendrec(endpoint, msg)` — send and wait for reply (the common RPC pattern)
- `notify(endpoint, bits)` — lightweight async notification (doesn't block, doesn't transfer data, just sets event bits)

Then define read/write/connect/etc. as library functions that construct messages and call `sendrec()` to the appropriate service. This is exactly how MINIX 3 works.

### 2. No Handle Passing Between Running Processes (Critical)

Handles can be passed at `spawn()` time, but after that there's no mechanism to transfer handles between running processes. A real capability system needs this. Without it:

- A shell can't open a file and hand it to a running child
- A window server can't hand a framebuffer handle to a client that connects later
- Services can't delegate sub-resources to other services
- You can't implement a capability-passing protocol

**Recommendation:** Add handle transfer as part of the IPC message model. A message can carry one or more handles, which are removed from the sender's handle table and inserted into the receiver's. This is how Mach ports, seL4 capabilities, and Fuchsia handles work.

### 3. No Memory Management (Critical for Desktop)

No mmap, no shared memory, no memory mapping of any kind. This blocks:

- Zero-copy I/O (must copy everything through kernel buffers)
- Shared libraries
- Memory-mapped files
- Efficient large data transfer between processes
- Graphics framebuffers

**Recommendation (Desktop Profile):** Define at minimum:
- `map(handle, offset, len, flags) -> addr` — map a resource into address space
- `unmap(addr, len)` — remove mapping
- `grant(addr, len, target_endpoint)` — share a memory region with another process (one-way, explicit)

The embedded profile can skip this entirely (single address space).

### 4. No Async Notification or Signals (Important)

How do you:
- Interrupt a blocked process?
- Tell a parent its child crashed without calling wait()?
- Notify a process of an external event (window resize, network packet)?

`poll()` only covers I/O readiness on existing handles. There's no mechanism for asynchronous system events.

**Recommendation:** The `notify(endpoint, bits)` primitive from the IPC section solves this. Notifications are async (sender doesn't block), lightweight (just a bitmask, no data), and composable (bits OR together if multiple arrive before the receiver checks). The receiver can `poll()` for notifications alongside I/O handles. This is exactly seL4's notification model and it's elegant.

### 5. No Control Operation (Important)

`stat()` is read-only metadata. There's no way to:
- Put a TTY in raw mode
- Set baud rate on a serial device
- Enable non-blocking mode on a handle
- Query or configure service-specific behavior

**Recommendation:** Add a generic control call:
```c
int control(handle_t h, uint32_t op, void* in, size_t in_len, void* out, size_t out_len);
```

Services define their own `op` codes. The kernel just routes the message. This is the ioctl model but with explicit in/out buffers instead of the type-unsafe ioctl varargs mess.

### 6. No Threading Model (Important)

Not even mentioned. A modern OS needs to take a position:

- **Option A: Threads are first-class.** Add `thread_create`, synchronization primitives, shared memory within a process. This is what most modern OSes do.
- **Option B: Single-threaded processes only.** Use message passing for concurrency. This is the Erlang/MINIX philosophy — simpler, fewer bugs, but makes some patterns harder.
- **Option C: Both, via profiles.** Desktop profile supports threads. Embedded profile is single-threaded.

**Recommendation:** Option C. The embedded profile already has threads (TOS has them). Desktop profile should support threads within a process, with the caveat that inter-process communication still uses IPC, not shared memory hacks.

### 7. Async I/O Is Poll-Only (Nice to Have)

`poll()` is select-era (1983) technology. Modern systems use completion-based async:
- Linux: io_uring
- macOS: kqueue
- Windows: IOCP

These are fundamentally different: instead of "tell me what's ready, then I'll call read", it's "here are 10 reads I want done, tell me when each completes."

**Recommendation:** This is a v0.1 or v0.2 concern, not v0.0.1. `poll()` is sufficient for initial implementation. But design the IPC layer so that completion-based async can be layered on later without breaking changes.

## Structural Concern: Spec Covers Wrong Layer

The spec reads like a **userspace API spec** rather than an **OS architecture spec**. It defines what applications see, but not what the kernel provides or how services are structured.

For a microkernel, you need three layers:

### Layer 1: Kernel Primitives (the microkernel itself)
- IPC: send, receive, sendrec, notify
- Scheduling: thread/process management, priorities
- Memory: address spaces, mapping, grants
- Capability transfer: handle passing via IPC

### Layer 2: System Services (userspace servers built on kernel IPC)
- VFS service: files, directories, mount points
- TTY service: terminal devices
- Device services: hardware drivers
- Process service: spawn, wait, process lifecycle
- Network service (desktop profile)

### Layer 3: Userspace API (library that talks to services via IPC)
- read() → sendrec to VFS/TTY/device service
- write() → sendrec to VFS/TTY/device service
- connect() → sendrec to registry, then to target service
- spawn() → sendrec to process service
- etc.

The current spec only defines Layer 3. **The reworked spec should define all three layers**, with Layer 1 being normative, Layer 2 defining required services, and Layer 3 being the public API.

## Rework Plan

### Phase 1: Kernel Primitives
1. Define IPC primitives (send/receive/sendrec/notify)
2. Define message structure (including handle transfer slots)
3. Define scheduling model (priority-based, preemptive, with priority inheritance)
4. Define memory model for desktop profile (address spaces, map/unmap/grant)
5. Define the handle/endpoint/capability model at the kernel level

### Phase 2: System Services
1. Define the service lifecycle (registration, scheme binding)
2. Define required services per profile (VFS, TTY, device, process)
3. Define the service protocol (message types each service must handle)
4. Define the registry/naming service

### Phase 3: Userspace API
1. Keep most of the current spec's API (read, write, close, seek, poll, spawn, wait, connect, etc.)
2. Add control() for device/service configuration
3. Add handle transfer functions
4. Define these as library wrappers over IPC, not direct kernel calls
5. Add threading API (desktop profile)

### Phase 4: Profiles
1. Desktop: full multi-process, VFS, networking, threads, memory mapping
2. Embedded: single address space, direct-call IPC, static service table, threads (from TOS)

## Scheduler-Specific Notes

The current spec says nothing about scheduling. The reworked spec needs a scheduler section. Based on analysis of the TOS scheduler and what's needed for Slix:

### What TOS Has (Embedded Baseline)
- 4-level priority with per-priority FIFO queues and ready bitmask
- Timer-driven preemption with configurable quantum
- Comprehensive sync primitives (mutex, semaphore, rwlock, condvar, barrier, channel, mailbox)
- Watchdog for runaway threads
- Notification/event system

### What TOS Needs First (Backport to Embedded)
These improvements are being added to TOS and will form the embedded scheduler baseline:

1. **Priority inheritance on mutexes** — prevents priority inversion (Mars Pathfinder bug). When a high-priority thread blocks on a mutex held by a low-priority thread, the holder is temporarily boosted. Critical for any system with priorities + mutexes.

2. **Proper blocking queues** — replace spin+yield with sleep queues on mutexes/semaphores. Threads blocked on a lock go into a per-lock wait queue and are woken when the lock is released. No wasted cycles.

3. **O(log n) or O(1) timer management** — replace linear scan of all threads every tick with a timer wheel or min-heap. Matters as thread count grows.

4. **Dynamic quantum sizing** — allow different quanta for different priority levels or task types (short for interactive, longer for batch).

### What Slix Desktop Adds On Top
5. **Multi-level feedback queue (MLFQ)** — dynamically adjust priority based on behavior. Threads that use their full quantum get demoted. Threads that block before quantum expires get boosted. This automatically prioritizes interactive tasks over CPU-bound ones without manual priority assignment.

6. **SMP support** — per-CPU run queues, load balancing, cache affinity. Design for this from day one even if initial implementation is single-core.

7. **Process-level scheduling** — schedule threads, but track and account for CPU usage at the process level. Fair-share scheduling between processes.

8. **Deadline scheduling (optional)** — for real-time tasks that need guaranteed completion times. Can coexist with MLFQ for normal tasks.

## Implementation Order

1. **NOW: Add priority inheritance to TOS** — highest value, correctness fix, well-scoped
2. Add blocking queues to TOS
3. Improve TOS timer management
4. Add dynamic quantum to TOS
5. Write Slix spec v0.1 with all three layers defined
6. Port TOS scheduler to Slix as embedded baseline
7. Add MLFQ and SMP for desktop profile

## References

- MINIX 3 scheduler: priority-based, similar to TOS but with dynamic adjustment
- seL4: capability-based microkernel with formal verification; good model for handle/IPC design
- L4: minimalist microkernel, IPC-focused; demonstrates that a microkernel can be fast
- Mars Pathfinder priority inversion: https://www.cs.unc.edu/~anderson/teach/comp790/papers/mars_pathfinder_long_version.html
- Fuchsia (Zircon kernel): modern microkernel with handles, channels, and capability transfer — closest modern analog to what Slix is trying to be
