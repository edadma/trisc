# SLIX Development Roadmap

Concrete, ordered development plan. Each phase builds on the previous.

## Design Decisions (settled)

- **Handles, not file descriptors.** Per-process opaque references to resources, with rights bitmask. No global meaning.
- **spawn(), not fork+exec.** Explicit process creation with explicit handle passing. No implicit inheritance.
- **connect(), not open().** Scheme-based resource naming: `vfs:/etc/passwd`, `tty:0`, `dev:disk0`. Extensible.
- **IPC is the kernel.** Synchronous send/recv/reply + async notifications. Already built.
- **POSIX is a layer.** posix_spawn->spawn, fd->handle, open->connect, fork->COW-spawn (optional). Not in the kernel.
- **sysl, not C.** Kernel, servers, and userspace all in sysl. Boot trampoline is the only assembly.
- **HAL for portability.** Hardware-specific code (memcpy, timers, UART) lives in swappable HAL modules. Same interface, different implementations per target. Kernel/servers never import hardware-specific code directly.
- **TOML build config.** A `sysl.toml` selects target, HAL modules, servers, and compile-time constants. No conditional compilation in the language -- the build system generates a `config` module and controls which files are linked.
- **Minix 3 architecture.** Microkernel with isolated servers, IPC-based communication, RS-managed boot and recovery.

---

## Completed

### Phase 0a-0b: Foundation
- HAL extraction (memcpy/memset via DMA or CPU loop)
- sysl.toml config module generation
- Build tool assembles source list from sysl.toml
- FS client library separated from TFS server

### Phase 0c-0d: Filesystem layer
- VFS server with namespace and mount table
- Open file state tracking in VFS (position, refcount)

### Phase 1: Per-process handle table
- Handles 0/1/2 for stdin/stdout/stderr
- VFS maintains per-process handle tables indexed by PID

### Phase 2: spawn() with argv and handles
- PM-based spawn with argv string and handle map
- PM loads binary, sets up args, tells VFS to copy handles

### Phase 3a-3c: TTY scheme, handle inheritance, stdio
- connect("tty:N") returns TTY-backed handle
- Handle inheritance via PM_SPAWN + VFS_CMD_INHERIT
- Default stdin/stdout/stderr for all external programs

### Server Isolation (Minix 3 architecture)
- Boot module system: emulator loads .trb binaries, boot info header at 0x600000
- RS reads boot info, loads servers as isolated processes with own page tables
- All 5 servers isolated: disk, tty, tfs, vfs, pm
- Cross-address-space IPC (vm_copy_to/from with bounce buffers)
- PM loads binaries via cross-address-space copy (load_tof_to_ptbr)
- 17 new kernel syscalls for server isolation (44-66)

---

## Phase 4: Multi-Target Architecture

**Problem:** All arch-specific code (TRISC assembly, Sv32 page tables, MMIO device addresses, DMA, cli/sti) is mixed into kernel.lsysl and boot.asm. Porting to x86_64 means rewriting large portions of the kernel.

**Goal:** Factor arch-specific code into `oskit/arch/{target}/` modules behind stable interfaces. The kernel imports arch functions; the build system selects the target. Same pattern as Linux's `arch/` directory.

### What's arch-specific (must be in arch/)

| Component | TRISC | x86_64 |
|-----------|-------|--------|
| Boot entry | boot.asm (trap vectors, exception table) | boot.S (multiboot header, GDT, IDT) |
| Syscall entry | `trap 0` dispatch table | `syscall` instruction via MSR |
| Context switch | pshr/popr, susp/gusp, sptbr, rte | push/pop regs, swapgs, iretq |
| Page tables | Sv32 2-level (10+10+12 bits) | x86_64 4-level (9+9+9+9+12 bits) |
| PTBR management | gptbr/sptbr instructions | mov cr3 |
| TLB flush | tlbia instruction | invlpg / mov cr3 |
| Interrupts | cli/sti instructions | cli/sti (same mnemonics, different encoding) |
| Atomics | ll/sc (load-linked/store-conditional) | lock cmpxchg |
| Timer | MMIO timer device (0x800100) | PIT or LAPIC timer |
| Interrupt controller | MMIO INTC (0x800080) | PIC/IOAPIC/LAPIC |
| DMA/bulk copy | DMA controller (0x8001C0) | rep movsb (CPU) |
| Keyboard | MMIO keyboard (0x800004) | PS/2 via port I/O |
| Display | MMIO display (0x8000A0) | VGA framebuffer or serial |
| Memory layout | KERNEL_L1_BASE=0x7FE000, pools at 0x660000 | Higher-half kernel, pools in physical memory |

### What's arch-independent (stays in kernel/)

- Scheduler (priority queues, round-robin, time quantum, sleep queue)
- IPC (port management, message delivery, notifications, grants)
- Process/thread management (create, kill, reap, waitpid, zombie/orphan)
- Page allocator algorithm (bump allocator, free list)
- Thread state machine (ready, blocked, suspended, terminated)
- All 5 servers (pure IPC, fully portable)
- Filesystem (TFS, VFS routing)
- Shell and user programs

### Arch interface contract

Each `oskit/arch/{target}/` must export these functions:

**VM subsystem (`arch/*/vm.lsysl`):**
- `vm_init()` -- initialize kernel page table, enable MMU
- `vm_create_process_pt() -> int` -- allocate process page table
- `vm_create_server_pt(io_page: int) -> int` -- allocate server page table with MMIO grant
- `vm_v2p(ptbr: int, vaddr: int) -> int` -- virtual-to-physical translation
- `vm_copy_to(dst_ptbr, dst_vaddr, src, len)` -- cross-address-space write
- `vm_copy_from(src_ptbr, src_vaddr, dst, len)` -- cross-address-space read
- `vm_set_ptbr(addr: int)` -- set active page table
- PTE constants (PTE_V, PTE_R, PTE_W, PTE_X, PTE_U)

**CPU interface (`arch/*/cpu.lsysl`):**
- `arch_cli()` / `arch_sti()` -- disable/enable interrupts
- `build_stack_frame(entry, usp, ssp) -> i64` -- build initial context frame
- Atomic primitives for synchronization

**Boot (`arch/*/boot.asm` or `boot.S`):**
- Exception/interrupt vector table
- Syscall dispatch (fast-path and slow-path)
- Context switch (save/restore all registers)
- Timer ISR, keyboard ISR
- Initial stack and entry point

**Config (`arch/*/config.sysl`):**
- Memory layout constants (page table base, pool base/end, stack addresses)
- Device addresses (arch-specific MMIO or port I/O)

### Proposed directory structure

```
oskit/
  arch/
    trisc/
      boot.asm           # trap vectors, context switch, syscall dispatch
      vm.lsysl           # Sv32 page tables, PTBR ops, TLB flush
      cpu.lsysl          # cli/sti wrappers, build_stack_frame, atomics
      config.sysl        # TRISC memory layout, device addresses
      hal_mem.lsysl      # DMA-backed memcpy/memset
    x86/
      boot.S             # multiboot2 header, GDT, IDT, syscall MSR setup
      vm.lsysl           # x86_64 4-level page tables, CR3 ops, invlpg
      cpu.lsysl          # cli/sti, build_stack_frame (x86 register layout)
      config.sysl        # x86 memory layout
      hal_mem.lsysl      # rep movsb memcpy/memset
  kernel/
    kernel.lsysl         # scheduler, IPC, process mgmt (calls arch.* functions)
  drivers/               # some arch-specific, some shared via HAL
  servers/               # all portable (pure IPC)
  services/              # syscall ABI wrappers (portable, trampoline is arch-specific)
```

### Refactoring steps

1. **Extract VM code from kernel.lsysl** into `arch/trisc/vm.lsysl`: vm_init, vm_v2p, vm_create_process_pt, vm_create_server_pt, vm_copy_to, vm_copy_from, vm_set_ptbr, PTE constants, page_alloc/page_alloc_zero (these use arch-specific pool addresses). Kernel imports from arch module.

2. **Replace asm("cli")/asm("sti") with arch_cli()/arch_sti()** throughout kernel.lsysl. Define in `arch/trisc/cpu.lsysl`.

3. **Move build_stack_frame to arch/** -- register layout and context frame format are ISA-specific.

4. **Move config.sysl to arch/** -- memory layout constants are arch-specific. Generic constants (MAX_THREADS, MAX_PROCESSES, etc.) stay in a shared config.

5. **Test on TRISC** -- pure refactor, everything must still work identically.

6. **Create arch/x86/ stubs** -- implement the arch interface for x86_64, starting with boot.S (multiboot entry) and vm.lsysl (4-level page tables).

---

## Phase 5: RS Refactor (Minix 3 alignment)

**Problem:** SLIX's RS is fire-and-forget -- it starts servers once during boot and exits. Minix 3's RS is the root of the server tree: it starts first, monitors all servers, and can restart crashed ones. This is Minix 3's headline reliability feature.

### Current boot order (wrong)
```
kernel_main -> init -> RS -> {disk, tfs, tty, pm, vfs} -> init reads ttytab -> login
```
RS is started by init, which is backwards. RS should be more fundamental than init.

### Target boot order (Minix 3-style)
```
kernel_main -> RS -> {disk, tfs, tty, pm, vfs} -> init -> login
```
Kernel starts RS directly. RS starts all servers including init. Init is just another server that RS manages.

### RS responsibilities (current vs target)

| Feature | Current | Target |
|---------|---------|--------|
| Start servers in order | Yes | Yes |
| Handshake boot (wait for ready) | Yes | Yes |
| Monitor server health | No | Yes -- heartbeat or IPC watchdog |
| Restart crashed server | No | Yes -- detect exit, reload .trb, restart |
| Start init | No (init starts RS) | Yes (RS starts init last) |
| Privilege table | No | Yes -- restrict syscalls/IPC per server |

### Crash recovery design

1. RS keeps a server table: `{name, boot_module_idx, port, pid, state}`
2. When a server process exits (RS gets notification from PM), RS checks if it was unexpected
3. RS reloads the .trb from the boot module region (still in RAM), creates new page table, starts new process
4. RS re-registers the server's port (or the server does on startup)
5. Clients retry failed IPC calls -- servers are stateless or reconstruct state on restart

**Stateless vs stateful servers:**
- disk: stateless (just DMA, no internal state)
- tty: nearly stateless (line buffer can be lost)
- tfs: stateful (open file positions, dirty blocks) -- needs careful handling
- vfs: stateful (open file table, handle tables) -- hardest to restart
- pm: stateful (process table, waiters) -- also hard

For now, crash recovery works well for stateless drivers. Stateful server recovery requires checkpointing or state reconstruction, which is a longer-term goal.

### Privilege table design

Each server entry specifies:
- Allowed syscall bitmap (e.g., disk can't call svc_create_proc_susp)
- Allowed IPC targets (e.g., TFS can only send to disk and VFS)
- MMIO page grants (already implemented via vm_create_server_pt)
- IRQ grants (which IRQs the server can register for)

The kernel checks the privilege table on every syscall and IPC send. Violations are reported to RS, which can decide to restart or terminate the server.

---

## Phase 6: Pipes

**Goal:** `create_pipe()` returns two handles. Shell can do `cmd1 | cmd2`.

**Design:**
- Pipe buffers integrated into VFS (not a separate server)
- `create_pipe()` -> (read_handle, write_handle) via VFS IPC
- Read blocks when empty, returns 0 (EOF) when write end closed
- Write blocks when full, fails when read end closed
- Shell: `cmd1 | cmd2` -> create pipe, spawn cmd1 with stdout=pipe_write, spawn cmd2 with stdin=pipe_read

**What works when done:**
- `echo hello | cat` works
- `cat /etc/passwd | grep root` works
- `cat /etc/passwd | grep root | wc` works (multi-stage pipeline)

---

## Phase 7: Redirects and job control

**Goal:** Shell supports `>`, `<`, `>>`, `&`, `jobs`, `fg`, `bg`.

**Design:**
- Output redirect: shell opens file via connect(), passes as child's stdout handle
- Input redirect: same, as stdin handle
- Job table: nsh tracks background PIDs, prints status on completion
- `fg <job>` brings job to foreground (waitpid on it)

**What works when done:**
- `echo hello > /tmp/out` writes to file
- `cat < /tmp/out` reads from file
- `count &` runs in background, `jobs` shows it, `fg 1` brings it back

---

## Phase 8: Signals

**Goal:** Processes can receive and handle async events.

**Design:**
- PM delivers signals via kernel notification mechanism
- Default actions: terminate (SIGTERM, SIGKILL), ignore (SIGCHLD)
- Processes can register handlers via PM IPC
- SIGCHLD sent to parent when child exits
- SIGKILL is unblockable (PM terminates directly)
- Ctrl-C -> TTY sends SIGINT to foreground process group

**SLIX signal numbers (not POSIX):**
- SIG_TERM = 1, SIG_KILL = 2, SIG_CHILD = 3, SIG_INT = 4, SIG_PIPE = 5

**What works when done:**
- Ctrl-C kills foreground process
- Parent notified when child exits
- Programs can catch SIGTERM for cleanup

---

## Phase 9: x86_64 Target

**Goal:** SLIX boots in QEMU on x86_64 using the multi-target architecture from Phase 4.

**Prerequisites:** Phase 4 (arch/ refactor) complete, LLVM backend handles all oskit code.

### Boot path
- GRUB loads kernel ELF + server binaries as multiboot2 modules
- boot.S: set up GDT, IDT, enable paging (identity-map + higher-half)
- boot.S: set up syscall MSR (LSTAR, STAR, SFMASK)
- boot.S: call kernel_main

### Key differences from TRISC
- **Page tables:** 4-level (PML4 -> PDPT -> PD -> PT), 4KB pages, NX bit
- **Syscalls:** `syscall` instruction (not trap), LSTAR points to entry
- **Context switch:** push/pop all GPRs, swapgs for per-CPU data
- **Interrupts:** IOAPIC for device routing, LAPIC for timer, IDT for vectors
- **I/O:** Port I/O (`in`/`out`) for legacy, MMIO for modern devices
- **DMA:** Not needed for ramdisk (use memcpy); real disk uses AHCI DMA

### Device drivers needed
- PIT or LAPIC timer (simplest first)
- PS/2 keyboard (port I/O at 0x60/0x64)
- Serial port (port I/O at 0x3F8) -- for headless testing
- VGA text mode (MMIO at 0xB8000) -- for display
- Later: AHCI disk, virtio-blk, USB

### Build pipeline
```
sysl sources -> LLVM IR -> clang/llc -> .o files
boot.S -> nasm/gas -> boot.o
ld -> kernel.elf
grub-mkrescue -> bootable ISO
qemu-system-x86_64 -cdrom slix.iso
```

### Milestone: bare-metal hello
Already achieved (sysl -> LLVM -> x86_64 bare-metal hello in QEMU). Next: boot with page tables + syscall entry + scheduler.

---

## Phase 10: POSIX compatibility layer

**Goal:** Enough POSIX that standard C programs can be compiled and run.

**Design:**
- `libposix` maps POSIX calls to SLIX handles
- fd = handle (same integer)
- `open()` -> `connect("vfs:" + path)`
- `read(fd)` -> `handle_read(fd)`
- `posix_spawn()` -> `pm_spawn()` (nearly 1:1)
- `fork()` -> COW spawn (optional, expensive)
- Signal numbers mapped: SIGTERM->SIG_TERM, etc.

---

## Not on this roadmap (future)

- Networking (TCP/IP stack, socket server)
- Shared memory / mmap
- Dynamic linking / shared libraries
- SMP (multi-core)
- GUI / window system on x86
- Package manager
- Grant-based IPC (zero-copy memory grants, avoiding bounce buffers)
- Stateful server crash recovery (checkpointing, state reconstruction)
