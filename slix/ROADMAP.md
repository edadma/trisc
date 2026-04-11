# SLIX Development Roadmap

Concrete, ordered development plan. Each phase builds on the previous.

## Design Decisions (settled)

- **Handles, not file descriptors.** Per-process opaque references to resources, with rights bitmask. No global meaning.
- **spawn(), not fork+exec.** Explicit process creation with explicit handle passing. No implicit inheritance.
- **connect(), not open().** Scheme-based resource naming: `vfs:/etc/passwd`, `tty:0`, `dev:disk0`. Extensible.
- **IPC is the kernel.** Synchronous send/recv/reply + async notifications. Already built.
- **POSIX is a layer.** posix_spawn→spawn, fd→handle, open→connect, fork→COW-spawn (optional). Not in the kernel.
- **sysl, not C.** Kernel, servers, and userspace all in sysl. x86 boot trampoline is the only assembly.
- **HAL for portability.** Hardware-specific code (memcpy, timers, UART) lives in swappable HAL modules. Same interface, different implementations per target. Kernel/servers never import hardware-specific code directly.
- **TOML build config.** A `sysl.toml` selects target, HAL modules, servers, and compile-time constants. No conditional compilation in the language — the build system generates a `config` module and controls which files are linked.

---

## Phase 0a: Build system and HAL foundation

**Problem:** The build is hardcoded in Scala (`OskitDemoBuilder.scala`) with a manual list of source files. There's no way to select target hardware, swap HAL modules, or set compile-time constants without editing Scala code. Hardware-specific code (DMA memcpy, timer device addresses, UART) is scattered through kernel and driver code.

**Goal:** TOML-based build config that controls what gets compiled. HAL layer that isolates hardware-specific code behind stable interfaces.

**Build config — `sysl.toml`:**
```toml
[target]
arch = "trisc"           # trisc, x86_64, arm

[hal]
mem = "oskit/hal/mem_dma"       # memcpy/memset implementation
timer = "oskit/hal/timer_trisc" # timer device
uart = "oskit/hal/uart_trisc"   # serial I/O

[kernel]
max_threads = 8
max_processes = 16
page_size = 4096
default_quantum = 5

[servers]
include = ["pm", "vfs", "tfs", "tty"]

[drivers]
include = ["disk", "keyboard"]
```

**How values reach sysl code — generated config module:**
The build tool reads `sysl.toml` and generates `config.lsysl`:
```
module config
val ARCH = "trisc"
val MAX_THREADS = 8
val MAX_PROCESSES = 16
val PAGE_SIZE = 4096
val DEFAULT_QUANTUM = 5
val HAS_DMA = 1
```
Source code does `import config` and uses these as normal constants. No compiler changes needed.

**HAL structure:**
```
oskit/hal/
  mem_dma.lsysl       # memcpy/memset via DMA (TRISC)
  mem_cpu.lsysl       # memcpy/memset via CPU loops (generic)
  mem_x86.lsysl       # memcpy/memset via rep movsb (x86)
  timer_trisc.lsysl   # TRISC timer device
  timer_x86.lsysl     # x86 PIT/APIC
  uart_trisc.lsysl    # TRISC MMIO UART
  uart_x86.lsysl      # x86 COM1 port I/O
```
Each HAL module exports the same symbols (e.g., `memcpy`, `memset`, `timer_init`, `timer_ack`). The build system links the one specified in `sysl.toml`.

**The build tool is optional.** The sysl compiler works standalone — pass it source files and it compiles them. The build tool is a convenience that reads `sysl.toml`, generates the config module, and assembles the file list. Unit tests keep working as they do today — they construct their own source maps in Scala. Someone compiling a single program doesn't need `sysl.toml` at all.

**What to do:**
1. Define `sysl.toml` format
2. Write build tool (Scala, reads TOML, generates config module, assembles source list)
3. Extract hardware-specific memcpy/memset into `oskit/hal/mem_dma.lsysl`
4. Extract timer device code into `oskit/hal/timer_trisc.lsysl`
5. Provide a default `config.lsysl` with TRISC defaults so everything works without the build tool
6. Existing `OskitDemoBuilder` and tests keep working as-is (they can optionally use the build tool or continue constructing source maps manually)

**What works when done:** `sysl.toml` drives the full OS build. Changing `hal.mem` swaps the implementation with no source changes. Constants come from config module. But everything also works without the build tool — just pass the right files to the compiler manually.

---

## Phase 0b: Separate fs client library from TFS server
**Problem:** `oskit/servers/tfs.lsysl` contains both the TFS IPC server AND the `fs_open/fs_read/fs_write/...` client functions. Clients import `oskit.servers.{fs_open, fs_read}` which drags in the server code. The client functions also do `port_lookup("fs")` on every call.

**Goal:** Clean separation: client library talks to whatever filesystem server is registered, server wraps the TFS implementation.

**What to do:**
- NEW: `oskit/fs/client.lsysl` — module `oskit.fs.client` with `fs_open`, `fs_read`, `fs_write`, `fs_stat`, `fs_create`, `fs_unlink`, `fs_mkdir`, `fs_rmdir`, `fs_readdir`, `fs_chmod`, `fs_rename`
- Client caches the port on first use (not per-call lookup)
- Remove client functions from `oskit/servers/tfs.lsysl`
- Update all imports: init, nsh, login, ulib → import from `oskit.fs.client`

**What works when done:** Same behavior, cleaner structure. Client code doesn't depend on server code.

---

## Phase 0c: VFS server — namespace and mount table
**Problem:** The TFS server registers as "fs" and handles all filesystem requests directly. There's no mount table, no way to have multiple filesystems, no namespace layer.

**Goal:** VFS server owns the namespace. TFS becomes a backend that VFS delegates to.

**Design:**
- VFS server registers port "fs" (replaces TFS's registration)
- TFS server registers port "tfs" (private, VFS talks to it)
- VFS maintains a mount table: `[{path_prefix, backend_port}]`
- Root "/" is mounted on TFS at boot
- VFS receives client requests (open, read, write, ...), resolves path to mount point, forwards to backend
- For now: single mount point (root → TFS). Multi-mount comes later.

**What changes:**
- NEW: `oskit/servers/vfs.lsysl` — mount table, path resolution, request forwarding
- `oskit/servers/tfs.lsysl` — register as "tfs" instead of "fs"
- `oskit/apps/init.lsysl` — start VFS server, VFS mounts root on TFS
- `oskit/fs/client.lsysl` — talks to "fs" port (now VFS, transparent)

**What works when done:** All file operations go through VFS → TFS. Same behavior, but VFS is in the path. Foundation for mount points and multiple filesystems.

---

## Phase 0d: Open file state in VFS
**Problem:** There's no concept of an open file with persistent state. Every `fs_read(ino, buf, offset, len)` passes the inode and offset explicitly. Callers track their own position. There are no per-process open file entries.

**Goal:** VFS tracks open files with per-open-instance state (position, flags). Returns an opaque "file ID" (proto-handle) instead of a raw inode.

**Design:**
- VFS maintains a global open file table: `[{ino, position, flags, mount_idx, refcount}]`
- `fs_open(path)` → VFS resolves path, creates open file entry, returns file ID
- `fs_read(fid, buf, len)` → VFS looks up fid, reads at current position, advances position
- `fs_write(fid, buf, len)` → same
- `fs_close(fid)` → decrements refcount, frees entry when zero
- `fs_seek(fid, offset, whence)` → adjusts position

**What changes:**
- `oskit/servers/vfs.lsysl` — open file table, position tracking
- `oskit/fs/client.lsysl` — new API: `fs_read(fid, buf, len)` (no more offset/ino)
- `oskit/apps/nsh.lsysl` — use new API (simpler: no offset tracking)
- `oskit/bin/cat.lsysl` — use new API

**What works when done:** Files have position state. `read` advances automatically. Multiple opens of the same file have independent positions. This is the foundation for handles.

---

## Phase 1: Per-process handle table

**Goal:** Every process has a handle table. Handles are the interface to all resources.

**Design:**
- VFS maintains per-process handle tables (indexed by PID)
- Handle = index into per-process table (0, 1, 2, ...)
- Each entry maps to an open file table entry (from Phase 0c)
- Handle 0 = stdin, 1 = stdout, 2 = stderr (convention)
- PM notifies VFS on process creation/exit so VFS can manage tables
- `dup(handle)` → new handle pointing to same open file entry

**New API (replaces fs_* functions):**
- `open(path, flags) → handle`
- `read(handle, buf, len) → bytes_read`
- `write(handle, buf, len) → bytes_written`
- `close(handle)`
- `seek(handle, offset, whence) → position`
- `dup(handle) → new_handle`

**What changes:**
- `oskit/servers/vfs.lsysl` — per-process handle tables, handle allocation
- `oskit/servers/pm.lsysl` — notify VFS on process create/exit
- `oskit/fs/client.lsysl` — handle-based API
- `oskit/ulib/ulib.lsysl` — handle-based API for external programs
- nsh, cat, echo, etc. — use handle-based I/O

**What works when done:** Programs use `open/read/write/close` with handles. VFS routes to the right backend. Per-process isolation of handles.

---

## Phase 2: spawn() with argv and handles

**Goal:** Replace `create_process` + flat args string with proper `spawn(path, argv, handles)`.

**Design:**
- PM receives spawn request via IPC
- PM loads binary (delegates to loader)
- PM places argc/argv on process stack in POSIX layout:
  ```
  [strings]  "echo\0" "hello\0"
  [NULL]
  [argv[1]]  → "hello\0"
  [argv[0]]  → "echo\0"
  [argc = 2]
  ← SP
  ```
- PM tells VFS to create child's handle table, copying specified handles from parent
- No implicit handle inheritance — parent explicitly lists which handles to pass
- Child's handle 0/1/2 are whatever the parent specifies

**API (PM server IPC):**
- `PM_SPAWN(path, argc, argv_data, handle_map)` → pid
- handle_map: array of `(parent_handle, child_slot)` pairs

**What changes:**
- `oskit/servers/pm.lsysl` — PM_SPAWN handler, argv stack setup
- `oskit/apps/nsh.lsysl` — build argv array, specify handle map
- `oskit/ulib/ulib.lsysl` — remove PROG_ARGS_ADDR; argc/argv from stack
- `oskit/bin/*.lsysl` — update all programs to use argc/argv
- `oskit/kernel/kernel.lsysl` — remove create_process (PM owns this now)

**What works when done:**
- `echo hello world` → child gets `argc=3, argv=["echo","hello","world"]`
- Child inherits only the handles parent explicitly passes
- Programs have proper C-style entry point

---

## Phase 3: connect() and scheme routing

**Goal:** `connect("vfs:/etc/passwd")` returns a handle. Extensible naming.

**Design:**
- VFS maintains a scheme registry: `{ "vfs" → fs_port, "tty" → tty_port }`
- Servers register schemes at startup
- `connect(name)` parses scheme, looks up port, sends OPEN to that server, returns handle
- Paths without scheme default to "vfs:" (so `connect("/etc/passwd")` works)

**What changes:**
- `oskit/servers/vfs.lsysl` — scheme registry, connect routing
- `oskit/drivers/tty/tty.lsysl` — register as "tty" scheme handler
- `oskit/ulib/ulib.lsysl` — `connect()` function

**What works when done:**
- `connect("/etc/passwd")` → handle to file
- `connect("tty:0")` → handle to terminal
- New servers can register new schemes without kernel changes

---

## Phase 4: Pipes

**Goal:** `create_pipe()` returns two handles. Shell can do `cmd1 | cmd2`.

**Design:**
- Pipe server manages byte buffers
- `create_pipe()` → (read_handle, write_handle)
- read blocks when empty, returns 0 (EOF) when write end closed
- write blocks when full, fails when read end closed
- Shell: `cmd1 | cmd2` → create pipe, spawn cmd1 with stdout=write_end, spawn cmd2 with stdin=read_end

**What changes:**
- NEW: `oskit/servers/pipe.lsysl` (or integrated into VFS)
- `oskit/apps/nsh.lsysl` — pipe syntax parsing, spawn with redirected handles

**What works when done:**
- `echo hello | cat` works
- `cat /etc/passwd | grep root` works (once grep exists)

---

## Phase 5: Redirects and job control

**Goal:** Shell supports `>`, `<`, `>>`, `&`, `jobs`, `fg`, `bg`.

**Design:**
- Output redirect: shell opens file via connect(), passes as child's stdout handle
- Input redirect: same, as stdin handle
- Job table: nsh tracks background PIDs, prints status on completion
- `fg <job>` brings job to foreground (waitpid on it)

**What changes:**
- `oskit/apps/nsh.lsysl` — redirect parsing, job table, fg/bg/jobs commands

**What works when done:**
- `echo hello > /tmp/out` writes to file
- `cat < /tmp/out` reads from file
- `count &` runs in background, `jobs` shows it, `fg 1` brings it back

---

## Phase 6: Signals

**Goal:** Processes can receive and handle async events.

**Design:**
- PM delivers signals via kernel notification mechanism
- Default actions: terminate (SIGTERM, SIGKILL), ignore (SIGCHLD)
- Processes can register handlers via PM IPC
- SIGCHLD sent to parent when child exits
- SIGKILL is unblockable (PM terminates directly)
- Ctrl-C → TTY sends SIGINT to foreground process group

**SLIX signal numbers (not POSIX):**
- SIG_TERM = 1, SIG_KILL = 2, SIG_CHILD = 3, SIG_INT = 4, SIG_PIPE = 5

**What changes:**
- `oskit/servers/pm.lsysl` — signal delivery, handler registration
- `oskit/drivers/tty/tty.lsysl` — Ctrl-C sends SIG_INT via PM
- `oskit/ulib/ulib.lsysl` — signal handler registration API

**What works when done:**
- Ctrl-C kills foreground process
- Parent notified when child exits
- Programs can catch SIGTERM for cleanup

---

## Phase 7: POSIX compatibility layer

**Goal:** Enough POSIX that standard C programs can be compiled and run.

**Design:**
- `libposix` maps POSIX calls to SLIX handles
- fd = handle (same integer)
- `open()` → `connect("vfs:" + path)`
- `read(fd)` → `handle_read(fd)`
- `posix_spawn()` → `pm_spawn()` (nearly 1:1)
- `fork()` → COW spawn (optional, expensive)
- Signal numbers mapped: SIGTERM→SIG_TERM, etc.

**What changes:**
- NEW: `posix/libposix.lsysl`

---

## Phase 8: x86_64 native

**Goal:** SLIX boots on real x86_64 hardware.

**Dependencies:** LLVM backend handles oskit code (inline asm, pointers, structs, modules).

**What to do:**
- LLVM codegen for all oskit code
- x86 device drivers (PCI, AHCI, VGA, PS/2, serial)
- GRUB boot on real hardware

---

## Not on this roadmap (future)

- Networking (TCP/IP stack, socket server)
- Shared memory / mmap
- Dynamic linking / shared libraries
- SMP (multi-core)
- GUI / window system on x86
- Package manager
