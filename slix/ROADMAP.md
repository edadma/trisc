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

## Phase 0a-1: HAL extraction

**Problem:** Hardware-specific code (DMA memcpy/memset, timer device addresses and registers, UART I/O) is scattered through kernel, driver, and std library code. Porting to a new target means editing these files.

**Goal:** Move hardware-specific code into `oskit/hal/` modules behind stable interfaces. Pure refactoring — no new tools, no config files. Manually linked as before.

**HAL structure:**
```
oskit/hal/
  mem_dma.lsysl       # memcpy/memset via DMA (TRISC)
  mem_cpu.lsysl       # memcpy/memset via CPU loops (generic)
  timer_trisc.lsysl   # TRISC timer device
  uart_trisc.lsysl    # TRISC MMIO UART
```
Each HAL module exports the same symbols (e.g., `memcpy`, `memset`, `timer_init`, `timer_ack`). The build includes whichever one is appropriate for the target.

**What to do:**
- Extract memcpy/memset from `std/mem/mem.lsysl` into `oskit/hal/mem_dma.lsysl`
- Write `oskit/hal/mem_cpu.lsysl` with CPU-loop fallback
- Extract timer init/ack from `oskit/kernel/timer.lsysl` into `oskit/hal/timer_trisc.lsysl`
- Update imports — kernel/servers import from HAL, not directly from hardware
- Tests and OskitDemoBuilder link the TRISC HAL modules explicitly (same as today, just different paths)

**What works when done:** All hardware-specific code lives in `oskit/hal/`. Swapping `mem_dma` for `mem_cpu` in the source list is all it takes to change the memcpy implementation. No source changes needed in kernel or servers.

---

## Phase 0a-2: `sysl.toml` config module generation

**Problem:** Kernel constants (MAX_THREADS, PAGE_SIZE, etc.) are hardcoded as `val` declarations scattered across source files. Changing them requires editing source.

**Goal:** A `sysl.toml` file defines compile-time constants. A build tool generates a `config.lsysl` module. Source code uses `import config`.

**Build config — `sysl.toml`:**
```toml
[target]
arch = "trisc"

[hal]
mem = "oskit/hal/mem_dma"
timer = "oskit/hal/timer_trisc"
uart = "oskit/hal/uart_trisc"

[kernel]
max_threads = 8
max_processes = 16
page_size = 4096
default_quantum = 5
```

**Generated `config.lsysl`:**
```
module config
val ARCH = "trisc"
val MAX_THREADS = 8
val MAX_PROCESSES = 16
val PAGE_SIZE = 4096
val DEFAULT_QUANTUM = 5
```

Source code does `import config.{MAX_THREADS, PAGE_SIZE}` and uses these as normal constants. No compiler changes needed.

**The build tool is optional.** A default `config.lsysl` with TRISC values ships in the repo. Tests and manual compilation work without `sysl.toml` — just include the default config module. The build tool only needs to run when you want different values.

**What to do:**
- Define `sysl.toml` format
- Write config generator (Scala, reads TOML, writes `config.lsysl`)
- Ship default `config.lsysl` with TRISC defaults
- Replace hardcoded constants in kernel with `import config` values
- Tests include the default config module in their source maps

**What works when done:** Constants come from config, not hardcoded vals. Changing MAX_THREADS means editing `sysl.toml` and regenerating — no source changes.

---

## Phase 0a-3: Build tool assembles source list from `sysl.toml`

**Problem:** `OskitDemoBuilder.scala` has a manual list of ~30 source files. Adding a new server or driver means editing Scala code.

**Goal:** The `[hal]`, `[servers]`, `[drivers]` sections in `sysl.toml` control which files get compiled. The build tool assembles the source list automatically.

**Extended `sysl.toml`:**
```toml
[servers]
include = ["pm", "vfs", "tfs", "tty"]

[drivers]
include = ["disk", "keyboard"]
```

**What to do:**
- Extend build tool to read `[hal]`, `[servers]`, `[drivers]` sections
- Build tool resolves module names to file paths (convention-based: `pm` → `oskit/servers/pm.lsysl` or `slix/servers/pm.lsysl`)
- Build tool outputs a complete source list or directly invokes the compiler
- `OskitDemoBuilder` becomes a thin wrapper that calls the build tool (or remains for tests)

**What works when done:** Adding a new server = add its name to `sysl.toml` and create the file. No Scala code changes. The build tool handles everything.

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
- `oskit/fs/client.lsysl` — `fs_connect()` function

**What works when done:**
- `connect("/etc/passwd")` → handle to file
- `connect("vfs:/etc/passwd")` → same, explicit scheme
- New servers can register new schemes without kernel changes

---

## Phase 3a: TTY as a VFS scheme handler

**Goal:** `connect("tty:0")` returns a handle backed by the terminal. Read returns keyboard input, write sends to display.

**Design:**
- TTY server gains READ and WRITE IPC commands (currently only has PUTS and per-char I/O)
- VFS registers "tty" scheme pointing to TTY server's port
- VFS creates open file table entries of type "tty" (no TFS inode, no position)
- Read/write on TTY handles forward to TTY server instead of TFS

**What changes:**
- `oskit/drivers/tty/tty.lsysl` — add TTY_CMD_READ (line-buffered), TTY_CMD_WRITE handlers
- `oskit/servers/vfs.lsysl` — register "tty" scheme, route TTY-backed handles to TTY server
- VFS open file table: add `oft_type` field (0=file, 1=tty) to distinguish backends

**What works when done:**
- `connect("tty:0")` → handle to terminal
- `read(handle, buf, len)` on TTY handle → reads keyboard input
- `write(handle, buf, len)` on TTY handle → writes to display

---

## Phase 3b: Handle inheritance in spawn

**Goal:** Parent process passes handles to child via PM_SPAWN.

**Design:**
- PM_SPAWN gains a handle map: array of (parent_handle, child_slot) pairs
- PM tells VFS to set up child's handle table by copying specified entries from parent
- VFS gains a `VFS_CMD_INHERIT` command: given parent PID, child PID, and handle map, copies handles
- No implicit inheritance — parent explicitly lists which handles to pass

**What changes:**
- `oskit/servers/pm.lsysl` — PM_SPAWN accepts handle map, sends VFS_CMD_INHERIT
- `oskit/servers/vfs.lsysl` — VFS_CMD_INHERIT handler copies handles between processes
- `oskit/servers/pm.lsysl` client — pm_spawn gains handle map parameter

**What works when done:**
- Parent opens a file, spawns child with that handle → child can read/write it
- Foundation for stdin/stdout/stderr setup

---

## Phase 3c: Default stdin/stdout/stderr

**Goal:** Every external program gets handles 0/1/2 (stdin/stdout/stderr) pointing to the terminal by default.

**Design:**
- Before spawning an external program, nsh opens `connect("tty:0")` three times to get TTY handles
- nsh passes these as handles 0, 1, 2 in the spawn handle map
- External programs use `read(0, ...)` for input and `write(1, ...)` for output
- ulib replaces `puts()` with `write(1, ...)` and adds `getline()` via `read(0, ...)`

**What changes:**
- `oskit/apps/nsh.lsysl` — open TTY handles, pass in spawn handle map
- `oskit/ulib/ulib.lsysl` — read/write use handle 0/1 instead of direct TTY IPC
- `oskit/bin/*.lsysl` — use stdin/stdout handles (cat reads file handle, writes stdout; echo writes stdout)
- NEW: `oskit/bin/grep.lsysl` — filter lines by substring match
- NEW: `oskit/bin/wc.lsysl` — count lines/words/bytes

**What works when done:**
- `cat /etc/passwd` reads file, writes to stdout (TTY)
- `echo hello` writes to stdout
- `grep root /etc/passwd` filters lines containing "root"
- `wc /etc/passwd` prints line/word/byte counts
- All programs work identically whether stdout is TTY or (later) pipe

---

## Phase 4: Pipes

**Goal:** `create_pipe()` returns two handles. Shell can do `cmd1 | cmd2`.

**Design:**
- Pipe buffers integrated into VFS (not a separate server)
- `create_pipe()` → (read_handle, write_handle) via VFS IPC
- Read blocks when empty, returns 0 (EOF) when write end closed
- Write blocks when full, fails when read end closed
- Shell: `cmd1 | cmd2` → create pipe, spawn cmd1 with stdout=pipe_write, spawn cmd2 with stdin=pipe_read

**What changes:**
- `oskit/servers/vfs.lsysl` — pipe buffer table, create_pipe, pipe-aware read/write
- `oskit/apps/nsh.lsysl` — pipe syntax parsing (`|`), create pipe, spawn with redirected handles
- `oskit/fs/client.lsysl` — `fs_create_pipe()` function

**What works when done:**
- `echo hello | cat` works
- `cat /etc/passwd | grep root` works
- `cat /etc/passwd | grep root | wc` works (multi-stage pipeline)

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
