# Musl → SLIX Port Notes

Working plan for porting musl libc 1.2.6 to SLIX on aarch64. This doc
inventories the syscall surface musl needs, maps it to what SLIX already
provides, and sketches the porting phases.

## Syscall mechanism

Musl on aarch64 invokes Linux syscalls via a very simple asm macro
(`arch/aarch64/syscall_arch.h`):

- Syscall number in `x8`
- Args in `x0`–`x5` (up to 6 args)
- `svc 0` instruction (ARM supervisor call trap)
- Return value in `x0`
- Negative return = `-errno` (musl's `__syscall_ret` converts this)

**For SLIX the mechanism is identical** (same instruction, same ABI). Only
the syscall *numbers* differ. So `arch/aarch64/syscall_arch.h` can stay
unchanged; the work lives in `arch/aarch64/bits/syscall.h.in` (the
`__NR_xxx` number table) and whatever POSIX-ish syscalls SLIX needs to
expose that don't exist today.

## Syscall inventory

Filtering the 348 unique `SYS_*` references in musl source against the 326
syscall numbers aarch64 currently defines yields **232 syscalls musl
actually uses on aarch64**. The other 116 are legacy/alt variants used by
32-bit archs (`SYS_getuid32`, `SYS_stat64`, `SYS_clock_gettime32`, etc.)
that aarch64 skips.

Not all 232 get pulled in by every program. A static `hello world` only
exercises a handful. Coreutils exercises ~40–50. Something with networking
and signals exercises most of them.

## SLIX syscall inventory (current)

Defined in `oskit/services/services.lsysl`. ~30 direct syscalls plus
~40 `svc_*` helpers that wrap IPC into servers.

Categories:

| Category | Coverage | Details |
|----------|----------|---------|
| Thread basics | ✅ | sleep, yield, exit, join, thread_id, uptime, tls_set/get |
| Console I/O | partial | putc, kbhit, getkey (char-level, not fd-based) |
| IPC / notifications | ✅ | notify_send/wait/read, event_wait/set/clear, pimutex |
| Memory / VM | partial | vm_create_pt, vm_copy_to/from, v2p, grants, phys_read |
| Process mgmt | partial | create_proc_susp, resume, reap, kill, waitpid (via PM) |
| Filesystem | **missing from syscall layer** | Exists in VFS/TFS servers via IPC; no `open`/`read`/`write` syscall |
| Signals | **missing** | No signal delivery in SLIX yet |
| Time | partial | uptime only; no wall-clock or clock_gettime |
| Networking | partial | Inet server exists; no socket() syscall |

The gap is significant but not scary: most POSIX FS/proc operations *work*
in SLIX through IPC to servers — they're just not yet exposed as syscalls
a libc can call directly.

## Porting strategy

### Option A (recommended): Expose POSIX-like syscalls in SLIX

Add new syscall numbers in SLIX whose handlers do the IPC-to-server dance
internally. From musl's perspective, `write(fd, buf, n)` is one `svc 0` and
the return value; the kernel routes that to the VFS server, which routes it
to the owning driver. Musl's source stays upstream-clean for those
syscalls — it just sees Linux-like numbers that happen to be wired up by
SLIX.

Pros: minimal musl changes, upstreamable cleanly, clear separation.
Cons: kernel grows a POSIX shim layer.

### Option B: Teach musl about SLIX IPC

Have musl's `write()` etc. issue IPC directly to VFS. Means replacing the
syscall layer entirely in musl for a bunch of functions.

Pros: SLIX kernel stays minimalist.
Cons: Huge musl fork, hard to rebase on upstream releases.

**Going with Option A.**

## Phases

### Phase 0: build system + arch dir (1-2 days)

- Add `arch/aarch64-slix/` in musl (or reuse `arch/aarch64/` with config
  overrides — musl's `configure` supports both patterns)
- Write `bits/syscall.h.in` with SLIX's syscall numbers
- Copy other `bits/` headers from `arch/aarch64/` as starting points
- Get `configure && make` to run end-to-end, even if most syscalls stub to
  `-ENOSYS`. Output: `libc.a`.

### Phase 1: hello world (1-2 days)

Target: `write(1, "hello\n", 6)` followed by `exit_group(0)` runs under
SLIX. The two syscalls that matter:

| POSIX | musl name | SLIX equivalent | Status |
|-------|-----------|-----------------|--------|
| `write(1, buf, n)` | `SYS_write` | Currently `putc` loop via `SYS_PUTC` | **Need new SLIX syscall `SYS_WRITE` that handles fd + buffer + length** |
| `exit_group(code)` | `SYS_exit_group` | `SYS_EXIT` | Rename in musl's SLIX syscall.h to 3 |

At this point static-linked GNU Hello should also run (it also uses
`__stdout`-via-`write`, `error`, `setlocale` stubs, etc.).

### Phase 2: coreutils-enabling set (1-2 weeks)

The minimum for useful programs:

- **File I/O**: `openat`, `read`, `write`, `close`, `fstat`, `newfstatat`,
  `getdents64`, `lseek`, `dup`, `dup3`, `fcntl`, `pipe2`, `readlinkat`,
  `unlinkat`, `renameat2`, `mkdirat`, `symlinkat`
- **Process**: `clone` (for fork emulation), `execve`, `wait4`, `waitid`,
  `getpid`, `getppid`, `gettid`, `kill`
- **Memory**: `brk`, `mmap`, `munmap`, `mprotect` — or at least enough for
  malloc. Musl's `mallocng` wants real mmap; `lite_malloc.c` can get by
  with brk.
- **Time**: `clock_gettime`, `nanosleep`, `gettimeofday`
- **Signals (minimal)**: `rt_sigaction`, `rt_sigprocmask`, `rt_sigreturn`,
  `sigaltstack`, `tkill` — even if initially signal delivery is stubbed,
  musl needs these to link
- **Misc**: `ioctl` (stubs for most, real handling for `TIOCGWINSZ`,
  `TCGETS` etc.), `uname`, `getuid`/`geteuid`/`getgid`/`getegid`,
  `set_tid_address`, `prlimit64`

Anything else stays `-ENOSYS` and the kernel returns the canonical error.

### Phase 3: near-full POSIX (weeks-to-months)

- Signals with actual delivery
- Real `pthread` (clone + futex + set_robust_list)
- `select`/`poll`/`ppoll`/`epoll*`
- `socket`/`bind`/`connect`/`send`/`recv` (via Inet server)
- `inotify*` if SLIX gets fs events

### Phase 4: dynamic linker (optional)

Musl's `ldso/` is self-contained; porting means getting relocations and TLS
setup working for SLIX's memory layout. Static linking is the default plan
for now, so this is defer-until-needed.

## Concrete next steps

1. **Decide syscall numbering for new SLIX POSIX calls.** Propose a range
   like 128-255 reserved for POSIX-ish syscalls that shim into VFS/PM/etc.
2. **Write `arch/aarch64-slix/bits/syscall.h.in`** defining the aarch64-
   SLIX numbers. For the SLIX-native calls (`SYS_PUTC`, `SYS_SLEEP`, etc.)
   that overlap musl's set semantically but differ in signature, decide
   whether to re-expose them under Linux-compat names.
3. **Wire up the first two SLIX kernel syscalls**: `SYS_WRITE` (fd + buf +
   len, shim into VFS/TTY) and `SYS_EXIT_GROUP` (alias of existing EXIT,
   or new semantics for multi-thread exit).
4. **Build musl against a stub sysroot**, then link a `hello.c` that
   uses `write(1, ...)` + `exit(0)`. Verify `svc 0` actually fires and the
   output appears.

Everything else is iteration on the same loop: pick a program you want to
run, run it, see what syscall fails, implement that syscall, loop.
