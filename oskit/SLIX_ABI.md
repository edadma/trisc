# SLIX ABI Reference

**Version:** Phase 0b lockdown, slix@5f0804a9f, 2026-05-02.

This document is the canonical reference for every binary interface
in SLIX: the kernel-syscall ABI, the POSIX shim's syscall surface,
and the per-server IPC protocols. It exists to make the Phase 1 VM
+ fork/exec rewrite *intentional*: anything outside this document
is not part of the ABI, and anything inside it is what new code
must keep compatible (until a phase explicitly says otherwise).

If you change a wire format, an error return, or a syscall number,
update this file *in the same commit* as the code change. If you
add a new command, add it here too. Reviewers should reject
ABI-touching diffs that don't update SLIX_ABI.md.

## 0. Wire-format conventions

These conventions are uniform across all servers and the kernel.

- **Endianness.** Every multi-byte integer in an IPC message is
  big-endian. The encoding helpers are `unix_write_u16` / `_u32` /
  `_u64` (the names are historical — they're shared across
  servers, not unix-specific). The decoding side uses
  `unix_read_u16` / `_u32` / `_u64`. Big-endian was chosen so
  hex-dumped traffic is human-readable; do not switch to LE.
- **Message shape.** Request: `[cmd:1 byte][args...]`. Reply:
  `[status:1 byte][payload...]`, where `status` is `0` on success
  and a negative `errno`-style code on failure. Errno values match
  Linux constants (defined inline in `oskit/posix/shim.lsysl` and
  copied into servers; e.g. `EAGAIN=11`, `EINVAL=22`, `EBADF=9`).
- **String args.** NUL-terminated (`path...NUL`) unless an
  explicit length precedes them. Path length cap is 255 bytes
  including NUL; per-component names are capped at TFS' on-disk
  ceiling (`DIR_NAME_LEN=60` per directory entry, v2). Longer
  components are truncated by the shim.
- **fd args.** Server-side fd refers to a slot in that server's
  per-process socket/file table. Translation between the shim's
  posix-fd table and the server's slot is done by the shim.
- **Reply size.** Servers must `ipc_reply` with at least the
  fixed-size header even on error paths; the only acceptable
  variable-size payload is the data block of READ/RECVFROM/etc.,
  whose length is reported in the reply header.
- **Cross-PT delivery.** When sender and receiver are in different
  page tables, the kernel uses a bounce buffer (see
  `ipc_memcpy_cross` in `oskit/ipc/ipc.lsysl`). Servers do not
  need to be aware; they receive a kernel-VA pointer.
- **Notification port.** Some commands return `-EAGAIN` from a
  non-blocking variant and additionally arm a wake-up notify on
  state change. Subscribers are tracked per-fd with a `fire_seq`
  counter (per-fd EPOLLET, Phase A2 closeout). Subscribed clients
  receive `SYS_NOTIFY_SEND` (kernel syscall 23).

## 1. Kernel syscall ABI (`syscall(num, ...)`)

Direct kernel services callable from any thread (kernel-mode or
user-mode). Numbers are stable. Definitions live in
`oskit/services/services.lsysl`; handlers register via
`register_syscall` / `register_syscall6` in `oskit/kernel/kernel.lsysl`.

| Num | Name                  | Handler                              |
| --- | --------------------- | ------------------------------------ |
| 0   | SYS_SLEEP             | sleep_current                        |
| 1   | SYS_PUTC              | (fast-path UART write)               |
| 2   | SYS_YIELD             | yield_current                        |
| 3   | SYS_EXIT              | terminate_current                    |
| 4   | SYS_JOIN              | join_current                         |
| 5   | SYS_THREAD_ID         | sys_thread_id_handler                |
| 6   | SYS_UPTIME            | sys_uptime_handler                   |
| 7   | SYS_THREAD_COUNT      | sys_thread_count_handler             |
| 8   | SYS_THREAD_STATE      | sys_thread_state_handler             |
| 9   | SYS_THREAD_NAME       | sys_thread_name_handler              |
| 10  | SYS_SLEEP_UNTIL       | sleep_until_current                  |
| 11  | SYS_KBHIT             | sys_kbhit_handler                    |
| 12  | SYS_GETKEY            | sys_getkey_handler                   |
| 13  | SYS_CTX_SWITCHES      | (fast-path query)                    |
| 14  | SYS_CPU_TICKS         | (fast-path query)                    |
| 15  | SYS_TOTAL_SWITCHES    | (fast-path query)                    |
| 16  | SYS_SET_WATCHDOG      | (timer reload)                       |
| 17  | SYS_PANIC             | (immediate halt)                     |
| 18  | SYS_CHECK_STACK       | (canary verify)                      |
| 19  | SYS_SUSPEND           | (state→SUSPENDED)                    |
| 20  | SYS_RESUME            | (state→READY)                        |
| 21  | SYS_TLS_SET           | (TLS slot write)                     |
| 22  | SYS_TLS_GET           | (TLS slot read)                      |
| 23  | SYS_NOTIFY_SEND       | sys_notify_send_handler              |
| 24  | SYS_NOTIFY_WAIT       | sys_notify_wait_handler              |
| 25  | SYS_NOTIFY_READ       | sys_notify_read_handler              |
| 26  | SYS_EVENT_WAIT        | sys_event_wait_handler               |
| 27  | SYS_EVENT_SET         | sys_event_set_handler                |
| 28  | SYS_EVENT_CLEAR       | sys_event_clear_handler              |
| 29  | SYS_PIMUTEX_LOCK      | pimutex_lock_kernel                  |
| 30  | SYS_PIMUTEX_UNLOCK    | pimutex_unlock_kernel                |
| 31  | SYS_PORT_CREATE       | ipc_port_create_handler              |
| 32  | SYS_IPC_SEND          | ipc_send_handler                     |
| 33  | SYS_IPC_RECV          | ipc_recv_handler                     |
| 34  | SYS_IPC_REPLY         | ipc_reply_handler                    |
| 35  | SYS_PORT_CLOSE        | ipc_port_close_handler               |
| 36  | SYS_PORT_REGISTER     | ipc_port_register_handler            |
| 37  | SYS_PORT_LOOKUP       | ipc_port_lookup_handler              |
| 38  | SYS_IPC_SEND_TIMED    | ipc_send_timed_handler               |
| 39  | SYS_IPC_RECV_NOTIFY   | ipc_recv_notify_handler              |
| 40  | SYS_WAITPID           | waitpid_current                      |
| 41  | SYS_GETPID            | getpid_current                       |
| 42  | SYS_THREAD_PID        | sys_thread_pid_handler               |
| 43  | SYS_THREAD_NAME_LEN   | sys_thread_name_len_handler          |
| 44  | SYS_VM_CREATE_PT      | sys_vm_create_pt_handler             |
| 45  | SYS_CREATE_PROC_SUSP  | sys_create_proc_susp_handler         |
| 46  | SYS_RESUME_PROC       | sys_resume_proc_handler              |
| 47  | SYS_REAP_PROC         | sys_reap_proc_handler                |
| 48  | SYS_KILL_PROC         | sys_kill_proc_handler                |
| 49  | SYS_REGISTER_PM       | sys_register_pm_handler              |
| 50  | SYS_SET_THREAD_PTBR   | sys_set_thread_ptbr_handler          |
| 51  | SYS_VM_SET_PTBR_SC    | sys_vm_set_ptbr_handler              |
| 52  | SYS_GET_THREAD_PID    | sys_get_thread_pid_handler           |
| 53  | SYS_GET_THREAD_PTBR   | sys_get_thread_ptbr_handler          |
| 54  | SYS_VM_COPY_TO        | sys_vm_copy_to_handler               |
| 55  | SYS_VM_COPY_FROM      | sys_vm_copy_from_handler             |
| 56  | SYS_PROC_STATE        | sys_proc_state_handler               |
| 57  | SYS_PROC_EXIT_CODE    | sys_proc_exit_code_handler           |
| 58  | SYS_PROC_MAIN_TID     | sys_proc_main_tid_handler            |
| 59  | SYS_PROC_PARENT_TID   | sys_proc_parent_tid_handler          |
| 60  | SYS_PROC_SET_PARENT_TID | sys_proc_set_parent_tid_handler    |
| 61  | SYS_GETUID            | sys_getuid_handler                   |
| 62  | SYS_SETUID            | sys_setuid_handler                   |
| 63  | SYS_VM_CREATE_SERVER_PT | sys_vm_create_server_pt_handler    |
| 64  | SYS_KB_SET_NOTIFY     | sys_kb_set_notify_handler            |
| 65  | (ipc_last_msg_len)    | ipc_last_msg_len_handler             |
| 66  | SYS_VM_V2P            | sys_vm_v2p_handler                   |
| 67  | SYS_PHYS_READ         | sys_phys_read_handler                |
| 68  | SYS_PORT_TRANSFER     | ipc_port_transfer_handler            |
| 69  | SYS_SET_PRIV          | sys_set_priv_handler                 |
| 70  | SYS_SET_IPC_MASK      | sys_set_ipc_mask_handler             |
| 71  | SYS_GRANT_CREATE      | sys_grant_create_handler             |
| 72  | SYS_GRANT_REVOKE      | sys_grant_revoke_handler             |
| 73  | SYS_GRANT_COPY        | sys_grant_copy_handler               |
| 74  | SYS_GRANT_MAP         | sys_grant_map_handler                |
| 75  | SYS_GRANT_UNMAP       | sys_grant_unmap_handler              |
| 76  | SYS_VIRTIO_SET_NOTIFY | sys_virtio_set_notify_handler        |
| 77  | SYS_GETINFO           | sys_getinfo_handler                  |
| 78  | SYS_SET_RS_TID        | sys_set_rs_tid_handler               |
| 79  | SYS_GRANT_POSIX_RANGE | sys_grant_posix_range_handler        |
| 80  | SYS_VIRTIO_PCI_INFO   | sys_virtio_pci_info_handler          |
| 81  | SYS_TIMER_SUBSCRIBE   | sys_timer_subscribe_handler          |
| 82  | SYS_PUTS              | (fast-path UART)                     |
| 83  | SYS_SLEEP_OR_NOTIFY   | sys_sleep_or_notify_handler          |
| 84  | SYS_GET_THREAD_UID    | sys_get_thread_uid_handler           |
| 85  | SYS_FD_TRANSPLANT     | (fd handoff helper)                  |
| 86  | SYS_VMA_SELFTEST      | sys_vma_selftest_handler (debug)     |
| 87  | SYS_VMA_CREATE        | sys_vma_create_handler (debug)       |
| 88  | SYS_VMA_CREATE_PID    | sys_vma_create_pid_handler (debug)   |
| 89  | SYS_COW_REFCNT_SELFTEST | sys_cow_refcnt_selftest_handler (debug) |
| 90  | SYS_COW_SHARE_SELF    | sys_cow_share_self_handler (debug)   |
| 91  | SYS_FORK              | sys_fork_handler                     |
| 92  | SYS_ELF_SELFTEST      | sys_elf_selftest_handler (debug)     |

PHASE 1 NOTE. The VM/process syscalls (44–60, 63, 66, 79) all
assume the current "fixed-region eager mapping" model. They
remain stable in number but their semantics change in Phase 1
when VMA lists land:
- `SYS_VM_CREATE_PT` returns a PT with no eager mappings (VMA list
  is consulted lazily on fault).
- `SYS_VM_COPY_TO` / `SYS_VM_COPY_FROM` still work, but will fault
  in pages on access via the destination VMA.

## 2. POSIX shim syscall surface (`posix_dispatch`)

User programs call into the shim via the architecture-specific
fast-path (TRISC `ecall`, x86_64 `syscall`, aarch64 `svc`) with the
Linux-amd64 syscall numbering. Numbers are not chosen by SLIX —
they match Linux so musl-linked binaries Just Work. Source:
`oskit/posix/shim.lsysl::posix_dispatch`.

| Num | Linux name                | SLIX handler                   |
| --- | ------------------------- | ------------------------------ |
| 128 | write                     | sys_write                      |
| 129 | exit_group                | terminate_current              |
| 130 | read                      | sys_read                       |
| 131 | accept                    | sys_accept                     |
| 132 | accept4                   | sys_accept4                    |
| 135 | bind                      | sys_bind                       |
| 136 | brk                       | sys_brk                        |
| 142 | clock_getres              | sys_clock_getres               |
| 143 | clock_gettime             | sys_clock_gettime              |
| 144 | clock_nanosleep           | sys_clock_nanosleep            |
| 147 | close                     | sys_close                      |
| 148 | connect                   | sys_connect                    |
| 151 | dup                       | sys_dup                        |
| 152 | dup3                      | sys_dup3                       |
| 153 | epoll_create1             | sys_epoll_create1              |
| 154 | epoll_ctl                 | sys_epoll_ctl                  |
| 155 | epoll_pwait               | sys_epoll_pwait                |
| 156 | eventfd2                  | sys_eventfd2                   |
| 159 | faccessat                 | sys_faccessat                  |
| 160 | faccessat2                | sys_faccessat                  |
| 167 | fchmodat                  | sys_fchmodat                   |
| 171 | fcntl                     | sys_fcntl                      |
| 172 | fsync                     | (validated stub: fd → 0/-EBADF) |
| 178 | fstat                     | sys_fstat                      |
| 180 | fdatasync                 | (validated stub)               |
| 181 | ftruncate                 | sys_ftruncate                  |
| 187 | getegid                   | (returns 0)                    |
| 188 | geteuid                   | (returns 0)                    |
| 189 | getgid                    | (returns 0)                    |
| 192 | getpeername               | sys_getpeername                |
| 194 | getpid                    | posix_fd_current_pid           |
| 195 | getppid                   | (returns 1)                    |
| 197 | getrandom                 | sys_getrandom                  |
| 200 | getrlimit                 | sys_prlimit64                  |
| 201 | getrusage                 | (zero-fills 144B)              |
| 203 | getsockname               | sys_getsockname                |
| 204 | getsockopt                | sys_getsockopt                 |
| 205 | gettid                    | (returns current_thread)       |
| 206 | gettimeofday              | sys_gettimeofday               |
| 207 | getuid                    | (returns 0)                    |
| 215 | munmap                    | sys_munmap                     |
| 216 | mremap                    | (returns -ENOMEM, falls back)  |
| 217 | listen                    | sys_listen                     |
| 221 | lseek                     | sys_lseek                      |
| 222 | mmap                      | sys_mmap                       |
| 223 | madvise                   | (returns 0)                    |
| 226 | mprotect                  | sys_mprotect                   |
| 227 | mkdirat                   | sys_mkdirat                    |
| 251 | nanosleep                 | sys_nanosleep                  |
| 252 | newfstatat                | sys_newfstatat                 |
| 254 | openat                    | sys_openat                     |
| 256 | pipe2                     | sys_pipe2                      |
| 258 | ppoll                     | sys_ppoll                      |
| 259 | prctl                     | (PR_SET_NAME / PR_GET_NAME ack; rest -EINVAL) |
| 260 | pread64                   | sys_pread64                    |
| 263 | prlimit64                 | sys_prlimit64                  |
| 268 | pwrite64                  | sys_pwrite64                   |
| 274 | readv                     | sys_readv                      |
| 276 | recvfrom                  | sys_recvfrom                   |
| 278 | recvmsg                   | sys_recvmsg                    |
| 281 | renameat                  | sys_renameat                   |
| 282 | renameat2                 | sys_renameat (flags ignored)   |
| 299 | sched_yield               | (returns 0)                    |
| 306 | sendmsg                   | sys_sendmsg                    |
| 307 | sendto                    | sys_sendto                     |
| 326 | setsockopt                | sys_setsockopt                 |
| 334 | shutdown                  | sys_shutdown                   |
| 337 | socket                    | sys_socket                     |
| 338 | socketpair                | sys_socketpair                 |
| 345 | sync                      | (returns 0)                    |
| 347 | syncfs                    | (validated stub)               |
| 356 | timerfd_create            | sys_timerfd_create             |
| 357 | timerfd_gettime           | sys_timerfd_gettime            |
| 358 | timerfd_settime           | sys_timerfd_settime            |
| 365 | unlinkat                  | sys_unlinkat                   |
| 372 | writev                    | sys_writev                     |

Anything not listed returns `-ENOSYS`. New shim syscalls land here.

## 3. Server IPC tables

All servers register a port name via `ipc_port_register`. Clients
look up the port via `ipc_port_lookup(name) -> port_id`. Every
request has a 1-byte command discriminator at offset 0 and a
1-byte status reply at offset 0. Status is 0 on success, negative
errno on failure unless otherwise noted.

### 3.1 disk — `oskit/drivers/disk/disk.lsysl` (port: "disk")

| Num | Name              | Request                              | Reply                       |
| --- | ----------------- | ------------------------------------ | --------------------------- |
| 1   | DISK_CMD_READ     | `[1][lba:4be][addr:4be]`             | `[status:1]`                |
| 2   | DISK_CMD_WRITE    | `[2][lba:4be][addr:4be]`             | `[status:1]`                |
| 3   | DISK_CMD_CAPACITY | `[3]`                                | `[status:1][cap:4be]`       |

`addr` is a kernel-VA pointer to the 512-byte buffer in the
client's address space; the disk driver uses the kernel's bounce
copy facility to move data across PTs.

### 3.2 tfs — `oskit/servers/tfs.lsysl` (port: "tfs")

| Num | Name              | Request                                                             | Reply                                                                |
| --- | ----------------- | ------------------------------------------------------------------- | -------------------------------------------------------------------- |
| 1   | TFS_CMD_OPEN      | `[1][path...NUL]`                                                   | `[status:1][ino:4be]`                                                |
| 2   | TFS_CMD_READ      | `[2][ino:4be][offset:4be][len:4be]`                                 | `[status:1][nread:4be][data...]`                                     |
| 3   | TFS_CMD_WRITE     | `[3][ino:4be][offset:4be][len:4be][data...]`                        | `[status:1][nwritten:4be]`                                           |
| 4   | TFS_CMD_CREATE    | `[4][parent:4be][type:1][perm:2][name...NUL]`                       | `[status:1][ino:4be]`                                                |
| 5   | TFS_CMD_UNLINK    | `[5][parent:4be][name...NUL]`                                       | `[status:1]`                                                         |
| 6   | TFS_CMD_STAT      | `[6][ino:4be]`                                                      | `[status:1][mode:4be][nlinks:4be][uid:4be][gid:4be][size:4be][mtime:4be][ctime:4be]` |
| 7   | TFS_CMD_MKDIR     | `[7][parent:4be][perm:2][name...NUL]`                               | `[status:1][ino:4be]`                                                |
| 8   | TFS_CMD_RMDIR     | `[8][parent:4be][name...NUL]`                                       | `[status:1]`                                                         |
| 9   | TFS_CMD_READDIR   | `[9][dir_ino:4be][index:4be]`                                       | `[status:1][ino:4be][name:60]`                                       |
| 10  | TFS_CMD_SYNC      | `[10]`                                                              | `[status:1]`                                                         |
| 11  | TFS_CMD_CHMOD     | `[11][ino:4be][perm:2]`                                             | `[status:1]`                                                         |
| 12  | TFS_CMD_RENAME    | `[12][old_parent:4be][new_parent:4be][old_name_len:1][old_name][new_name]` | `[status:1]`                                                  |
| 13  | TFS_CMD_TRUNCATE  | (see code)                                                          | `[status:1]`                                                         |
| 14  | TFS_CMD_SYMLINK   | `[14][parent:4be][tlen:2be][target:tlen][name...]`                  | `[status:1][ino:4be]`                                                |
| 15  | TFS_CMD_READLINK  | `[15][ino:4be]`                                                     | `[status:1][len:2be][target:len]`                                    |

TFS on-disk version: **2** (Phase 0e — `DIR_NAME_LEN=60`, dir
entries are 64 bytes (`[ino:4be][name:60]`), directories use the
same direct+indirect chain files do, so a single dir holds up to
~131K entries at 4KB blocks. Block-pointer width stays i16; volume
cap is 256 MB; inode count cap is 65535. These are stable through
Phase 9 — see `project_slix_phase0d_done.md`.

### 3.3 tty — `oskit/drivers/tty/tty.lsysl` (port: "tty")

| Num | Name                        | Notes                                |
| --- | --------------------------- | ------------------------------------ |
| 1   | TTY_CMD_WRITE               | `[1][console:1][data...]` → `[status:1]` |
| 2   | TTY_CMD_READ                | `[2][console:1]` → `[status:1][data...]`. Defers if console empty. |
| 3   | TTY_CMD_PUTS                | (see code)                           |
| 4   | TTY_CMD_SWITCH              | `[4][console:1]` → `[status:1]`      |
| 5   | TTY_CMD_POLL                | `[5][console:1]` → `[status:1][bits:4be][fire_seq:2be]`. Per-fd EPOLLET counter. |
| 6   | TTY_CMD_EPOLL_SUB           | (see code)                           |
| 7   | TTY_CMD_EPOLL_UNSUB         | (see code)                           |
| 8   | TTY_CMD_EPOLL_INST_CLOSE    | (see code)                           |

### 3.4 pm — `oskit/servers/pm.lsysl` (port: "pm")

| Num | Name                  | Notes                          |
| --- | --------------------- | ------------------------------ |
| 1   | PM_CMD_WAITPID        | `[1][pid:4be]` → `[status:1][exit:4be]` |
| 2   | PM_CMD_KILL           | `[2][pid:4be]` → `[status:1]` |
| 3   | PM_CMD_SPAWN          | (see code)                     |
| 4   | PM_CMD_SIGNAL         | (see code)                     |
| 5   | PM_CMD_SET_FG         | (see code)                     |
| 6   | PM_CMD_SIGINT         | (see code)                     |
| 7   | PM_CMD_SPAWN_SUSP     | (see code)                     |
| 8   | PM_CMD_TRANSPLANT_FD  | (see code)                     |
| 9   | PM_CMD_RESUME         | (see code)                     |

PHASE 1 NOTE. `PM_CMD_SPAWN` currently uses `load_tof_to_ptbr`
(the TRB-format loader). It will be replaced by an ELF loader +
PT_INTERP path that drops VMAs and fault-fills as part of Phase
1's `execve` work. The command number and wire shape stay; the
*meaning* of the args (file path → ELF interp resolution) changes.

### 3.5 vfs — `oskit/servers/vfs.lsysl` (port: "fs")

| Num | Name                            |
| --- | ------------------------------- |
| 1   | VFS_CMD_OPEN                    |
| 2   | VFS_CMD_READ                    |
| 3   | VFS_CMD_WRITE                   |
| 4   | VFS_CMD_CREATE                  |
| 5   | VFS_CMD_UNLINK                  |
| 6   | VFS_CMD_STAT                    |
| 7   | VFS_CMD_MKDIR                   |
| 8   | VFS_CMD_RMDIR                   |
| 9   | VFS_CMD_READDIR                 |
| 10  | VFS_CMD_SYNC                    |
| 11  | VFS_CMD_CHMOD                   |
| 12  | VFS_CMD_RENAME                  |
| 13  | VFS_CMD_CLOSE                   |
| 14  | VFS_CMD_SEEK                    |
| 15  | VFS_CMD_CONNECT                 |
| 16  | VFS_CMD_INHERIT                 |
| 17  | VFS_CMD_EXIT                    |
| 18  | VFS_CMD_PIPE                    |
| 19  | VFS_CMD_ACCEPT                  |
| 20  | VFS_CMD_POLL                    |
| 21  | VFS_CMD_EPOLL_SUB               |
| 22  | VFS_CMD_EPOLL_UNSUB             |
| 23  | VFS_CMD_EPOLL_INST_CLOSE        |
| 24  | VFS_CMD_FSTAT                   |
| 25  | VFS_CMD_DUP_HANDLE              |
| 26  | VFS_CMD_OFT_QUEUE_REF_BY_HANDLE |
| 27  | VFS_CMD_OFT_QUEUE_REF           |
| 28  | VFS_CMD_REGISTER_FD_BY_OFT      |
| 29  | VFS_CMD_TRUNCATE                |

VFS_CMD_OPEN reply protocol (post Phase 0c chunk 4): byte 0 is a
discriminated status — `0` = success (followed by `[handle:4][ino:4]`),
`1` = ENOENT (path missing in backend), `2` = EMFILE (OFT slot
pool or per-process handle table exhausted). The shim's `sys_openat`
maps each surface to the matching errno; in particular, status `2`
must skip the `O_CREAT`-then-retry path so a transient slot
shortage doesn't masquerade as "file does not exist". Older clients
that still use `if reply[0] != 0` continue to work — both error
codes trigger the non-zero branch.

PHASE 3 NOTE. The VFS server today is a routing-only shim that
forwards file ops to TFS and pipe/TCP ops to inet/unix. The
`oft_*` arrays are bring-up scaffolding. Phase 3 replaces this
with a proper vnode/inode/dentry layer (master-roadmap invariant
7). Existing command numbers stay; new ones (mount, open with O_*,
fcntl(F_DUPFD), xattr family) come in.

### 3.6 ds — `oskit/servers/ds.lsysl` (port: "ds")

Data-store (shared key/value with subscribe). Pre-Phase-3 stand-in
for procfs/sysfs.

| Num | Name              |
| --- | ----------------- |
| 1   | DS_CMD_PUBLISH    |
| 2   | DS_CMD_RETRIEVE   |
| 3   | DS_CMD_DELETE     |
| 4   | DS_CMD_SUBSCRIBE  |

### 3.7 nic — `oskit/servers/nic.lsysl` (port: "nic")

| Num | Name                        | Notes                                                |
| --- | --------------------------- | ---------------------------------------------------- |
| 1   | NIC_CMD_GET_MAC             | `[1]` → `[status:1][mac:6]`                          |
| 2   | NIC_CMD_SEND_PACKET         | `[2][len:2be][frame:len]` → `[status:1]`             |
| 3   | NIC_CMD_SUBSCRIBE_RX        | `[3]` → `[status:1]`. Subscriber gets notify_send on RX. |
| 4   | NIC_CMD_RECV_PACKET         | `[4]` → `[status:1][len:2be][frame:len]` or `[status:-1][len:0]` if empty |
| 5   | NIC_CMD_DEBUG_DROP_NEXT     | (test injector)                                      |
| 9   | NIC_CMD_DEBUG_DROP_MASK     | (test injector)                                      |

### 3.8 inet — `oskit/servers/inet.lsysl` + `inet_proto.lsysl` (port: "inet")

UDP / TCP / ICMP / config / polling. 59 commands total. All
replies start with `[status:1]`; success codes are 0 (or the slot
number for SOCKET); failures are negative errnos.

| Num | Name                            | Group     |
| --- | ------------------------------- | --------- |
| 1   | INET_CMD_SOCKET                 | UDP       |
| 2   | INET_CMD_BIND                   | UDP       |
| 3   | INET_CMD_SENDTO                 | UDP       |
| 4   | INET_CMD_RECVFROM               | UDP       |
| 5   | INET_CMD_CLOSE                  | shared    |
| 10  | INET_CMD_TCP_CONNECT            | TCP       |
| 11  | INET_CMD_TCP_SEND               | TCP       |
| 12  | INET_CMD_TCP_RECV               | TCP       |
| 13  | INET_CMD_TCP_CLOSE              | TCP       |
| 14  | INET_CMD_TCP_LISTEN             | TCP       |
| 15  | INET_CMD_TCP_ACCEPT             | TCP       |
| 16  | INET_CMD_SET_IP_CONFIG          | config    |
| 17  | INET_CMD_GET_IP_CONFIG          | config    |
| 18  | INET_CMD_RECVFROM_TIMEOUT       | UDP       |
| 19  | INET_CMD_ARP_PROBE              | util      |
| 20  | INET_CMD_TCP_DEBUG              | util      |
| 21  | INET_CMD_ICMP_SEND              | ICMP      |
| 22  | INET_CMD_ICMP_CHECK             | ICMP      |
| 23  | INET_CMD_PID_EXIT               | mgmt      |
| 24  | INET_CMD_UDP_GETNAME            | mgmt      |
| 25  | INET_CMD_TCP_GETNAME            | mgmt      |
| 26  | INET_CMD_TCP_RECV_NB            | TCP NB    |
| 27  | INET_CMD_TCP_SHUTDOWN           | TCP       |
| 28  | INET_CMD_POLL                   | epoll     |
| 29  | INET_CMD_EPOLL_SUB              | epoll     |
| 30  | INET_CMD_EPOLL_UNSUB            | epoll     |
| 31  | INET_CMD_EPOLL_INST_CLOSE       | epoll     |
| 32  | INET_CMD_TCP_ACCEPT_NB          | TCP NB    |
| 33  | INET_CMD_TCP_CONNECT_NB         | TCP NB    |
| 34  | INET_CMD_TCP_SETBUF             | TCP opt   |
| 35  | INET_CMD_TCP_GETBUF             | TCP opt   |
| 36  | INET_CMD_TCP_ABORT_CLOSE        | TCP opt   |
| 37  | INET_CMD_TCP_SET_KEEPALIVE      | TCP opt   |
| 38  | INET_CMD_ICMP_UNREACH_INJECT    | test      |
| 39  | INET_CMD_TCP_SET_KEEPOPT        | TCP opt   |
| 40  | INET_CMD_TCP_GET_KEEPOPT        | TCP opt   |
| 41  | INET_CMD_TCP_GET_ERR            | TCP opt   |
| 42  | INET_CMD_TCP_INJECT_FAIL        | test      |
| 43  | INET_CMD_TCP_GET_INFO           | TCP opt   |
| 44  | INET_CMD_IP_REASM_SELFTEST      | test      |
| 45  | INET_CMD_IP_REASM_OVERLAP_TEST  | test      |
| 46  | INET_CMD_IP_REASM_TIMEOUT_TEST  | test      |
| 47  | INET_CMD_TCP_SEND_PARK_TEST     | test      |
| 48  | INET_CMD_TCP_ARP_REPLAY_TEST    | test      |
| 49  | INET_CMD_PID_EXIT_SELFTEST      | test      |
| 50  | INET_CMD_GET_STATS              | stats     |
| 51  | INET_CMD_FRAME_POOL_STATS       | stats     |
| 52  | INET_CMD_TCP_SET_USERTO         | TCP opt   |
| 53  | INET_CMD_TCP_BLACKHOLE_ACKS_TEST | test     |
| 54  | INET_CMD_PMTU_INJECT_TEST       | test      |
| 55  | INET_CMD_FRAG_EGRESS_COUNT      | stats     |
| 56  | INET_CMD_PMTU_DISCOVER_INJECT   | test      |
| 57  | INET_CMD_TCP_DATA_SEG_COUNT     | stats     |
| 58  | INET_CMD_ADD_OWNER              | refcount  |
| 59  | INET_CMD_QUEUE_REF              | refcount  |
| 99  | INET_CMD_TCP_OOO_SELFTEST       | test      |

NB ("non-blocking") variants return `-EAGAIN` instead of parking.
TCP/UDP both register subscribers via `*_EPOLL_SUB` / poll via
`*_POLL`; reply layout for POLL is `[status:1][bits:4be][fire_seq:2be]`.

### 3.9 unix — `oskit/servers/unix.lsysl` (port: "unix")

AF_UNIX SOCK_DGRAM / SOCK_STREAM, abstract namespace, socketpair,
SCM_RIGHTS. 28 commands.

| Num | Name                         | Group     |
| --- | ---------------------------- | --------- |
| 1   | UNIX_CMD_SOCKET              | base      |
| 2   | UNIX_CMD_BIND                | base      |
| 3   | UNIX_CMD_CONNECT             | base      |
| 4   | UNIX_CMD_LISTEN              | base      |
| 5   | UNIX_CMD_ACCEPT              | base      |
| 6   | UNIX_CMD_SENDTO              | base      |
| 7   | UNIX_CMD_RECVFROM            | base      |
| 8   | UNIX_CMD_SEND                | base      |
| 9   | UNIX_CMD_RECV                | base      |
| 10  | UNIX_CMD_CLOSE               | base      |
| 11  | UNIX_CMD_PID_EXIT            | lifecycle |
| 12  | UNIX_CMD_SENDMSG             | msg       |
| 13  | UNIX_CMD_RECVMSG             | msg       |
| 14  | UNIX_CMD_ACCEPT_NB           | NB        |
| 15  | UNIX_CMD_RECVFROM_NB         | NB        |
| 16  | UNIX_CMD_RECV_NB             | NB        |
| 17  | UNIX_CMD_POLL                | epoll     |
| 18  | UNIX_CMD_EPOLL_SUB           | epoll     |
| 19  | UNIX_CMD_EPOLL_UNSUB         | epoll     |
| 20  | UNIX_CMD_EPOLL_INST_CLOSE    | epoll     |
| 21  | UNIX_CMD_RECVMSG_NB          | NB        |
| 22  | UNIX_CMD_GETSOCKNAME         | getter    |
| 23  | UNIX_CMD_GETPEERNAME         | getter    |
| 24  | UNIX_CMD_SOCKETPAIR          | adv       |
| 25  | UNIX_CMD_SHUTDOWN            | adv       |
| 26  | UNIX_CMD_PEERCRED            | adv       |
| 27  | UNIX_CMD_DISCONNECT          | adv       |
| 28  | UNIX_CMD_ADD_OWNER           | adv       |

`UNIX_CMD_RECVFROM` / `UNIX_CMD_RECVFROM_NB` carry a 1-byte
`peek` flag (post-0a-2). `UNIX_CMD_RECVMSG` / `UNIX_CMD_RECVMSG_NB`
do **not** support MSG_PEEK yet — see master-roadmap Phase 3
(0a-2b deferred; SCM_RIGHTS peek requires fd-aliasing primitive).

### 3.10 rs — `oskit/servers/rs.lsysl` (port: implicit, "rs")

RS is the reincarnation server (Minix lineage). It boots
servers from boot-info modules, restarts crashed servers, and
hands out port lookups before the lookup-table is populated.

`rs_ipc_mask_for_idx` controls which servers may send to which
others; new server-to-server pairs need an entry here or the send
silently fails (see auto-memory `feedback_rs_ipc_mask_fanout`).

## 4. Memory layout map

### 4.1 Cross-arch fixed addresses

From `oskit/config/config.sysl`:

| Symbol             | Value          | Use                                   |
| ------------------ | -------------- | ------------------------------------- |
| PAGE_SIZE          | 4096           | universal                             |
| THREAD_STACK_SIZE  | 16384          | per-kernel-thread stack (BSS slot)    |
| THREAD_STACK_HALF  | 8192           | user/super split inside a kernel slot |
| KERNEL_L1_BASE     | 0x7FE000       | kernel page-table root                |
| PAGE_POOL_BASE     | 0x400000       | physical page pool start              |
| PAGE_POOL_END      | 0x7FD000       | physical page pool end                |
| BOOT_INFO_ADDR     | 0x600000       | boot-info header VA                   |
| MAX_THREADS        | 32             | kernel thread slots                   |
| MAX_PROCESSES      | 16             | PT slots                              |
| MAX_PORTS          | 16             | IPC port slots                        |

### 4.2 Per-arch program layout

Source: `oskit/arch/<arch>/prog_config.sysl`.

| Arch     | PROG_USP    | PROG_SSP    | SRV_USP    | SRV_SSP    |
| -------- | ----------- | ----------- | ---------- | ---------- |
| x86_64   | 0x60090000  | 0x60080000  | 0xD4000    | 0xD3000    |
| aarch64  | 0x60090000  | 0x60080000  | 0x60090000 | 0x60080000 |
| trisc    | 0xD0000     | 0xCF000     | 0xD0000    | 0xCF000    |

PHASE 1 NOTE. **All four columns disappear in Phase 1**. The
per-process VMA list will allocate stack VMAs from a free range
and return the initial SP from the kernel's exec path. Server
pages remain identity-mapped in the meantime, but their layout
moves into a generic device-process model in Phase 5 (master-
roadmap invariant 11).

### 4.3 Cross-arch invariants

- **Guard page (x86_64 only).** Page `0xCB` is left non-present
  in every process PT. Stack overflow past the bottom of the
  legacy stack region (`PROG_SSP=0xCC000`-era) faults instead of
  silently corrupting low memory. TRISC has no guard because its
  kernel loads at `0x0` and BSS extends into low pages. aarch64's
  high carve-out makes a guard unnecessary.
- **Kernel thread stacks.** `kernel_stacks: [32][16384]byte` lives
  in BSS. Static. Tied to MAX_THREADS × THREAD_STACK_SIZE.
  Replacement = "alloc_stack(size)" API in Phase 1.
- **Boot-info header.** Lives at `0x600000` (x86 + trisc). Magic
  `"SLIX"` + module count + per-module entries. Servers consume
  it via the loader code in `oskit/loader/`.
- **MMIO grants.** A server is created via `vm_create_server_pt`
  with a single MMIO page granted; the server sees the page at
  `MMIO_SERVER_VA = 0x60100000` (aarch64) or its arch-specific
  equivalent. virtio-net slot offset is `0xe00` from this base on
  aarch64 virt.

## 5. Phase 1 replacement targets (recap)

These items are PHASE 1 REPLACEMENT TARGETS. The numbers stay,
the semantics change. Do not extend them; do not add new
fixed-region state.

- `oskit/arch/x86_64/prog_config.sysl::PROG_USP/PROG_SSP`
- `oskit/arch/aarch64/prog_config.sysl::PROG_USP/PROG_SSP`
- `oskit/arch/trisc/prog_config.sysl::PROG_USP/PROG_SSP`
- `oskit/arch/x86_64/vm.lsysl::vm_create_process_pt` (eager mapping)
- `oskit/arch/aarch64/vm.lsysl::vm_create_process_pt` (eager mapping
  + `USER_CODE_PAGES` / `USER_STACK_PAGES` / `USER_STACK_L3_START`)
- `oskit/arch/trisc/vm.lsysl::vm_create_process_pt` (eager mapping)
- `oskit/kernel/kernel.lsysl::kernel_stacks` (static BSS array)
- `PM_CMD_SPAWN` semantics — TOF loader → ELF + PT_INTERP

## 6. Procedure for adding to the ABI

1. Add the new constant in the canonical source file (server, shim,
   or `services.lsysl`). Use the next free number in the relevant
   range (do not reorder existing numbers).
2. Update this document in the same commit.
3. Add a regression test under `oskit/bin/test_*.lsysl` (or in
   `Aarch64NshTests` / `X86NshTests` for shim-syscall changes).
   Per CLAUDE.md rule 9, every new feature ships with a test.
4. If the new command is server-to-server, add the sender→receiver
   entry to `rs_ipc_mask_for_idx` (see `feedback_rs_ipc_mask_fanout`
   in auto-memory). Otherwise the send silently fails.
5. Build + run NshTests on **both arches** before commit.

## 7. Document version history

- **2026-05-02 / slix@5f0804a9f** — Phase 0b lockdown, initial
  cut. Captures kernel ABI through SYS_FD_TRANSPLANT (85), POSIX
  shim through writev (372), all 9 servers as of post-0a-3.
- **2026-05-02 / Phase 1 chunk 1** — kernel ABI extended with
  SYS_VMA_SELFTEST (86, debug-only). New `oskit/kernel/vma.lsysl`
  defines a per-process VMA red-black tree; `Process` struct grows
  a `vma_tree: VMATree` field. No kernel codepath consults the
  tree yet — chunks 2 and 3 wire it into page-fault handling.
- **2026-05-02 / Phase 1 chunk 2** — page-fault path now consults
  the VMA tree. Both arch fault handlers (x86_64 ISR vector 14,
  aarch64 EC=0x20/0x24 EL0 abort) call into
  `kernel_handle_user_fault` → `vma_handle_fault`. On a hit for
  an anonymous VMA the handler allocates a zero page and installs
  it via the new `vm_install_user_page(ptbr, vaddr, paddr, writable)`
  helper, then returns to retry the faulting instruction; on a
  miss the legacy "kill on fault" path runs unchanged. Adds debug
  syscall SYS_VMA_CREATE (87) so test programs can register a VMA
  before `mmap` lands in chunk 4. No COW / file-backed support yet
  (chunks 5 + 7).
- **2026-05-07 / Phase 1 chunk 4** — userspace `mmap` (222) /
  `munmap` (215) / `mprotect` (226) for anonymous mappings. The
  shim layer routes to new kernel-side helpers
  `kernel_mmap_anon` / `kernel_munmap` / `kernel_mprotect` which
  wrap the existing VMA tree ops with edge-splitting + frame
  freeing. User mmap arena lives at `[0x60100000, 0x60200000)` —
  inside the existing 2MB user PT, so no intermediate-PT
  allocation is needed (chunk 7 will lift that limit). `mremap`
  (216) intentionally returns -ENOMEM so musl's realloc-via-
  mremap path falls back to mmap+memcpy+munmap. Two arch helpers
  added: `vm_unmap_user_page(ptbr, vaddr) -> u64` (clear PTE +
  return previously-installed PA) and `vm_update_user_prot(ptbr,
  vaddr, writable, executable) -> int` (re-flag an existing
  PTE). The aarch64 `vm_install_user_page` was simultaneously
  fixed to honour `writable=0` via AP=11 (was silently RW
  regardless of arg) — without this fix `mprotect(PROT_READ)`
  would be a no-op on aarch64.
- **2026-05-07 / Phase 1 chunk 5** — copy-on-write infrastructure
  for anonymous mappings. New `oskit/kernel/page_refcnt.lsysl`
  carries a per-physical-frame byte refcount (256 KB BSS,
  indexable up to 1 GB above pool base). User-data-page wrappers
  `kalloc_user_page` / `kfree_user_page` hand out frames at
  refcount=1 and only release them to the page pool at
  refcount=0; PT/PD/PDPT/kstack pages remain refcount-unaware to
  keep the `oskit.kernel` ↔ `oskit.arch` dependency boundary
  acyclic. `vma_handle_fault` now takes a COW branch when the
  faulting PTE is already present — write fault on a RO PTE in a
  MAP_PRIVATE VMA allocates a fresh frame, copies the contents,
  installs the new frame writable, and decrements the original
  frame's refcount. Two debug syscalls plumb the test program:
  SYS_COW_REFCNT_SELFTEST (89) runs the kernel-side refcount
  invariants and SYS_COW_SHARE_SELF (90) plumbs `va_dst` to the
  same physical frame as `va_src` (both PTEs RO, refcount += 1)
  so a userspace test can drive the COW path without yet having
  `fork()` (chunk 6).
- **2026-05-08 / Phase 1 chunk 6** — `fork()` (SYS_FORK = 91).
  Kernel-direct syscall (no PM round-trip) clones the calling
  process: fresh PTBR via `vm_create_process_pt`, deep copy of
  the parent's VMA tree via new `vma_clone_for_fork`, and shared
  RO mappings of every currently-resident user page with
  refcount-bumped frames. New `arch_setup_fork_frame` (both
  arches) builds the child's saved exception frame as a byte
  copy of the parent's `syscall_ssp` frame with the syscall-
  return register zeroed, so when the scheduler picks up the
  child it ERETs/iretqs straight back to user mode at the
  parent's post-syscall PC with `rc=0`; the parent gets the
  child's PID via the dispatcher's normal return-write path.
  Three adjacent fixes ride this commit: (1) `kfree_user_page`
  falls through to `page_free` for in-pool frames at refcount=0
  so chunk-3's raw-allocated `vm_copy_to` frames don't leak when
  reap walks the VMA tree; (2) new `vma_unmap_all_pages` (called
  from `reap_process` before `vm_free_process_pt`) walks every
  VMA range, clears each leaf PTE, and drops the process's
  refcount on each frame — fixes a pre-existing leak of mmap'd
  pages on process exit; (3) new `arch_fork_clone_eager` byte-
  copies the parent's legacy 0xCC..0xFF code region into the
  child on x86 (no-op on aarch64) so forked children resume at
  parent's post-syscall PC instead of trapping on the freshly-
  allocated zero pages of the eager region. Migrating x86 user
  programs to link at `0x60000000` (matching aarch64) is the
  cleaner long-term path — once that lands the helper becomes
  universally a no-op.
- **2026-05-08 / Phase 1 chunk 7** — ELF parsing infrastructure
  (`SYS_ELF_SELFTEST` = 92). New `oskit.lib.elf` module exposes
  ELF64 magic/class validation, header-field accessors
  (`elf64_entry`, `elf64_phoff`, `elf64_phentsize`, `elf64_phnum`)
  and PHT-entry accessors (`elf64_ph_type/flags/offset/vaddr/`
  `filesz/memsz/align`) plus an `elf64_pf_to_vma_prot`
  bit-translator. Pure parsing — no I/O, no kernel imports —
  so reusable from a kernel-side selftest, PM's cross-PTBR
  loader, and chunk 8's `execve()`. The loader's
  `load_elf64_to_ptbr` was refactored to use the new module
  (no behavior change). The kernel-side `sys_elf_selftest`
  handler synthesizes a two-LOAD-segment ELF in BSS, drives
  every accessor against it, and returns `0` or the failing-
  step number — locks down the parser before any real ELF flows
  through it. The chunk explicitly defers actually *installing*
  per-LOAD-segment VMAs in a target address space; that lives
  with chunk 8 (`execve`) where pid-by-definition is the calling
  process and the VMA install is part of dropping/replacing
  the address space.
