# RS Crash Recovery Design

Minix 3-style server crash detection and restart. RS monitors its child
servers and restarts them from boot modules when they die.

## Detection Mechanism

PM notifies RS via `notify_send_to(rs_tid, dead_pid)` when a server
process dies (exit, kill, or MMU fault). No signals needed — uses the
existing notification system. PM checks if the dead process's parent
is RS before sending.

## Recovery Flow

1. Server crashes (killed, faults, exits unexpectedly)
2. Kernel marks process as zombie, notifies PM
3. PM's `pm_handle_exit` detects parent == RS, sends notification
4. RS wakes in `rs_monitor()`, reads dead PID from notification
5. RS looks up which server died by PID (tracked in `rs_server_pids[]`)
6. RS reloads the server's boot module via `svc_phys_read`
7. RS calls `rs_start_from_module()` to create a fresh process
8. The new server re-registers its IPC port (same name as before)
9. RS waits for `rs_ready()`, logs restart

## Stale IPC Port Handling

When a server dies, its IPC port remains registered. When the restarted
server calls `ipc_port_register` with the same name, the kernel clears
any existing port with that name first. This ensures `port_lookup`
returns the new server's port, not the dead one.

## Implementation Changes

### kernel_kill_process (kernel.lsysl)

Currently reaps the process immediately, bypassing PM. Fix: set
`PROC_ZOMBIE` and `notify_send(kernel_pm_tid, pid)` instead, matching
how normal thread exit works. This also fixes a pre-existing bug where
`kill` skips PM's VFS handle cleanup.

### RS PID tracking (rs.lsysl)

New module-level arrays:
- `rs_server_pids[6]` — PID of each server (by boot order index)
- `rs_server_io_page[6]` — MMIO page grant for restart

`rs_start_from_module` returns the PID (was: 1). Boot loop stores PIDs.

### PM notification (pm.lsysl)

In `pm_handle_exit`, after processing the dead process, check if its
parent TID == RS TID. If so, `notify_send_to(rs_tid, dead_pid)`.

### ipc_port_register_handler (ipc.lsysl)

Before registering a new name, scan all ports and clear any existing
port with the same name. This prevents stale ports from dead servers
shadowing the new registration.

### rs_monitor (rs.lsysl)

Replace the stub loop with crash recovery logic:
```
rs_monitor()
    while true
        ipc_recv_notify(rs_port, &buf[0], 1)
        val dead_pid = notify_read_self()
        if dead_pid > 0
            rs_handle_crash(dead_pid)

rs_handle_crash(dead_pid)
    // Find server index by PID
    // Skip init (index 5) — don't restart
    // Reload boot module, start fresh process
    // Update rs_server_pids[idx]
    // Wait for rs_ready()
```

## Edge Cases

### Notification coalescing

The notification value is a single integer, not a queue. If two servers
die before RS reads the first notification, the second overwrites the
first. Mitigation: after handling each notification, RS scans all
`rs_server_pids` against `svc_proc_state` to catch any missed deaths.

### PM crash

PM is the notification intermediary. If PM itself crashes, RS receives
no notification. PM crash is a fatal system failure in the initial
implementation. Future work: kernel-level fallback notification.

### Init process

Init (boot order index 5) is not auto-restarted. Restarting init would
leave orphaned user processes in an inconsistent state.

### Restart race

If a restarted server crashes before calling `rs_ready()`, RS blocks
forever in `rs_wait_ready()`. Future work: timed wait with watchdog.

## Testing

1. Boot to shell (nsh or login)
2. Run `ps` to find tfs PID
3. `kill <tfs_pid>` to kill the TFS server
4. `cat /etc/ttytab` fails (no TFS server)
5. RS detects death, restarts tfs from boot module
6. `cat /etc/ttytab` works again

Automated via TRISC headless tests (OSKitNshTests) and x86 QEMU
headless tests (X86NshTests).
