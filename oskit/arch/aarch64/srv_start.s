// aarch64 server startup — entry point for standalone SLIX servers.
//
// Servers don't use the ulib argv/sysl_start path: they read their
// config from the RS info page at INFO_PAGE_VA inside their own
// generated main() wrapper, so _start just calls main() and exits
// via SYS_EXIT. Mirrors oskit/arch/x86_64/srv_start.s.

.section .text
.global _start

_start:
    bl  main
    mov x8, #3                   // SYS_EXIT
    mov x0, #0
    svc #0
1:  b   1b

// syscall(number: int, arg: i64) -> i64
// AAPCS64: x0 = number, x1 = arg; SLIX ABI: x8 = number, x0 = arg.
.global syscall
syscall:
    mov x8, x0
    mov x0, x1
    svc #0
    ret

// thread_exit — sysl build_stack_frame pre-loads x30 with this so
// that a thread returning from its entry function halts cleanly.
.global thread_exit
thread_exit:
    mov x8, #3                   // SYS_EXIT
    mov x0, #0
    svc #0
1:  b   1b
