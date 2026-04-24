// aarch64 server startup — entry point for standalone SLIX servers.
//
// Servers don't use the ulib argv/sysl_start path: they query their
// config (RS TID + ramdisk info) via sys_getinfo inside their own
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

// syscall6(number, a0, a1, a2, a3, a4, a5) -> i64
// AAPCS64 call site: x0=num, x1=a0, x2=a1, x3=a2, x4=a3, x5=a4, x6=a5.
// 6-arg dispatcher reads x8=num and x0..x5=a0..a5 from the saved frame.
.global syscall6
syscall6:
    mov x8, x0
    mov x0, x1
    mov x1, x2
    mov x2, x3
    mov x3, x4
    mov x4, x5
    mov x5, x6
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
