// aarch64 program startup — entry point for standalone SLIX binaries.
//
// SLIX uses the System V Application Binary Interface process-
// initialization layout (same as Linux / NetBSD / Minix 3). PM
// builds the frame on the new process's stack before resuming it,
// so at entry SP_EL0 points at argc:
//
//   [sp + 0x00]   argc          (i64)
//   [sp + 0x08]   argv[0]       (pointer)
//   ...
//   [sp + 8+8*argc]  NULL        (argv terminator)
//   [sp + 16+8*argc] envp[0]...  NULL (envp terminator)
//   [sp + after envp] auxv pairs, AT_NULL terminated
//   [sp + after auxv] string data argv/envp point into
//
// Hands (argc, &argv[0]) to sysl_start, which wraps the C-style
// argv into a sysl `[]string` and calls the program's main.

.section .text
.global _start

_start:
    ldr  w0, [sp]                // argc (low 32 bits of the i64 slot)
    add  x1, sp, #8              // &argv[0]
    bl   oskit_ulib__sysl_start

    // sysl_start normally exits via syscall(SYS_EXIT, 0). If it
    // returns anyway, issue our own SYS_EXIT to halt cleanly.
    mov x8, #3                   // SYS_EXIT
    mov x0, #0
    svc #0
1:  b   1b

// syscall(number: int, arg: i64) -> i64
//   AAPCS64 call site: x0 = number, x1 = arg.
//   SLIX dispatcher reads x8 = number, x0 = arg.
//   Shuffle + SVC; x0 already carries the return value on resume.
.global syscall
syscall:
    mov x8, x0
    mov x0, x1
    svc #0
    ret

// thread_exit — sysl build_stack_frame pre-loads x30 with this so
// that returning from a thread's entry function halts cleanly
// via SYS_EXIT instead of falling off the end of the stack.
.global thread_exit
thread_exit:
    mov x8, #3                   // SYS_EXIT
    mov x0, #0
    svc #0
1:  b   1b
