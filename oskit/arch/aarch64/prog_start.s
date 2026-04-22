// aarch64 program startup — entry point for standalone SLIX binaries.
//
// Mirrors oskit/arch/x86_64/prog_start.s. Reads argc/argv from the
// POSIX blob PM writes to the info page at INFO_PAGE_VA
// (0x60090000 on aarch64, matching oskit.arch.INFO_PAGE_VA), calls
// `oskit_ulib__sysl_start`, and exits via SYS_EXIT.

.section .text
.global _start

.set PROG_ARGS_ADDR, 0x60090000

// Entry point. Layout at PROG_ARGS_ADDR:
//   +0   argc (i32)
//   +4   padding
//   +8   argv[0] pointer
//   ...
//   +8+8*argc  NULL
//   +16+8*argc string data
//
// PM copies this blob into the child's page table before resuming.
// When no PM has run (kernel-direct spawn for bring-up tests), the
// page is still mapped but zero-filled, so argc=0 and sysl_start
// falls through to main() with an empty []string.
_start:
    ldr  x2, =PROG_ARGS_ADDR
    ldr  w0, [x2]                // argc (i32)
    add  x1, x2, #8              // &argv[0]
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
