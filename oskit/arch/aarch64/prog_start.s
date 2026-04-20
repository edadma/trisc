// aarch64 program startup — entry point for standalone SLIX binaries.
//
// Mirrors oskit/arch/x86_64/prog_start.s. Reads argc/argv from the
// POSIX blob PM writes at PROG_ARGS_ADDR (once PM lands on aarch64;
// for the bare-metal test we pass argc=0 / cargv=null), calls
// `oskit_ulib__sysl_start`, and exits via SYS_EXIT.

.section .text
.global _start

.set PROG_ARGS_ADDR, 0xBF000

// Entry point. Layout at PROG_ARGS_ADDR:
//   +0   argc (i32)
//   +4   padding
//   +8   argv[0] pointer
//   ...
//   +8+8*argc  NULL
//   +16+8*argc string data
//
// On QEMU virt bring-up there's no PM yet, so PROG_ARGS_ADDR isn't
// mapped in the user PT. Guard: if the region is unmapped we pass
// argc=0 and a null cargv so sysl_start falls through to main with
// an empty []string. Once PM lands, swap in the real read.
_start:
    // For now, bypass PROG_ARGS_ADDR read and call with empty argv.
    // (The carve-out at 0x60000000 doesn't include 0xBF000; until
    // the ramdisk loader / PM writes real args, just zero them.)
    mov x0, #0
    mov x1, #0
    bl  oskit_ulib__sysl_start

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
