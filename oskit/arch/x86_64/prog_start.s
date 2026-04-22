# ============================================================================
# x86_64 program startup — entry point for standalone SLIX binaries
# ============================================================================
# SLIX uses the System V ABI process-initialization stack layout. PM
# builds the frame on the new process's stack before resuming it, so
# at entry RSP points at argc:
#
#   [rsp + 0x00]  argc           (i64)
#   [rsp + 0x08]  argv[0]        (pointer)
#   ...
#   [rsp + 8+8*argc]  NULL        (argv terminator)
#   [rsp + 16+8*argc] envp[0]..., NULL (envp terminator)
#   [rsp + after envp] auxv pairs, AT_NULL terminated
#   [rsp + after auxv] string data
#
# Hands (argc, &argv[0]) to sysl_start, which wraps the C-style argv
# into a sysl `[]string` and calls the program's main.

.section .text
.code64

.global _start
_start:
    movl (%rsp), %edi              # argc (low 32 bits of the i64 slot)
    leaq 8(%rsp), %rsi             # &argv[0]
    call oskit_ulib__sysl_start
    movq $3, %rdi                  # SYS_EXIT
    movq $0, %rsi                  # (sysl_start exits itself; fallback 0)
    int $0x80
    hlt

# syscall(number: int, arg: i64) -> i64
# System V ABI: rdi = number, rsi = arg, returns in rax
.global syscall
syscall:
    int $0x80
    retq

# syscall6(number, a0, a1, a2, a3, a4, a5) -> i64
#
# SysV AMD64 passes 7 args as: rdi=num, rsi=a0, rdx=a1, rcx=a2, r8=a3,
# r9=a4, [rsp+8]=a5 (after the `call`). The SLIX 6-arg dispatcher
# reads num from RDI and a5 from R10, so we only need to lift a5 off
# the stack into r10 before `int $0x80`. Return value is in RAX.
.global syscall6
syscall6:
    movq 8(%rsp), %r10
    int $0x80
    retq

# thread_exit — called when a thread function returns
.global thread_exit
thread_exit:
    movq $3, %rdi          # SYS_EXIT
    movq $0, %rsi
    int $0x80
    hlt
