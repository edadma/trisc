# ============================================================================
# x86_64 program startup — entry point for standalone SLIX binaries
# ============================================================================
# Reads argc/argv from the POSIX blob PM placed at PROG_ARGS_ADDR=0xBF000,
# calls sysl_start (in ulib), then exits.

.section .text
.code64

.set PROG_ARGS_ADDR, 0xBF000

# Entry point. Layout at PROG_ARGS_ADDR:
#   +0   argc (i32)
#   +4   padding
#   +8   argv[0] pointer
#   ...
#   +8+8*argc  NULL
#   +16+8*argc string data
.global _start
_start:
    andq $-16, %rsp
    movabs $PROG_ARGS_ADDR, %rax
    movl (%rax), %edi              # argc → rdi
    leaq 8(%rax), %rsi             # &argv[0] → rsi
    call sysl_start
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

# thread_exit — called when a thread function returns
.global thread_exit
thread_exit:
    movq $3, %rdi          # SYS_EXIT
    movq $0, %rsi
    int $0x80
    hlt
