# ============================================================================
# x86_64 program startup — entry point for standalone SLIX binaries
# ============================================================================
# Provides: _start (calls main, then exits), syscall, thread_exit

.section .text
.code64

# Entry point: call main() then exit
.global _start
_start:
    call main
    movq $3, %rdi          # SYS_EXIT
    movq $0, %rsi          # exit code 0
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
