# ============================================================================
# x86_64 program startup — entry point for standalone SLIX binaries
# ============================================================================
# Provides: _start (calls main, then exits), syscall, thread_exit

.section .text
.code64

# Entry point: align stack for ABI, call main() then exit.
# iretq sets RSP to usp-8 (mod 16 = 8), but call main needs
# RSP mod 16 = 0 so that at main's entry RSP mod 16 = 8.
.global _start
_start:
    andq $-16, %rsp
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
