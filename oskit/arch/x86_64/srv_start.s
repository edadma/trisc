# ============================================================================
# x86_64 server startup — entry point for isolated SLIX servers
# ============================================================================
# Servers don't take argv; they read their init info from the RS info
# page at 0xBF000 via their own main() wrapper.

.section .text
.code64

.global _start
_start:
    andq $-16, %rsp
    call main
    movq $3, %rdi                  # SYS_EXIT
    movq $0, %rsi
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
