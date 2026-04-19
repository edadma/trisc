; Syscall trampoline for isolated servers. Servers use ENTRY main
; (no _start needed — RS info at 0xBF000 read by their own main wrapper).
; Kept separate from oskit/ulib/syscall.asm, which provides the POSIX
; argv _start entry for user programs.
;
; ABI: r1 = syscall number, arg on stack → r2
; Returns result in r1.

segment code

global syscall, func

syscall
    ldd  r2, r7, r0     ; load second arg from caller's stack into r2
    trap 0               ; enter kernel: r1 = number, r2 = arg
    jalr r0, r6          ; return to caller (result in r1)
