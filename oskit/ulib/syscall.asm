; Syscall trampoline for external programs.
; Identical to the one in boot.asm — external programs
; get their own copy since they're linked independently.
;
; ABI: r1 = syscall number, arg on stack → r2
; Returns result in r1.

segment code

global syscall, func

syscall
    ldd  r2, r7, r0     ; load second arg from caller's stack into r2
    trap 0               ; enter kernel: r1 = number, r2 = arg
    jalr r0, r6          ; return to caller (result in r1)

; Minimal malloc/free stubs for programs that don't use the heap.
; The codegen emits extern refs to these for any module with string
; types. Programs needing a real allocator link against posix/stdlib.

global malloc, func

malloc
    ldi  r1, 0          ; return null
    jalr r0, r6

global free, func

free
    jalr r0, r6          ; no-op
