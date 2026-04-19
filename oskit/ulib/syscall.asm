; Entry stub + syscall trampoline for external programs.
; Identical layout to boot.asm — each standalone program
; gets its own copy since they're linked independently.
;
; ABI: r1 = syscall number, arg on stack → r2
; Returns result in r1.

segment code

extern oskit_ulib__sysl_start

global _start, func
entry _start

; Program entry. PM has written the POSIX argv blob at 0xBF000:
;   +0   argc (i32)
;   +4   padding
;   +8   argv[0] pointer (i64, absolute)
;   ...
;   +8+8*argc  NULL terminator
; Hands (argc, &argv[0]) to sysl_start, which wraps the C-style
; argv into a sysl `[]string` and calls the program's main.
_start
    movi r2, 0xBF000
    ldw  r1, r2, r0              ; r1 = argc (sign-extended i32)
    movi r2, 0xBF008             ; cargv = &argv[0]
    pshd r2                       ; push 2nd arg (cargv) on stack
    movi r4, oskit_ulib__sysl_start
    jalr r6, r4
    addi r7, r7, 8                ; clean up stack arg (sysl_start normally exits)

    ; Fallback if sysl_start ever returns: exit via SYS_EXIT trap.
    addi r2, r0, 0                ; exit code 0 on stack
    pshd r2
    addi r1, r0, 3                ; SYS_EXIT = 3
    trap 0
    halt

global syscall, func

syscall
    ldd  r2, r7, r0     ; load second arg from caller's stack into r2
    trap 0               ; enter kernel: r1 = number, r2 = arg
    jalr r0, r6          ; return to caller (result in r1)

; malloc/free are provided by posix/stdlib/alloc, linked into the
; external program binary. The codegen emits extern refs to these
; for any module with string types.
