.global _start

_start:
    ldr x0, =_stack_top
    mov sp, x0
    bl main
hang:
    wfi
    b hang
