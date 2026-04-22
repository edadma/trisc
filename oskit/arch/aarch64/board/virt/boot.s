// aarch64 boot stub for QEMU virt machine.
//
// Boot protocol (Linux aarch64 boot, followed by QEMU -kernel):
//   - CPU enters at EL1 (when run with default -cpu cortex-a72, no virtualization)
//   - x0 = physical address of device tree blob (DTB)
//   - x1, x2, x3 = 0 (reserved)
//   - MMU and caches are off
//   - PSTATE.DAIF = all IRQs masked
//
// This stub sets up a stack, zeroes BSS, then calls kernel_main().
// If kernel_main returns it spins in wfi forever.

.section .text.boot
.global _start

_start:
    // Save DTB pointer (x0) into a well-known global so higher-level
    // code can later walk the device tree if it wants to.
    adrp x1, dtb_addr
    add  x1, x1, #:lo12:dtb_addr
    str  x0, [x1]

    // Set up the stack (grows downward from stack_top).
    adrp x1, stack_top
    add  x1, x1, #:lo12:stack_top
    mov  sp, x1

    // Install the exception vector table. Any fault, IRQ or FIQ
    // from here on routes through `exception_report` in vectors.s
    // instead of silently jumping to whatever was at VBAR_EL1 reset.
    adrp x1, vector_table
    add  x1, x1, #:lo12:vector_table
    msr  vbar_el1, x1
    isb

    // Zero BSS: memset(&__bss_start, 0, __bss_end - __bss_start).
    adrp x1, __bss_start
    add  x1, x1, #:lo12:__bss_start
    adrp x2, __bss_end
    add  x2, x2, #:lo12:__bss_end
1:
    cmp  x1, x2
    b.hs 2f
    str  xzr, [x1], #8
    b    1b
2:

    // Turn the MMU on with an identity map before handing off to
    // sysl. After mmu_init the CPU is running cached Normal memory
    // at 0x40000000+ and Device memory for the MMIO region below.
    bl   mmu_init

    bl   kernel_main

halt:
    wfi
    b    halt

// thread_exit — trampoline a thread returns to when its entry function
// falls off the end. Issues SYS_EXIT (SLIX native syscall 3) with status
// 0. The dispatcher's exit path must not return.
.global thread_exit
thread_exit:
    mov x8, #3               // SLIX native SYS_EXIT
    mov x0, #0               // status
    svc #0
1:  b   1b                   // exit never returns; spin if it does

.section .bss
.align 4                     // 16-byte align (aarch64 SP alignment rule)
stack_bottom:
    .skip 16384              // 16 KB boot stack
stack_top:

.align 3
.global dtb_addr
dtb_addr:
    .quad 0
