# ============================================================================
# SLIX x86_64 Boot — Multiboot → Long Mode → Kernel
# ============================================================================
#
# Flow: QEMU/GRUB loads us in 32-bit protected mode.
# We set up page tables, enable long mode, jump to 64-bit,
# set up IDT + PIC + PIT, then call kernel_init / kernel_main.
#
# Provides to Sysl code (as extern):
#   arch_cli, arch_sti, vm_set_ptbr, vm_flush_tlb, vm_get_ptbr
#
# Expects from Sysl code (kernel):
#   kernel_init, kernel_main, schedule, current_thread
#
# ============================================================================

.set MULTIBOOT_MAGIC, 0x1BADB002
.set MULTIBOOT_FLAGS, 0x00000003
.set MULTIBOOT_CHECKSUM, -(MULTIBOOT_MAGIC + MULTIBOOT_FLAGS)

# PIC ports
.set PIC1_CMD,  0x20
.set PIC1_DATA, 0x21
.set PIC2_CMD,  0xA0
.set PIC2_DATA, 0xA1

# PIT
.set PIT_CH0,  0x40
.set PIT_CMD,  0x43
.set PIT_HZ,   100        # 100 Hz timer (10ms ticks)
.set PIT_DIV,  11932       # 1193182 / 100

# COM1
.set COM1, 0x3F8

# ============================================================================
# Multiboot header
# ============================================================================

.section .multiboot
.align 4
    .long MULTIBOOT_MAGIC
    .long MULTIBOOT_FLAGS
    .long MULTIBOOT_CHECKSUM

# ============================================================================
# BSS — page tables, stacks, IDT
# ============================================================================

.section .bss
.align 4096

# Identity-map page tables
pml4:   .skip 4096
pdpt:   .skip 4096
pd:     .skip 4096

# Kernel stack (16 KB)
.align 16
stack_bottom:
    .skip 16384
.global stack_top
stack_top:

# IDT: 256 entries x 16 bytes = 4096 bytes
.align 16
idt:    .skip 4096

# ============================================================================
# 32-bit entry point
# ============================================================================

.section .text
.code32
.global _start

_start:
    cli
    movl $stack_top, %esp

    # --- Set up identity-map page tables ---

    # PML4[0] -> PDPT
    movl $pdpt, %eax
    orl  $0x03, %eax          # present + writable
    movl %eax, pml4

    # PDPT[0] -> PD
    movl $pd, %eax
    orl  $0x03, %eax
    movl %eax, pdpt

    # PD[0..15] -> identity-map first 32MB via 2MB pages
    # PS bit (0x80) = 2MB page, present + writable + PS
    movl $pd, %edi
    movl $0x00000083, %eax     # 0MB, present+write+PS
    movl $16, %ecx             # 16 entries = 32MB
1:
    movl %eax, (%edi)
    movl $0, 4(%edi)
    addl $0x200000, %eax       # next 2MB
    addl $8, %edi
    loop 1b

    # CR3 = PML4
    movl $pml4, %eax
    movl %eax, %cr3

    # Enable PAE (CR4 bit 5)
    movl %cr4, %eax
    orl  $0x20, %eax
    movl %eax, %cr4

    # Enable long mode (EFER.LME, MSR 0xC0000080 bit 8)
    movl $0xC0000080, %ecx
    rdmsr
    orl  $0x100, %eax
    wrmsr

    # Enable paging (CR0 bit 31)
    movl %cr0, %eax
    orl  $0x80000000, %eax
    movl %eax, %cr0

    # Load 64-bit GDT and far-jump to long mode
    lgdt gdt64_ptr
    ljmp $0x08, $entry64

# ============================================================================
# 64-bit entry point
# ============================================================================

.code64
entry64:
    # Load data segments
    movw $0x10, %ax
    movw %ax, %ds
    movw %ax, %es
    movw %ax, %ss
    movw %ax, %fs
    movw %ax, %gs

    movabs $stack_top, %rsp

    # Enable SSE (required by LLVM-generated code)
    movq %cr0, %rax
    andq $~0x4, %rax          # clear CR0.EM
    orq  $0x2, %rax           # set CR0.MP
    movq %rax, %cr0
    movq %cr4, %rax
    orq  $0x600, %rax         # CR4.OSFXSR + CR4.OSXMMEXCPT
    movq %rax, %cr4

    # Initialize UART, PIC, PIT, IDT
    call oskit_arch_x86_64__runtime_init

    # Call kernel
    call kernel_init
    call kernel_main

    # rax = first thread's SSP (or 0 if no threads)
    testq %rax, %rax
    jz idle_loop

    # Start first thread: switch to its stack and pop context
    movq %rax, %rsp
    jmp restore_context

idle_loop:
    sti
    hlt
    jmp idle_loop

# ============================================================================
# arch_cli / arch_sti — called from Sysl kernel code
# Bare instruction + ret, no frame setup overhead.
# ============================================================================

.global arch_cli
arch_cli:
    cli
    retq

.global arch_sti
arch_sti:
    sti
    retq

# ============================================================================
# outb / inb — x86 port I/O, System V calling convention
# ============================================================================
# outb(port: int, val: byte)  — rdi = port, sil = val
# inb(port: int) -> int       — rdi = port, returns in eax

.global outb
outb:
    movl %edi, %edx
    movb %sil, %al
    outb %al, %dx
    retq

.global inb
inb:
    movl %edi, %edx
    xorl %eax, %eax
    inb %dx, %al
    retq

.global io_wait
io_wait:
    outb %al, $0x80
    retq

# ============================================================================
# load_idt — Load IDT register
# ============================================================================
# rdi = idt table base, esi = limit (e.g. 4095)
# Builds the 10-byte IDTR descriptor on the stack and loads it.

.global load_idt
load_idt:
    subq $16, %rsp
    movw %si, (%rsp)       # limit (16-bit)
    movq %rdi, 2(%rsp)     # base  (64-bit)
    lidt (%rsp)
    addq $16, %rsp
    retq

# ============================================================================
# vm_set_ptbr — Set CR3 (page table base register)
# ============================================================================
# rdi = new page table physical address

.global vm_set_ptbr
vm_set_ptbr:
    movq %rdi, %cr3
    retq

# ============================================================================
# vm_flush_tlb — Flush entire TLB by reloading CR3
# ============================================================================

.global vm_flush_tlb
vm_flush_tlb:
    movq %cr3, %rax
    movq %rax, %cr3
    retq

# ============================================================================
# vm_get_ptbr — Read CR3
# ============================================================================

.global vm_get_ptbr
vm_get_ptbr:
    movq %cr3, %rax
    retq

# ============================================================================
# Context Switch
# ============================================================================
#
# Called from timer ISR with interrupts disabled.
# Saves all GP registers onto current stack, calls schedule()
# with current RSP, gets back next thread's RSP, restores.
#
# Stack frame (pushed by CPU on interrupt, then by us):
#
#   [CPU pushed]        [we push]
#   +120 SS             -8   R15
#   +112 RSP            -16  R14
#   +104 RFLAGS         -24  R13
#   +96  CS             -32  R12
#   +88  RIP            -40  R11
#                       -48  R10
#                       -56  R9
#                       -64  R8
#                       -72  RBP
#                       -80  RDI
#                       -88  RSI
#                       -96  RDX
#                       -104 RCX
#                       -112 RBX
#                       -120 RAX
# ============================================================================

save_context:
    pushq %rax
    pushq %rbx
    pushq %rcx
    pushq %rdx
    pushq %rsi
    pushq %rdi
    pushq %rbp
    pushq %r8
    pushq %r9
    pushq %r10
    pushq %r11
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    retq          # return to caller (timer_isr_entry etc.)

restore_context:
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %r11
    popq %r10
    popq %r9
    popq %r8
    popq %rbp
    popq %rdi
    popq %rsi
    popq %rdx
    popq %rcx
    popq %rbx
    popq %rax
    iretq

do_schedule:
    # rsp = current thread's saved context
    # Switch to kernel stack for schedule() call
    movq %rsp, %rbx            # save current thread's RSP
    movabs $stack_top, %rsp    # use kernel stack

    movq %rbx, %rdi            # arg1 = current SSP
    call schedule              # returns next SSP in rax

    testq %rax, %rax
    jz .idle

    movq %rax, %rsp            # switch to next thread's stack
    jmp restore_context

.idle:
    movabs $stack_top, %rsp
    # Mark no current thread (current_thread = -1)
    movabs $current_thread, %rdi
    movq $-1, (%rdi)
    sti
.idle_spin:
    hlt
    jmp .idle_spin

# ============================================================================
# Timer ISR — PIT IRQ0 (vector 32)
# ============================================================================

.global timer_isr_entry
timer_isr_entry:
    cli
    # Save all registers
    pushq %rax
    pushq %rbx
    pushq %rcx
    pushq %rdx
    pushq %rsi
    pushq %rdi
    pushq %rbp
    pushq %r8
    pushq %r9
    pushq %r10
    pushq %r11
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15

    # Send EOI to PIC
    movb $0x20, %al
    outb %al, $PIC1_CMD

    jmp do_schedule

# ============================================================================
# Syscall entry — int 0x80
# ============================================================================
#
# Convention (matching TRISC):
#   rdi = syscall number
#   rsi = arg1
#   rdx = arg2
#
# For fast-path syscalls, we don't save full context.
# For slow-path (context-switching), we save and go through schedule.
# ============================================================================

.global syscall_entry
syscall_entry:
    cli

    # Save all registers (slow path needs it; fast path will just iretq)
    pushq %rax
    pushq %rbx
    pushq %rcx
    pushq %rdx
    pushq %rsi
    pushq %rdi
    pushq %rbp
    pushq %r8
    pushq %r9
    pushq %r10
    pushq %r11
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15

    # Dispatch to C handler
    # rdi = syscall number (already there from caller)
    # Load from saved context: rdi was pushed, reload it
    movq 8*8(%rsp), %rdi      # saved RDI (syscall number)
    movq 7*8(%rsp), %rsi      # saved RSI (arg1)
    movq 5*8(%rsp), %rdx      # saved RDX (arg2)
    movq %rsp, %rcx            # arg4 = saved context pointer

    call oskit_arch_x86_64__syscall_dispatch      # in runtime.c

    # rax: 0 = fast path (just return), 1 = needs reschedule
    testq %rax, %rax
    jnz do_schedule

    # Fast path: restore and return
    jmp restore_context

# ============================================================================
# Exception stubs
# ============================================================================

.global exc_divide_error
exc_divide_error:
    cli
    movq $0, %rdi          # exception number
    call oskit_arch_x86_64__exception_handler
    hlt

.global exc_gpf
exc_gpf:
    cli
    # Error code was pushed by CPU
    popq %rsi              # error code
    movq $13, %rdi         # GPF = exception 13
    call oskit_arch_x86_64__exception_handler
    hlt

.global exc_page_fault
exc_page_fault:
    cli
    popq %rsi              # error code
    movq %cr2, %rdx        # faulting address
    movq $14, %rdi         # PF = exception 14
    call oskit_arch_x86_64__exception_handler
    hlt

.global exc_double_fault
exc_double_fault:
    cli
    popq %rsi              # error code (always 0)
    movq $8, %rdi
    call oskit_arch_x86_64__exception_handler
    hlt

# Generic exception stub (no error code)
.global exc_generic
exc_generic:
    cli
    call oskit_arch_x86_64__exception_handler
    hlt

# ============================================================================
# syscall wrapper — called from user Sysl code
# ============================================================================
#
# syscall(number: int, arg: int) -> int
#
# Sysl r1-only ABI on LLVM becomes System V:
#   rdi = number, rsi = arg (from stack on TRISC, direct reg on x86)
#
# We use int 0x80 to enter the kernel.
# ============================================================================

.global syscall
syscall:
    int $0x80
    retq

# ============================================================================
# thread_exit trampoline
# ============================================================================

.global thread_exit
thread_exit:
    movq $3, %rdi          # SYS_EXIT
    movq $0, %rsi
    int $0x80
    hlt                    # should never reach here

# ============================================================================
# GDT
# ============================================================================

.section .rodata
.align 16
gdt64:
    .quad 0x0000000000000000   # null
    .quad 0x00AF9A000000FFFF   # 64-bit code: present, executable, readable
    .quad 0x00AF92000000FFFF   # 64-bit data: present, writable
gdt64_ptr:
    .word gdt64_ptr - gdt64 - 1
    .long gdt64
