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

# TSS: 64-bit Task State Segment (104 bytes)
# RSP0 at offset 4 is loaded by CPU on ring 3 → ring 0 transitions.
.align 16
.global tss
tss:    .skip 104

# Multiboot module info (filled by boot code)
.global mboot_mod_start
.global mboot_mod_end
mboot_mod_start: .skip 8    # physical address of first module (ramdisk)
mboot_mod_end:   .skip 8    # end address of first module
.global mboot_mod1_start
.global mboot_mod1_end
mboot_mod1_start: .skip 8   # physical address of second module (boot info)
mboot_mod1_end:   .skip 8   # end address of second module

# ============================================================================
# 32-bit entry point
# ============================================================================

.section .text
.code32
.global _start

_start:
    cli
    movl $stack_top, %esp

    # Save multiboot info pointer (EBX) before it gets clobbered
    movl %ebx, %esi            # ESI = multiboot info pointer (preserved)

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

    # --- TSS setup ---
    # Set RSP0 = stack_top (default kernel stack for ring 3 → ring 0 transitions)
    movabs $tss, %rdi
    movabs $stack_top, %rax
    movq %rax, 4(%rdi)         # TSS.RSP0 at offset 4
    # Set I/O Map Base = 104 (>= TSS limit → no IOPB → all ring 3 port I/O denied)
    movw $104, 102(%rdi)

    # Write TSS descriptor into GDT at offset 0x28
    # TSS base address is in BSS (identity-mapped, < 4GB)
    movabs $gdt64 + 0x28, %rdi
    movabs $tss, %rbx
    movw $0x67, (%rdi)         # limit[15:0] = 103
    movw %bx, 2(%rdi)          # base[15:0]
    movq %rbx, %rax
    shrq $16, %rax
    movb %al, 4(%rdi)          # base[23:16]
    movb $0x89, 5(%rdi)        # access: present, DPL=0, 64-bit TSS available
    movb $0x00, 6(%rdi)        # limit[19:16]=0, flags=0
    shrq $8, %rax              # rax was (base>>16), now (base>>24)
    movb %al, 7(%rdi)          # base[31:24]
    movl $0, 8(%rdi)           # base[63:32] = 0
    movl $0, 12(%rdi)          # reserved = 0

    # Load task register
    movw $0x28, %ax
    ltr %ax

    # --- Parse multiboot module info ---
    # ESI (preserved from 32-bit) = multiboot info pointer
    # Multiboot info flags at offset 0: bit 3 = modules present
    # Offset 20 = mods_count (u32), Offset 24 = mods_addr (u32)
    # Each module entry: mod_start(u32), mod_end(u32), string(u32), reserved(u32)
    movl %esi, %eax            # zero-extend ESI to RAX
    testl %eax, %eax
    jz .no_modules
    movl (%rax), %ecx          # flags
    testl $8, %ecx             # bit 3 = modules present?
    jz .no_modules
    movl 20(%rax), %ecx        # mods_count
    testl %ecx, %ecx
    jz .no_modules
    movl 24(%rax), %edx        # mods_addr (pointer to module array)
    # Module 0 (ramdisk)
    movl (%rdx), %ecx          # mod_start (first module)
    movl 4(%rdx), %ebx         # mod_end
    movq %rcx, mboot_mod_start(%rip)
    movq %rbx, mboot_mod_end(%rip)
    # Module 1 (boot info) — each entry is 16 bytes
    movl 20(%rax), %ecx        # reload mods_count
    cmpl $2, %ecx
    jl .no_mod1
    movl 16(%rdx), %ecx        # mod1_start (second entry at offset 16)
    movl 20(%rdx), %ebx        # mod1_end
    movq %rcx, mboot_mod1_start(%rip)
    movq %rbx, mboot_mod1_end(%rip)
.no_mod1:
    # Set page pool minimum above highest module end so page_alloc
    # doesn't hand out pages containing multiboot module data.
    # Uses the last stored module end (mod1_end if 2 modules, mod_end if 1).
    movq mboot_mod1_end(%rip), %rax
    testq %rax, %rax
    jz .use_mod0_end
    # Page-align mod1_end upward: (addr + 0xFFF) & ~0xFFF
    addq $0xFFF, %rax
    andq $-0x1000, %rax
    movq %rax, oskit_arch__page_pool_min_addr(%rip)
    jmp .no_modules
.use_mod0_end:
    movq mboot_mod_end(%rip), %rax
    testq %rax, %rax
    jz .no_modules
    addq $0xFFF, %rax
    andq $-0x1000, %rax
    movq %rax, oskit_arch__page_pool_min_addr(%rip)
.no_modules:

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
# tss_set_rsp0 — Update TSS.RSP0 (kernel stack for ring 3 → ring 0)
# ============================================================================
# rdi = new RSP0 value (top of per-process kernel stack)

.global tss_set_rsp0
tss_set_rsp0:
    movabs $tss, %rax
    movq %rdi, 4(%rax)
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
# set_hw_watchpoint — Set DR0 hardware write watchpoint (4 bytes)
# ============================================================================
# rdi = address to watch
# Sets DR0 to the address, DR7 to enable 4-byte write-only breakpoint on DR0.
# The CPU will raise #DB (vector 1) on any write to the watched address.

.global set_hw_watchpoint
set_hw_watchpoint:
    movq %rdi, %dr0
    # DR7: L0=1, G0=1, LE=1, R/W0=01 (write), LEN0=11 (4 bytes)
    # = (3 << 18) | (1 << 16) | (1 << 8) | 3 = 0xD0103
    movq $0xD0103, %rax
    movq %rax, %dr7
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
    movl $-1, (%rdi)
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
# Convention:
#   rdi = syscall number
#   rsi = arg1
#   rdx = arg2
#
# Fast-path syscalls (putc, thread_id, uptime) are handled inline
# and return via iretq without rescheduling.
#
# Slow-path syscalls are dispatched through the kernel's syscall_table
# and always go through do_schedule afterward.
#
# Saved register layout (after 15 pushes, RSP-relative):
#   +0:R15 +8:R14 +16:R13 +24:R12 +32:R11 +40:R10 +48:R9 +56:R8
#   +64:RBP +72:RDI +80:RSI +88:RDX +96:RCX +104:RBX +112:RAX
#   +120:RIP +128:CS +136:RFLAGS +144:RSP +152:SS
# ============================================================================

.global syscall_entry
syscall_entry:
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

    # Reload syscall number and args from saved context
    movq 9*8(%rsp), %rbx      # saved RDI = syscall number
    movq 10*8(%rsp), %r12     # saved RSI = arg1
    movq 11*8(%rsp), %r13     # saved RDX = arg2

    # --- Fast path: putc (syscall 1) ---
    cmpq $1, %rbx
    jne .not_putc
    movq %r12, %rdi            # arg = char
    call oskit_arch_x86_64__uart_putc
    jmp restore_context
.not_putc:

    # --- Fast path: thread_id (syscall 5) ---
    cmpq $5, %rbx
    jne .not_thread_id
    movq current_thread(%rip), %rax
    movq %rax, 14*8(%rsp)     # write to saved RAX
    jmp restore_context
.not_thread_id:

    # --- Fast path: uptime (syscall 6) ---
    cmpq $6, %rbx
    jne .not_uptime
    movq ticks(%rip), %rax
    movq %rax, 14*8(%rsp)     # write to saved RAX
    jmp restore_context
.not_uptime:

    # --- Slow path: table dispatch ---
    # Bounds check
    cmpq $96, %rbx             # MAX_SYSCALLS
    jge .bad_syscall
    cmpq $0, %rbx
    jl .bad_syscall

    # Look up handler: syscall_table[num] (array of i64)
    leaq syscall_table(%rip), %rax
    movq (%rax,%rbx,8), %rax  # rax = handler (function pointer)
    testq %rax, %rax
    jz .bad_syscall

    # Save SSP so kernel handlers can write return values
    leaq syscall_ssp(%rip), %rcx
    movq %rsp, (%rcx)

    # Privilege check: is this syscall allowed for the current process?
    pushq %rax                 # save handler pointer
    movl %ebx, %edi            # arg = syscall number
    call oskit_kernel__syscall_check_allowed
    testl %eax, %eax
    popq %rax                  # restore handler pointer
    jz .denied_syscall

    # Call handler: rdi = arg1 (env=null for non-capturing wrappers)
    # The handlers are __wrap_* functions: (i8* env, i32 arg)
    xorq %rdi, %rdi            # env = null
    movl %r12d, %esi           # arg1 (i32)
    call *%rax

    jmp do_schedule

.denied_syscall:
    # Permission denied — return -1 to caller
    movq $-1, 14*8(%rsp)      # write -1 to saved RAX
    jmp restore_context

.bad_syscall:
    # Unknown syscall — just return
    jmp restore_context

# ============================================================================
# Serial ISR — COM1 IRQ4 (vector 36)
# ============================================================================
# Lightweight ISR: reads serial data, does NOT reschedule.
# Returns to interrupted context via iretq.

.global serial_isr_entry
serial_isr_entry:
    cli
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

    call oskit_arch_x86_64__serial_handler

    # EOI to master PIC
    movb $0x20, %al
    outb %al, $PIC1_CMD

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

# ============================================================================
# Exception stubs
# ============================================================================

# After printing the exception, switch to kernel stack and idle
# with interrupts enabled so the timer can still schedule other threads.
.global exc_divide_error
exc_divide_error:
    cli
    movq $0, %rdi
    xorq %rsi, %rsi
    movq (%rsp), %rdx      # faulting RIP (no error code for #DE)
    movq %cr3, %rcx
    call oskit_arch_x86_64__exception_handler
    jmp exc_idle

.global exc_gpf
exc_gpf:
    cli
    popq %rsi              # error code
    movq (%rsp), %rdx      # faulting RIP (from iretq frame on stack)
    movq $13, %rdi
    movq %cr3, %rcx
    call oskit_arch_x86_64__exception_handler
    jmp exc_idle

.global exc_page_fault
exc_page_fault:
    cli
    popq %rsi
    movq %cr2, %rdx
    movq $14, %rdi
    movq %cr3, %rcx
    call oskit_arch_x86_64__exception_handler
    jmp exc_idle

.global exc_double_fault
exc_double_fault:
    cli
    popq %rsi
    movq $8, %rdi
    xorq %rdx, %rdx
    movq %cr3, %rcx
    call oskit_arch_x86_64__exception_handler
    hlt

# Per-vector exception stubs (no error code)
.macro exc_no_errcode vec
.global exc_stub_\vec
exc_stub_\vec:
    cli
    movq $\vec, %rdi
    xorq %rsi, %rsi
    movq (%rsp), %rdx
    movq %cr3, %rcx
    call oskit_arch_x86_64__exception_handler
    jmp exc_idle
.endm

# Per-vector exception stubs (with error code)
.macro exc_with_errcode vec
.global exc_stub_\vec
exc_stub_\vec:
    cli
    popq %rsi
    movq $\vec, %rdi
    movq (%rsp), %rdx
    movq %cr3, %rcx
    call oskit_arch_x86_64__exception_handler
    jmp exc_idle
.endm

# Generate stubs for all exception vectors not already handled
# Vector 1 (#DB) — Debug exception (hardware watchpoint)
# Print faulting RIP and DR6 (debug status), then clear DR7 and continue
.global exc_stub_1
exc_stub_1:
    cli
    # Save registers we'll use
    pushq %rax
    pushq %rdi
    # Print marker
    movq $0x57, %rdi          # 'W' for watchpoint
    call debug_char
    movq $0x3A, %rdi          # ':'
    call debug_char
    # Print faulting RIP (at offset +16 on stack: rdi, rax, RIP)
    movq 16(%rsp), %rdi       # faulting RIP
    shrq $16, %rdi
    call debug_hex4
    movq 16(%rsp), %rdi
    call debug_hex4
    movq $0x0A, %rdi          # newline
    call debug_char
    # Clear DR6 (debug status) so the exception doesn't re-fire
    xorq %rax, %rax
    movq %rax, %dr6
    # Restore and return to faulting instruction (it already wrote)
    popq %rdi
    popq %rax
    iretq
exc_no_errcode 2
exc_no_errcode 3
exc_no_errcode 4
exc_no_errcode 5
exc_no_errcode 6
exc_no_errcode 7
exc_no_errcode 9
exc_no_errcode 15
exc_no_errcode 16
exc_no_errcode 18
exc_no_errcode 19
exc_no_errcode 20
exc_no_errcode 22
exc_no_errcode 23
exc_no_errcode 24
exc_no_errcode 25
exc_no_errcode 26
exc_no_errcode 27
exc_no_errcode 28
exc_no_errcode 29
exc_no_errcode 30
exc_no_errcode 31
# Vectors WITH error codes: 10, 11, 12, 17, 21
exc_with_errcode 10
exc_with_errcode 11
exc_with_errcode 12
exc_with_errcode 17
exc_with_errcode 21

# Switch to kernel stack and idle with interrupts on.
# The timer ISR will fire and schedule other threads.
exc_idle:
    movabs $stack_top, %rsp
    movabs $current_thread, %rdi
    movl $-1, (%rdi)
    sti
.exc_idle_spin:
    hlt
    jmp .exc_idle_spin

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
# Multiboot module info accessors
# ============================================================================

.global get_mboot_mod_start
get_mboot_mod_start:
    movq mboot_mod_start(%rip), %rax
    retq

.global get_mboot_mod_end
get_mboot_mod_end:
    movq mboot_mod_end(%rip), %rax
    retq

.global get_mboot_mod1_start
get_mboot_mod1_start:
    movq mboot_mod1_start(%rip), %rax
    retq

.global get_mboot_mod1_end
get_mboot_mod1_end:
    movq mboot_mod1_end(%rip), %rax
    retq

# ============================================================================
# GDT
# ============================================================================
# Must be in .data (not .rodata) — TSS descriptor is filled at runtime.

.section .data
.align 16
gdt64:
    .quad 0x0000000000000000   # 0x00: null
    .quad 0x00AF9A000000FFFF   # 0x08: kernel code (DPL=0)
    .quad 0x00AF92000000FFFF   # 0x10: kernel data (DPL=0)
    .quad 0x00AFFA000000FFFF   # 0x18: user code   (DPL=3)
    .quad 0x00AFF2000000FFFF   # 0x20: user data   (DPL=3)
    .quad 0                    # 0x28: TSS descriptor lo (filled at runtime)
    .quad 0                    # 0x30: TSS descriptor hi (filled at runtime)
gdt64_ptr:
    .word gdt64_ptr - gdt64 - 1
    .long gdt64
