# x86 boot trampoline — multiboot (32-bit) → long mode (64-bit)
#
# GRUB/QEMU loads us in 32-bit protected mode. We set up page
# tables, enable long mode, and jump to 64-bit code in startup64.

.set MULTIBOOT_MAGIC, 0x1BADB002
.set MULTIBOOT_FLAGS, 0x00000003
.set MULTIBOOT_CHECKSUM, -(MULTIBOOT_MAGIC + MULTIBOOT_FLAGS)

.section .multiboot
.align 4
    .long MULTIBOOT_MAGIC
    .long MULTIBOOT_FLAGS
    .long MULTIBOOT_CHECKSUM

.section .bss
.align 4096

# Identity-map page tables: PML4 → PDPT → PD (first 2MB)
pml4:   .skip 4096
pdpt:   .skip 4096
pd:     .skip 4096

.align 16
stack_bottom:
    .skip 16384
.global stack_top
stack_top:

.section .text
.code32
.global _start

_start:
    cli
    movl $stack_top, %esp

    # PML4[0] → PDPT
    movl $pdpt, %eax
    orl  $0x03, %eax
    movl %eax, pml4

    # PDPT[0] → PD
    movl $pd, %eax
    orl  $0x03, %eax
    movl %eax, pdpt

    # PD[0..3] → identity-map first 8MB via 2MB pages (PS bit = 0x80)
    movl $0x83, pd             # 0-2MB
    movl $0x200083, pd + 8     # 2-4MB
    movl $0x400083, pd + 16    # 4-6MB
    movl $0x600083, pd + 24    # 6-8MB

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

# --- 64-bit entry point ---
.code64
entry64:
    movw $0x10, %ax
    movw %ax, %ds
    movw %ax, %es
    movw %ax, %ss
    movw %ax, %fs
    movw %ax, %gs

    movabs $stack_top, %rsp

    # Enable SSE (required by LLVM-generated code for memset/memcpy)
    # Clear CR0.EM (bit 2), set CR0.MP (bit 1)
    movq %cr0, %rax
    andq $~0x4, %rax
    orq  $0x2, %rax
    movq %rax, %cr0
    # Set CR4.OSFXSR (bit 9) and CR4.OSXMMEXCPT (bit 10)
    movq %cr4, %rax
    orq  $0x600, %rax
    movq %rax, %cr4

    call boot_entry

    hlt
    jmp . - 1

.section .rodata
.align 16
gdt64:
    .quad 0x0000000000000000   # null
    .quad 0x00AF9A000000FFFF   # 64-bit code: present, executable, readable
    .quad 0x00AF92000000FFFF   # 64-bit data: present, writable
gdt64_ptr:
    .word gdt64_ptr - gdt64 - 1
    .long gdt64
