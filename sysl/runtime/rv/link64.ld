/* Linker script for rv64-elf sysl programs running under qemu-system-riscv64
 * with OpenSBI. OpenSBI relocates itself out of 0x80000000, then jumps to
 * the kernel at 0x80200000 (the default "fw_jump_addr" for the virt machine).
 * RAM size in the qemu virt default is 128 MiB but we only declare what we
 * actually need symbolic names for.
 */

OUTPUT_FORMAT("elf64-littleriscv")
OUTPUT_ARCH(riscv)
ENTRY(_start)

MEMORY {
    RAM (rwxa) : ORIGIN = 0x80200000, LENGTH = 64M
}

SECTIONS {
    . = 0x80200000;

    .text : ALIGN(4K) {
        *(.text._start)
        *(.text .text.*)
    } > RAM

    .rodata : ALIGN(4K) {
        *(.rodata .rodata.*)
        *(.srodata .srodata.*)
    } > RAM

    .data : ALIGN(4K) {
        *(.data .data.*)
        *(.sdata .sdata.*)
    } > RAM

    .bss : ALIGN(4K) {
        __bss_start = .;
        *(.bss .bss.*)
        *(.sbss .sbss.*)
        *(COMMON)
        . = ALIGN(16);
        __bss_end = .;
    } > RAM

    /* Heap follows BSS, grows up. Stack at the very top, grows down. */
    . = ALIGN(16);
    __heap_start = .;
    . = ORIGIN(RAM) + LENGTH(RAM) - 64K;
    __heap_end = .;
    __stack_top = ORIGIN(RAM) + LENGTH(RAM);

    /DISCARD/ : {
        *(.eh_frame .eh_frame.*)
        *(.note .note.*)
        *(.comment)
        *(.riscv.attributes)
    }
}
