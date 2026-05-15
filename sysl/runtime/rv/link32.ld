/* Linker script for rv32-elf sysl programs running under qemu-system-riscv32
 * with OpenSBI. The virt machine's load address for rv32 kernels is also
 * 0x80400000 (OpenSBI lives below it). qemu-system-riscv32 -machine virt's
 * RAM starts at 0x80000000 — we put the kernel above the firmware.
 */

OUTPUT_FORMAT("elf32-littleriscv")
OUTPUT_ARCH(riscv)
ENTRY(_start)

MEMORY {
    RAM (rwxa) : ORIGIN = 0x80400000, LENGTH = 64M
}

SECTIONS {
    . = 0x80400000;

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
