/* Linker script for SLIX aarch64 on QEMU virt.
 *
 * QEMU -kernel loads the image at 0x40080000 (64KB above RAM start,
 * following the Linux aarch64 boot protocol text_offset). _start is
 * the entry point and must sit first in .text.boot. */

ENTRY(_start)

SECTIONS
{
    . = 0x40080000;

    .text : {
        KEEP(*(.text.boot))
        *(.text*)
    }

    .rodata : { *(.rodata*) }

    /* Page tables (l0_table, l1_table) need 4KB alignment — emitted
     * in .data via `.balign 4096` in mmu.s. Force the section itself
     * onto a page boundary so the per-symbol alignment holds. */
    . = ALIGN(4096);
    .data   : { *(.data*) }

    . = ALIGN(8);
    __bss_start = .;
    .bss    : { *(.bss*) *(COMMON) }
    . = ALIGN(8);
    __bss_end = .;

    /DISCARD/ : { *(.comment) *(.note*) *(.eh_frame*) *(.ARM.*) }
}
