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
    .data   : { *(.data*) }

    . = ALIGN(8);
    __bss_start = .;
    .bss    : { *(.bss*) *(COMMON) }
    . = ALIGN(8);
    __bss_end = .;

    /DISCARD/ : { *(.comment) *(.note*) *(.eh_frame*) *(.ARM.*) }
}
