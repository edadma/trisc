/* Linker script for standalone aarch64 SLIX programs.
 * Load address 0x60000000 — matches the user carve-out in
 * vm_create_process_pt (16 code pages at 0x60000000..0x6000FFFF,
 * 16 stack pages at 0x60010000..0x6001FFFF). */

ENTRY(_start)

SECTIONS
{
    . = 0x60000000;

    .text : { *(.text .text.*) }
    .rodata : { *(.rodata .rodata.*) }
    .data : { *(.data .data.*) }
    .bss : { *(.bss .bss.*) *(COMMON) }

    _heap_start = .;
    _heap_end = 0x60010000;

    /DISCARD/ : { *(.comment) *(.note*) *(.eh_frame*) }
}
