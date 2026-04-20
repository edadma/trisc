/* Linker script for standalone aarch64 SLIX programs.
 * Load address 0x60000000 — matches the user carve-out in
 * vm_create_process_pt (128 code pages at 0x60000000..0x6007FFFF,
 * 16 stack pages at 0x60080000..0x6008FFFF,
 * info page at  0x60090000..0x60090FFF). */

ENTRY(_start)

SECTIONS
{
    . = 0x60000000;

    .text : { *(.text .text.*) }
    .rodata : { *(.rodata .rodata.*) }
    .data : { *(.data .data.*) }
    .bss : { *(.bss .bss.*) *(COMMON) }

    _heap_start = .;
    _heap_end = 0x60080000;

    /DISCARD/ : { *(.comment) *(.note*) *(.eh_frame*) }
}
