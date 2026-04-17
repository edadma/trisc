/* Linker script for standalone x86_64 SLIX servers.
 * Load address 0xD0000 — servers have large BSS (RS ~128KB)
 * and need the full 192KB region to 0x100000.
 * Stack: 16KB (0xCC000..0xCFFFF). */

ENTRY(_start)

SECTIONS
{
    . = 0xD0000;

    .text : { *(.text) }
    .rodata : { *(.rodata*) }
    .data : { *(.data) }
    .bss : { *(.bss) *(COMMON) }

    _heap_start = .;
    _heap_end = 0x100000;

    /DISCARD/ : { *(.comment) *(.note*) *(.eh_frame*) }
}
