/* Linker script for standalone x86_64 SLIX servers.
 * Load address 0x40000 — servers have large BSS (inet ~253KB,
 * RS ~128KB). The 0x40000..0xCB000 region is mapped fresh
 * user pages by vm_create_server_pt, giving ~555KB for
 * code+BSS below the guard page at 0xCB000.
 * Stack: 32KB (0xCC000..0xD3FFF). */

ENTRY(_start)

SECTIONS
{
    . = 0x40000;

    .text : { *(.text) }
    .rodata : { *(.rodata*) }
    .data : { *(.data) }
    .bss : { *(.bss) *(COMMON) }

    _heap_start = .;
    _heap_end = 0xCB000;

    /DISCARD/ : { *(.comment) *(.note*) *(.eh_frame*) }
}
