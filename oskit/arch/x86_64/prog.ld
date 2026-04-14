/* Linker script for standalone x86_64 SLIX programs.
 * Load address 0xD0000 matches TRISC binary layout.
 * Heap from end of BSS to 0x100000. */

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
