/* Linker script for SLIX aarch64 musl-linked user programs.
 * Matches the user carve-out laid down by vm_create_process_pt:
 *   0x60000000..0x6007FFFF  code + data + heap (128 pages)
 *   0x60080000..0x6008FFFF  user stack (16 pages, USP = 0x60090000)
 * Musl's crt1.o provides _start, which reads argc from [sp] — PM
 * already builds a System V ABI init frame there before resume. */

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
