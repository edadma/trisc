/* Linker script for standalone aarch64 SLIX programs.
 * Load address 0x60000000 — matches the user carve-out in
 * vm_create_process_pt (128 code pages at 0x60000000..0x6007FFFF,
 * 16 stack pages at 0x60080000..0x6008FFFF,
 * info page at  0x60090000..0x60090FFF).
 *
 * `_heap_start` is a fixed absolute symbol at 0x60040000 — must
 * land inside the heap VMA that `pm_load_image_pie` installs at
 * `[VMA_DEFAULT_HEAP_BOT=0x60040000, VMA_DEFAULT_HEAP_TOP=0x60080000)`.
 * The PIE loader sizes the per-PT_LOAD VMA exactly to the binary's
 * MemSize, so `_heap_start = .` (right after BSS) puts the std.alloc
 * bump allocator's first allocation at whatever offset BSS happens
 * to end at — and when BSS ends near a page boundary the first
 * malloc's footer write crosses into the unmapped page just past
 * the per-PT_LOAD VMA's vend and the process dies. Matches the
 * pattern slix-prog.ld already uses for musl-static programs. */

ENTRY(_start)

SECTIONS
{
    . = 0x60000000;

    .text : { *(.text .text.*) }
    .rodata : { *(.rodata .rodata.*) }
    .data : { *(.data .data.*) }
    .bss : { *(.bss .bss.*) *(COMMON) }

    _heap_start = 0x60040000;
    _heap_end = 0x60080000;

    /DISCARD/ : { *(.comment) *(.note*) *(.eh_frame*) }
}
