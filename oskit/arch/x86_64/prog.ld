/* Linker script for standalone x86_64 SLIX programs.
 *
 * Phase D.2 of the per-process VMA-list invariant: programs link
 * at 0x60000000, matching aarch64 and the cross-arch
 * `VMA_DEFAULT_CODE_BOT` / `VMA_DEFAULT_CODE_TOP` constants in
 * `oskit/kernel/vma.lsysl`. Heap runs from end-of-BSS to
 * `VMA_DEFAULT_CODE_TOP = 0x60080000`; the stack lives in the
 * adjacent VMA `[0x60080000, 0x60090000)` seeded by
 * `vma_seed_default_layout`. The legacy 0xE0000-load layout
 * (program at 0xE0000, stack at 0xCC000..0xDFFFF, heap to
 * 0x100000) is gone — `vm_create_process_pt` no longer
 * eager-maps the 0xCC..0xFF region.
 */

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
