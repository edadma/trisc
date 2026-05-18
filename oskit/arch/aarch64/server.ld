/* Linker script for standalone aarch64 SLIX servers.
 *
 * Servers link at 0x60000000 — same code carve-out as user programs.
 * `vma_seed_default_layout` (oskit/kernel/kernel.lsysl) installs one
 * RWX anon VMA covering [VMA_DEFAULT_CODE_BOT=0x60000000,
 * VMA_DEFAULT_CODE_TOP=0x60080000) so the entire 512 KiB region is
 * mappable; only stack/code pages are eager-installed, the rest
 * fault in lazily.
 *
 * Unlike `prog.ld` (which hard-codes `_heap_start = 0x60040000` so
 * the PIE loader's heap VMA at `[VMA_DEFAULT_HEAP_BOT, VMA_DEFAULT_HEAP_TOP)`
 * matches), servers don't go through `pm_load_image_pie` — they get
 * the full RWX VMA — so we can put `_heap_start = .` right after BSS
 * and reclaim the wasted gap. This mirrors x86's `server.ld`.
 *
 * Sizing for the largest server matters: PM does a `malloc(size)`
 * per `exec()` to buffer the new image, and the previous 256 KiB
 * fixed heap couldn't fit dhclient (132 KiB) followed by login
 * (152 KiB) because the std.alloc free list can't satisfy a 152 KiB
 * request out of a single freed 132 KiB block. With `_heap_start = .`,
 * PM's heap is ~430 KiB (PM's code+data+BSS is small) — same budget
 * x86 servers have always had.
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
