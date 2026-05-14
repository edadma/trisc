/* Linker script for standalone x86_64 SLIX servers.
 *
 * Phase D.2: servers link at 0x60000000, matching user programs and
 * aarch64's `oskit/arch/aarch64/prog.ld`. The cross-arch user
 * carve-out `[VMA_DEFAULT_CODE_BOT, VMA_DEFAULT_CODE_TOP)` =
 * `[0x60000000, 0x60080000)` covers code+data+heap; the stack VMA
 * `[0x60080000, 0x60090000)` adjacent is seeded by
 * `vma_seed_default_layout`. The legacy 0x40000-load layout
 * (servers at 0x40000..0xCB000, stack at 0xCC000..0xD3FFF) is
 * gone — `vm_create_server_pt` no longer eager-maps the
 * 0x40..0xCA region.
 *
 * 512 KiB of code+BSS at 0x60000000..0x60080000 is the same
 * budget aarch64 has lived with; inet (~253 KiB BSS) and RS
 * (~128 KiB) both fit comfortably.
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
