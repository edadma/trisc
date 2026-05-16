/* Linker script for SLIX aarch64 musl-linked user programs.
 * Matches the user carve-out laid down by vm_create_process_pt:
 *   0x60000000..0x6003FFFF  code + data + bss (256 pages)
 *   0x60040000..0x6007FFFF  heap (256 pages, brk()-managed)
 *   0x60080000..0x6008FFFF  user stack (16 pages, USP = 0x60090000)
 * `_heap_start` is a fixed absolute symbol so the brk()/sbrk()
 * syscall has a deterministic initial break independent of the
 * binary's BSS size — see oskit/posix/shim.lsysl sys_brk.
 * Musl's crt1.o provides _start, which reads argc from [sp] — PM
 * already builds a System V ABI init frame there before resume.
 *
 * The `text` PHDR carries FILEHDR + PHDRS, which lays out the ELF
 * header + program header table as part of the first PT_LOAD. Without
 * this, lld puts the PHT at file offset 64 but the first LOAD only
 * begins at file offset 0x1000 — and `pm_compute_at_phdr` in
 * oskit/servers/pm.lsysl walks PT_LOADs for the one covering phoff,
 * returning 0 when none does. Once Phase 1 chunk 9 started populating
 * AT_PHDR/AT_PHNUM in the auxv, musl's static_init_tls iterated from
 * NULL on every static binary, faulting at vaddr=0 in __init_libc.
 * Bringing the headers into a LOAD makes AT_PHDR resolve to a real
 * runtime VA inside the first text page (same convention Linux uses). */

ENTRY(_start)

PHDRS
{
    text   PT_LOAD FILEHDR PHDRS FLAGS(5);   /* R+X */
    data   PT_LOAD FLAGS(6);                 /* R+W */
}

_heap_start = 0x60040000;
_heap_end   = 0x60080000;

SECTIONS
{
    . = 0x60000000 + SIZEOF_HEADERS;

    .text : { *(.text .text.*) } : text
    .rodata : { *(.rodata .rodata.*) } : text

    . = ALIGN(0x1000);
    .data : { *(.data .data.*) } : data
    .bss : { *(.bss .bss.*) *(COMMON) } : data

    /DISCARD/ : { *(.comment) *(.note*) *(.eh_frame*) }
}
