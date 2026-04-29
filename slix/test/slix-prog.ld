/* Linker script for SLIX aarch64 musl-linked user programs.
 * Matches the user carve-out laid down by vm_create_process_pt:
 *   0x60000000..0x6003FFFF  code + data + bss (256 pages)
 *   0x60040000..0x6007FFFF  heap (256 pages, brk()-managed)
 *   0x60080000..0x6008FFFF  user stack (16 pages, USP = 0x60090000)
 * Forcing `.` to 0x60040000 before defining _heap_start gives the
 * brk()/sbrk() syscall a fixed initial break independent of the
 * binary's BSS size — see oskit/posix/shim.lsysl sys_brk.
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

    /* Heap base is fixed at 0x60040000 so the kernel's brk syscall
     * can return a deterministic initial break. Programs are limited
     * to 256KB of text+data+BSS, which fits all current slix-musl
     * binaries (largest is ~80KB). */
    . = 0x60040000;
    _heap_start = .;
    _heap_end = 0x60080000;

    /DISCARD/ : { *(.comment) *(.note*) *(.eh_frame*) }
}
