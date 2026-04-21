ENTRY(_start)

SECTIONS {
    . = 0x100000;            /* 1 MB — standard multiboot load address */

    .text : {
        *(.multiboot)        /* Multiboot header must be in first 8KB */
        *(.text)
    }
    .rodata : { *(.rodata*) }
    .data   : { *(.data) }

    . = ALIGN(4096);
    .bss : {
        *(.bss)
        *(COMMON)
    }

    _heap_start = .;         /* Sysl kernel references this for page pool */
}
