ENTRY(_start)

SECTIONS {
    . = 0x40000000;

    .text : { *(.text) }
    .rodata : { *(.rodata) }
    .data : { *(.data) }
    .bss : { *(.bss) }

    . = ALIGN(16);
    . = . + 4096;
    _stack_top = .;
}
