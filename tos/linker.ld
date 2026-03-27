# ============================================================================
# TOS — Linker script
# ============================================================================
#
# Places vector table at 0x0000 and code/data starting at 0x0100.
#
# ============================================================================

MEMORY
    vectors : ORIGIN = 0x0000, LENGTH = 0x0020
    code    : ORIGIN = 0x0100, LENGTH = 0x3F00

SECTIONS
    .vectors : { *(vectors) } > vectors
    .code    : { *(code) }    > code

ENTRY boot
