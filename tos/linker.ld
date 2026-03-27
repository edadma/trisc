# ============================================================================
# TOS (Tiny OS) — Linker Script
# ============================================================================
#
# Tells the linker where to place each section in memory.
#
# The vector table must be at address 0x0000 because the CPU reads
# exception vectors from fixed addresses starting there.
#
# Code and data start at 0x0100, leaving room for the 64-byte
# vector table (16 vectors × 4 bytes = 64 bytes).
#
# ============================================================================

MEMORY
    vectors : ORIGIN = 0x0000, LENGTH = 0x0040
    code    : ORIGIN = 0x0100, LENGTH = 0xDEFF

SECTIONS
    .vectors : { *(vectors) } > vectors
    .code    : { *(code) }    > code

ENTRY boot
