# ============================================================================
# TOS (Tiny OS) — Linker Script
# ============================================================================
#
# Sections are placed in the order listed. Sections with an explicit
# address start there; others follow sequentially after the previous.
# All named sections exist even if empty.
#
# Memory layout:
#   0x00000 — 0x0009F  Vector table (20 x 8 bytes)
#   0x000A0 — ...      Code (functions)
#   ...                Read-only data (string literals)
#   ...                Initialized data (globals with values)
#   ...                BSS (zero-initialized globals, arrays)
#   ...     — 0xFFFFF  Heap / stack space
#   0x100000+          Memory-mapped devices
#
# ============================================================================

SECTIONS
    vectors: 0x0000
    code
    rodata
    data
    bss

SYMBOL _heap_start = AFTER bss
SYMBOL _heap_end = 0x100000

ENTRY boot
