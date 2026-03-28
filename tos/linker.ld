# ============================================================================
# TOS (Tiny OS) — Linker Script
# ============================================================================
#
# Tells the linker where to place each section in memory.
#
# The vector table must be at address 0x0000 because the CPU reads
# exception vectors from fixed addresses starting there.
#
# 20 vectors x 8 bytes = 160 bytes (0xA0).
# Code starts at 0x00A0 immediately after the vector table.
#
# ============================================================================

SECTIONS
    vectors: 0x0000
    code: AFTER vectors

ENTRY boot
