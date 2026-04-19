// aarch64 VM asm helpers.
//
// Wraps the handful of system-register writes the sysl `vm.lsysl`
// layer needs — TTBR0 read/write and TLB invalidation. Each routine
// is a bare msr/mrs followed by the required barriers so sysl
// callers can treat them as ordinary function calls.

.section .text

// vm_get_ptbr() -> u64
//   Returns current TTBR0_EL1.
.global vm_get_ptbr
vm_get_ptbr:
    mrs x0, ttbr0_el1
    ret

// vm_set_ptbr(addr: u64)
//   Loads TTBR0_EL1 and flushes the TLB so the new translation is
//   visible. Follows the ARM recommended sequence:
//     msr ttbr0_el1, Xt; isb; tlbi vmalle1; dsb ish; isb
.global vm_set_ptbr
vm_set_ptbr:
    msr ttbr0_el1, x0
    isb
    tlbi vmalle1
    dsb  ish
    isb
    ret

// vm_flush_tlb()
//   Invalidate all TLB entries at EL1 (both stages). Useful after
//   modifying page-table entries without changing TTBR0.
.global vm_flush_tlb
vm_flush_tlb:
    dsb  ishst
    tlbi vmalle1
    dsb  ish
    isb
    ret
