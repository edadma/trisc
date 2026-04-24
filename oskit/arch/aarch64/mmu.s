// aarch64 MMU bring-up — identity map + enable (arch-level).
//
// Sets up a minimal two-level page table that identity-maps:
//   [0x0000_0000..0x3FFF_FFFF]  Device-nGnRnE  (UART, GIC, virtio)
//   [0x4000_0000..0x7FFF_FFFF]  Normal WBWA    (RAM — kernel, stack)
// using 1GB block descriptors at translation level 1 (4KB granule,
// 48-bit VA, stage 1 at EL1). After `mmu_init` returns the CPU is
// running with paging on, data and instruction caches enabled.
//
// Contract: called once at boot before any code that depends on
// caching/ordering assumptions. Safe from EL1 with interrupts
// masked, which is the state boot.s establishes.

.section .text

.global mmu_init
mmu_init:
    // MAIR_EL1
    //   Attr0 = 0x00  Device-nGnRnE
    //   Attr1 = 0xFF  Normal Outer/Inner WB, WA, RA
    mov  x0, #0xFF00
    msr  mair_el1, x0

    // TCR_EL1
    //   T0SZ       = 16  (48-bit VA for TTBR0)
    //   EPD0       = 0   (TTBR0 walks enabled)
    //   IRGN0/ORGN0= 01  (Normal WBWA cacheable page-table walks)
    //   SH0        = 11  (inner shareable)
    //   TG0        = 00  (4KB granule)
    //   EPD1       = 1   (TTBR1 walks disabled — kernel uses TTBR0 only)
    //   IPS        = 000 (32-bit PA, QEMU virt RAM fits easily)
    ldr  x0, =0x0000000000803510
    msr  tcr_el1, x0

    // TTBR0_EL1 = physical address of l0_table (identity-mapped so
    // VA == PA for the tables themselves — fine before MMU is on).
    adrp x0, l0_table
    add  x0, x0, #:lo12:l0_table
    msr  ttbr0_el1, x0

    isb

    // Flush any stale TLB entries and wait for completion.
    tlbi vmalle1
    dsb  ish
    isb

    // Enable MMU + caches in SCTLR_EL1.
    mrs  x0, sctlr_el1
    orr  x0, x0, #(1 << 0)     // M  — MMU enable
    orr  x0, x0, #(1 << 2)     // C  — data cache enable
    orr  x0, x0, #(1 << 12)    // I  — instruction cache enable
    msr  sctlr_el1, x0
    isb

    // Enable FP/SIMD for both EL0 and EL1 via CPACR_EL1.FPEN = 0b11.
    // Reset value traps any FP/SIMD access from both ELs, which bites
    // as soon as clang emits NEON-ish lowerings for things like the
    // LLVM-generated memcpy inside server binaries. Enabling once at
    // boot keeps every process's FP state "just works" — we don't do
    // lazy FP context switching yet.
    mrs  x0, cpacr_el1
    orr  x0, x0, #(3 << 20)    // FPEN[21:20] = 0b11 — no trap
    msr  cpacr_el1, x0
    isb

    ret


// Static page tables live in .data so the CPU can set the Access
// Flag later if we ever flip AF=0 — we preset AF=1 here, but .data
// still avoids any risk of a write-permission fault on table pages.
// Both tables are 4KB-aligned via `.balign 4096`; link.ld places
// .data on a page boundary so the alignment holds.

.section .data

.balign 4096
.global l0_table
l0_table:
    // L0[0] -> L1 table (table descriptor: lower two bits 0b11 = valid+table)
    .quad l1_table + 0x3
    .skip 4088

.balign 4096
.global l1_table
l1_table:
    // L1[0]: 0x00000000-0x3FFFFFFF, Device-nGnRnE
    //   block|valid (0b01), AttrIndx=0, AP=0 (EL1 RW, EL0 none),
    //   SH=0 (non-shareable — Device is naturally ordered),
    //   AF=1, UXN=1, PXN=1.
    .quad 0x0060000000000401
    // L1[1]: 0x40000000-0x7FFFFFFF, Normal WBWA
    //   block|valid, AttrIndx=1, AP=0, SH=3 (inner shareable),
    //   AF=1, UXN=0, PXN=0 (kernel needs to execute from here).
    .quad 0x0000000040000705
    .skip 4080
