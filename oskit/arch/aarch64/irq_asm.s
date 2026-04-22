// aarch64 IRQ / generic-timer system register helpers.
//
// Sysl can't emit msr/mrs directly, so these are plain-asm thunks
// that the sysl gic/timer layers call as `extern`.

.section .text

// cntfrq_el0() -> u64
//   Read the generic timer's frequency in Hz.
.global cntfrq_el0
cntfrq_el0:
    mrs x0, cntfrq_el0
    ret

// cntpct_el0() -> u64
//   Read the current physical counter value.
.global cntpct_el0
cntpct_el0:
    mrs x0, cntpct_el0
    ret

// cntp_tval_set(tval: u64)
//   Set the EL1 physical timer compare-down count. When CVAL reaches
//   zero the timer raises its IRQ line (PPI 14, IRQ 30 on GIC).
.global cntp_tval_set
cntp_tval_set:
    msr cntp_tval_el0, x0
    isb
    ret

// cntp_ctl_set(v: u64)
//   CNTP_CTL_EL0 — bit 0 ENABLE, bit 1 IMASK, bit 2 ISTATUS (RO).
.global cntp_ctl_set
cntp_ctl_set:
    msr cntp_ctl_el0, x0
    isb
    ret
