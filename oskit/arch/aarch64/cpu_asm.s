// aarch64 CPU primitives (arch-level, board-independent).
//
// Exposes arch_cli/arch_sti used by the kernel to mask and unmask
// asynchronous exceptions (IRQ, FIQ, SError) when holding a lock or
// entering a critical section. Bit D (debug) is left alone — we
// only toggle the three async interrupts and watch for aborts.
//
// `msr daifset, #imm` ORs `imm` into the masked bits of PSTATE.DAIF
// and `msr daifclr, #imm` clears them. #0x7 covers A (SError), I,
// and F; debug traps are controlled separately via MDSCR_EL1.

.section .text

.global arch_cli
arch_cli:
    msr daifset, #0x7
    ret

.global arch_sti
arch_sti:
    msr daifclr, #0x7
    ret

// sync_icache_line(addr: i64)
//   Clean the D-cache line containing `addr` to Point of Unification,
//   invalidate the entire I-cache, then ISB. This is the short form
//   of the architecture's "data side wrote, instruction side needs to
//   see it" dance — good enough when the modified region fits in one
//   cache line (A72 = 64B). For larger writes, loop dc cvau / ic ivau
//   over each line before the final dsb/isb.
.global sync_icache_line
sync_icache_line:
    dc  cvau, x0
    dsb ish
    ic  iallu
    dsb ish
    isb
    ret

// drop_to_el0(entry: i64, sp: i64)
//   Transition from EL1 to EL0 and start executing at `entry` with
//   SP_EL0 = `sp`. Does not return. The caller is responsible for
//   having a page table active that maps the entry point and stack
//   EL0-accessible. SPSR_EL1 = 0 means target mode EL0t (stack is
//   SP_EL0), DAIF unmasked; an exception from EL0 back to EL1 is
//   the normal way out.
.global drop_to_el0
drop_to_el0:
    msr sp_el0, x1
    msr elr_el1, x0
    mov x2, #0
    msr spsr_el1, x2
    isb
    eret

// arch_wfi — single wfi instruction, used by sysl-level idle loops
// that can't inline wfi directly.
.global arch_wfi
arch_wfi:
    wfi
    ret

// thread_exit — entry label a kernel thread "returns" into when its
// entry function executes `ret`. `build_stack_frame` pre-loads x30
// with this address. On TRISC/x86 the kernel patches this to a real
// trampoline that issues SYS_EXIT; until the aarch64 port wires the
// same logic in kernel.lsysl, this is a plain wfi loop so an errant
// return is visible as "thread is alive but wedged" rather than a
// silent fault.
.global thread_exit
thread_exit:
1:  wfi
    b    1b

// syscall(number: int, arg: i64) -> i64
//   User-facing syscall wrapper matching x86_64/TRISC ABI.
//   AAPCS64 passes number in x0 and arg in x1; the aarch64 SLIX
//   syscall dispatcher reads x8 for the syscall number and x0 for
//   the single argument. Shuffle, SVC, return — x0 already carries
//   the kernel's return value on resume.
.global syscall
syscall:
    mov x8, x0
    mov x0, x1
    svc #0
    ret

// arch_syscall_call(arg: i64, handler: i64)
//   Tail-call a syscall handler whose address came from syscall_table.
//   register_syscall stores the address of a __wrap_ trampoline
//   that expects the closure ABI: x0 = env pointer, x1 = real arg.
//   AAPCS64 call-site gives us x0 = arg, x1 = handler, so we must
//   shuffle: move handler to x2, arg to x1, zero x0, then br.
.global arch_syscall_call
arch_syscall_call:
    mov x2, x1
    mov x1, x0
    mov x0, #0
    br  x2

// user_svc_test — tiny EL0 entry used to sanity-check the EL1->EL0
// transition. Issues `svc #0x42` so the exception reporter in
// vectors.s fires `low64_sync` (V=8) with ESR_EL1 carrying EC=0x15
// (SVC64) and the immediate 0x42 in ISS[15:0]. Spins if SVC ever
// returns, which it currently never will (exception_report halts).
.global user_svc_test
user_svc_test:
    svc #0x42
1:  b    1b

// arch_spinlock_acquire(lock: *i64)
//   Spin on the lock word using LDXR/STXR until we win the exclusive
//   access. On win, DAIF is masked (interrupts off) and we return.
//   x0 = lock address.  Clobbers x1, x2, x3.
//
//   Pattern: ldxr x1, [x0]; cbnz x1, retry; mov x2, #1; stxr w3, x2, [x0]; cbnz w3, retry.
//   Then msr daifset, #0x7 to mask IRQ/FIQ/SError.
.global arch_spinlock_acquire
arch_spinlock_acquire:
1:  ldxr x1, [x0]
    cbnz x1, 1b
    mov  x2, #1
    stxr w3, x2, [x0]
    cbnz w3, 1b
    dmb  ish
    msr  daifset, #0x7
    ret

// arch_spinlock_release(lock: *i64)
//   Release the lock: dmb ish barrier, store 0, then unmask IRQs.
//   x0 = lock address.  Clobbers x1.
.global arch_spinlock_release
arch_spinlock_release:
    dmb  ish
    mov  x1, #0
    str  x1, [x0]
    msr  daifclr, #0x7
    ret

// arch_resume_process(frame_sp: i64)
//   Restore a 272-byte exception frame at SP_EL1 = `frame_sp`, then
//   ERET. Used by the scheduler to switch to a new thread after its
//   frame has been picked. Layout must exactly match the save path
//   in vectors.s (low64_sync / low64_irq). Does not return.
//
//   x0 = frame_sp
.global arch_resume_process
arch_resume_process:
    mov  sp, x0
    ldp  x0,  x1,  [sp, #0x100]
    msr  spsr_el1, x0
    msr  elr_el1,  x1
    ldp  x30, x0,  [sp, #0x0F0]
    msr  sp_el0,   x0
    ldp  x28, x29, [sp, #0x0E0]
    ldp  x26, x27, [sp, #0x0D0]
    ldp  x24, x25, [sp, #0x0C0]
    ldp  x22, x23, [sp, #0x0B0]
    ldp  x20, x21, [sp, #0x0A0]
    ldp  x18, x19, [sp, #0x090]
    ldp  x16, x17, [sp, #0x080]
    ldp  x14, x15, [sp, #0x070]
    ldp  x12, x13, [sp, #0x060]
    ldp  x10, x11, [sp, #0x050]
    ldp  x8,  x9,  [sp, #0x040]
    ldp  x6,  x7,  [sp, #0x030]
    ldp  x4,  x5,  [sp, #0x020]
    ldp  x2,  x3,  [sp, #0x010]
    ldp  x0,  x1,  [sp, #0x000]
    add  sp, sp, #0x110
    eret
