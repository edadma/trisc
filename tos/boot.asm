# ============================================================================
# TOS (Tiny OS) — Boot and interrupt glue (assembly)
# ============================================================================
#
# Only the parts that MUST be assembly:
#   - Vector table
#   - Initial stack setup
#   - ISR entry/exit (register save/restore, RTE)
#
# Everything else is in Sysl.
#
# Memory map:
#   0x0000 - 0x001F   Vector table
#   0x0100 - 0x3FFF   Kernel + user code
#   0x4000 - 0x4FFF   Thread 0 supervisor stack (grows down from 0x5000)
#   0x5000 - 0x5FFF   Thread 0 user stack (grows down from 0x6000)
#   0x6000 - 0x6FFF   Thread 1 supervisor stack (grows down from 0x7000)
#   0x7000 - 0x7FFF   Thread 1 user stack (grows down from 0x8000)
#   0x8000 - 0x8FFF   Thread 2 supervisor stack (grows down from 0x9000)
#   0x9000 - 0x9FFF   Thread 2 user stack (grows down from 0xA000)
#   0xE000 - 0xEFFF   Kernel supervisor stack (grows down from 0xF000)
#   0xFFE0 - 0xFFFF   Memory-mapped I/O
# ============================================================================

segment vectors

  dw boot                  # Vector 0: Reset
  dw timer_isr             # Vector 1: Timer interrupt
  dw default_isr           # Vector 2: unused
  dw default_isr           # Vector 3: unused
  dw default_isr           # Vector 4: unused
  dw default_isr           # Vector 5: unused
  dw default_isr           # Vector 6: unused
  dw default_isr           # Vector 7: unused

segment code

# ============================================================================
# Reset vector — boot entry point
# ============================================================================

global boot, func
entry boot
boot
  # Set up kernel supervisor stack and jump to Sysl kernel_main()
  movi r7, 0xF000
  movi r4, kernel_main
  jalr r6, r4
  # kernel_main doesn't return — it starts the first thread


# ============================================================================
# Timer ISR — saves context, calls Sysl schedule(), restores context
# ============================================================================
#
# On entry: supervisor mode, hardware pushed PC and PSR onto SSP.
# We save all registers, call the Sysl scheduler, then restore.
#
# ============================================================================

global timer_isr, func
timer_isr
  # ---- Save current thread's registers ----
  pshd r1
  pshd r2
  pshd r3
  pshd r4
  pshd r5
  pshd r6
  gusp r1
  pshd r1                 # save user stack pointer

  # ---- Call Sysl scheduler ----
  # Pass current SSP in r1 so scheduler can save it
  mov  r1, r7
  movi r4, schedule
  jalr r6, r4

  # schedule() returns the new thread's SSP in r1
  mov  r7, r1

  # ---- Restore new thread's registers ----
  popd r1
  susp r1                 # restore user stack pointer
  popd r6
  popd r5
  popd r4
  popd r3
  popd r2
  popd r1

  rte


# ============================================================================
# Default ISR — unhandled interrupts
# ============================================================================

global default_isr, func
default_isr
  rte


# ============================================================================
# start_first_thread — loads a thread context and RTEs into it
# ============================================================================
#
# Called from Sysl kernel_main() with r1 = SSP of first thread.
# This never returns — it transitions to user mode.
#
# ============================================================================

global start_first_thread, func
start_first_thread
  mov  r7, r1              # set SSP to thread's saved stack

  popd r1
  susp r1                  # restore user stack pointer
  popd r6
  popd r5
  popd r4
  popd r3
  popd r2
  popd r1

  rte
