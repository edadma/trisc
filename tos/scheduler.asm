# ============================================================================
# TOS — Scheduler and context switch
# ============================================================================
#
# The scheduler is driven by the timer interrupt. Every tick:
#   1. Save current thread's registers onto its supervisor stack
#   2. Save the supervisor stack pointer into the thread's TCB
#   3. Pick the next thread (simple round-robin)
#   4. Load the new thread's supervisor stack pointer from its TCB
#   5. Restore registers from the new thread's stack
#   6. RTE back to the new thread's user code
#
# Thread Control Block (TCB):
#   Each TCB is just one 8-byte value: the saved supervisor stack pointer.
#   All other thread state (registers, PC, PSR, USP) lives on the
#   supervisor stack itself.
#
#   tcb_table[i] = saved SSP for thread i
#
# Stack frame layout (top of stack = lowest address):
#   [USP]  ← pushed by ISR
#   [r6]
#   [r5]
#   [r4]
#   [r3]
#   [r2]
#   [r1]
#   [PC]   ← pushed by hardware on exception entry
#   [PSR]  ← pushed by hardware on exception entry
#
# ============================================================================

# ============================================================================
# Timer interrupt service routine
# ============================================================================

global timer_isr, func
timer_isr
  # ---- Save current thread's context ----
  # Hardware already pushed PC and PSR onto supervisor stack.
  # We're in supervisor mode, r7 = SSP.

  pshd r1
  pshd r2
  pshd r3
  pshd r4
  pshd r5
  pshd r6
  gusp r1
  pshd r1                 # save user stack pointer

  # ---- Save SSP to current thread's TCB ----
  movi r1, current_thread
  ldd  r1, r1, r0         # r1 = current thread index
  ldi  r2, 8
  mul  r1, r1, r2         # r1 = index * 8 (offset into tcb_table)
  movi r2, tcb_table
  add  r2, r2, r1         # r2 = &tcb_table[current]
  std  r7, r2, r0         # save SSP

  # ---- Pick next thread (round-robin) ----
  movi r1, current_thread
  ldd  r2, r1, r0         # r2 = current thread index
  addi r2, r2, 1          # r2 = next index
  movi r3, thread_count
  ldd  r3, r3, r0         # r3 = total thread count

  # if next >= count, wrap to 0
  slt  r4, r2, r3         # r4 = (next < count) ? 1 : 0
  bne  r4, r0, .no_wrap
  ldi  r2, 0              # wrap around
.no_wrap
  std  r2, r1, r0         # current_thread = next

  # ---- Load next thread's SSP from TCB ----
  ldi  r3, 8
  mul  r2, r2, r3         # r2 = next * 8
  movi r3, tcb_table
  add  r3, r3, r2         # r3 = &tcb_table[next]
  ldd  r7, r3, r0         # r7 = next thread's SSP

  # ---- Restore next thread's context ----
  popd r1
  susp r1                 # restore user stack pointer
  popd r6
  popd r5
  popd r4
  popd r3
  popd r2
  popd r1

  # ---- Return to user code ----
  # RTE pops PC and PSR, swaps to user stack if returning to user mode
  rte


# ============================================================================
# create_thread — Build a fake stack frame for a new thread
# ============================================================================
#
# Arguments:
#   r1 = entry point (PC for the new thread)
#   r2 = user stack top (USP)
#   r3 = supervisor stack top (SSP)
#
# The function builds a stack frame on the supervisor stack that looks
# exactly like what timer_isr saves. When the scheduler first switches
# to this thread, it will "restore" this constructed frame and RTE
# into the thread's entry point in user mode.
#
# Returns: nothing (thread index stored internally)
#
# ============================================================================

global create_thread, func
create_thread
  pshd r6                 # save return address
  pshd r5                 # save frame pointer

  # r3 = supervisor stack top for this thread
  # Build the frame from bottom (highest address) to top (lowest address)
  # Hardware pushes PSR then PC on exception entry, so they're at the bottom

  # PSR: user mode (bit 0 = 0 for user mode, assuming supervisor = 1)
  ldi  r4, 0              # PSR = 0 (user mode, interrupts enabled)
  addi r3, r3, -8
  std  r4, r3, r0         # push PSR

  # PC: thread entry point
  addi r3, r3, -8
  std  r1, r3, r0         # push PC = entry point

  # r1 through r6: all zeroed (fresh thread, no register state)
  ldi  r4, 0
  addi r3, r3, -8
  std  r4, r3, r0         # r1 = 0
  addi r3, r3, -8
  std  r4, r3, r0         # r2 = 0
  addi r3, r3, -8
  std  r4, r3, r0         # r3 = 0
  addi r3, r3, -8
  std  r4, r3, r0         # r4 = 0
  addi r3, r3, -8
  std  r4, r3, r0         # r5 = 0
  addi r3, r3, -8
  std  r4, r3, r0         # r6 = 0

  # USP: user stack pointer
  addi r3, r3, -8
  std  r2, r3, r0         # push USP

  # r3 now points to the top of the constructed frame = the saved SSP

  # Store SSP in tcb_table[thread_count]
  movi r1, thread_count
  ldd  r4, r1, r0         # r4 = current thread_count (= index for new thread)
  ldi  r5, 8
  mul  r5, r4, r5         # r5 = index * 8
  movi r6, tcb_table
  add  r6, r6, r5         # r6 = &tcb_table[index]
  std  r3, r6, r0         # tcb_table[index] = SSP

  # Increment thread count
  addi r4, r4, 1
  std  r4, r1, r0         # thread_count++

  # Return
  popd r5
  popd r6
  jalr r0, r6


# ============================================================================
# Default interrupt handler — does nothing, just returns
# ============================================================================

global default_isr, func
default_isr
  rte
