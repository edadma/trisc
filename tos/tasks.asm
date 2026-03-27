# ============================================================================
# TOS — User tasks
# ============================================================================
#
# These are simple user-mode programs that run as threads.
# Each task runs in user mode with its own user stack.
# The scheduler preempts them via timer interrupts.
#
# For demonstration, each task writes a character to the stdout device
# in a loop, so you can see interleaving:
#   Task A prints 'A' repeatedly
#   Task B prints 'B' repeatedly
#   Idle task does nothing (absorbs CPU when no other task is ready)
#
# Stdout device: write a byte to 0xFFE0 to output a character.
#
# ============================================================================

# ============================================================================
# Task A — prints 'A' forever
# ============================================================================

global task_a, func
task_a
  movi r1, 0xFFE0         # r1 = stdout device address
  ldi  r2, 65             # r2 = 'A'
.loop_a
  stb  r2, r1, r0         # write 'A' to stdout
  # Busy wait — gives the timer a chance to fire
  ldi  r3, 0
  movi r4, 1000
.wait_a
  addi r3, r3, 1
  slt  r5, r3, r4
  bne  r5, r0, .wait_a
  bra  .loop_a


# ============================================================================
# Task B — prints 'B' forever
# ============================================================================

global task_b, func
task_b
  movi r1, 0xFFE0         # r1 = stdout device address
  ldi  r2, 66             # r2 = 'B'
.loop_b
  stb  r2, r1, r0         # write 'B' to stdout
  # Busy wait
  ldi  r3, 0
  movi r4, 1000
.wait_b
  addi r3, r3, 1
  slt  r5, r3, r4
  bne  r5, r0, .wait_b
  bra  .loop_b


# ============================================================================
# Idle task — runs when nothing else is ready
# ============================================================================
#
# In a real RTOS this would execute WFI (wait for interrupt) to save
# power. For now it just spins.
#
# ============================================================================

global idle_task, func
idle_task
.idle_loop
  # Could use: wfi
  bra  .idle_loop
