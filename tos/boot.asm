# ============================================================================
# TOS (Tiny OS) — Boot and Interrupt Glue
# ============================================================================
#
# This file contains the parts of TOS that MUST be written in assembly:
#
#   1. Vector table — hardware requires this at address 0x0000
#   2. Boot entry   — sets up the initial supervisor stack before any
#                     Sysl code can run
#   3. Timer ISR    — saves/restores all registers around the Sysl
#                     scheduler call, then executes RTE
#   4. TRAP handler — system call entry point for user→kernel transitions
#   5. start_first_thread — performs the initial context restore + RTE
#                     to begin executing the first user thread
#   6. Default ISR  — catches unhandled interrupts
#
# Everything else (thread creation, scheduling policy, tasks) is in Sysl.
#
# ============================================================================
# Memory Map
# ============================================================================
#
#   Address Range     Purpose
#   ─────────────     ──────────────────────────────────────────────
#   0x0000 - 0x003F   Exception vector table (16 vectors × 4 bytes)
#   0x0100 - 0x3FFF   Kernel code + Sysl code + data
#   0x4000 - 0x4FFF   Thread 0 supervisor stack (4 KB, grows down from 0x5000)
#   0x5000 - 0x5FFF   Thread 0 user stack      (4 KB, grows down from 0x6000)
#   0x6000 - 0x6FFF   Thread 1 supervisor stack (4 KB, grows down from 0x7000)
#   0x7000 - 0x7FFF   Thread 1 user stack      (4 KB, grows down from 0x8000)
#   0x8000 - 0x8FFF   Thread 2 supervisor stack (4 KB, grows down from 0x9000)
#   0x9000 - 0x9FFF   Thread 2 user stack      (4 KB, grows down from 0xA000)
#   0xA000 - 0xAFFF   Thread 3 supervisor stack (4 KB, grows down from 0xB000)
#   0xB000 - 0xBFFF   Thread 3 user stack      (4 KB, grows down from 0xC000)
#   0xE000 - 0xEFFF   Kernel supervisor stack   (4 KB, grows down from 0xF000)
#   0xFFE0            Stdout device (write byte to print character)
#   0xFFE8            Timer device  (write interval in ms to start)
#
# Each thread gets two stacks:
#   - Supervisor stack: used when the CPU is in supervisor mode
#     (during interrupts, system calls). The thread's full register
#     context is saved here during a context switch.
#   - User stack: used when the thread is running in user mode.
#     The thread's own local variables, function calls, etc.
#
# ============================================================================


# ============================================================================
# Exception Vector Table
# ============================================================================
#
# The TRISC CPU reads this table on reset and on every exception.
# Each entry is a 4-byte (32-bit) absolute address of the handler.
#
# Vector  Exception
# ──────  ──────────────────
#   0     Reset (boot entry point)
#   1     Misaligned access
#   2     Illegal instruction
#   3     Privilege violation
#   4     System call (TRAP)
#   5     Timer interrupt
#   6     External interrupt
#   7     (reserved)
#   8     Overflow (TRAPV)
#   9     Bounds check (CHK)
#  10     Trace (single-step)
#  11-15  (reserved)
#
# ============================================================================

segment vectors

  dw boot               # Vector 0:  Reset — CPU starts here
  dw default_isr        # Vector 1:  Misaligned access
  dw default_isr        # Vector 2:  Illegal instruction
  dw default_isr        # Vector 3:  Privilege violation
  dw trap_handler       # Vector 4:  System call (TRAP instruction)
  dw timer_isr          # Vector 5:  Timer interrupt
  dw default_isr        # Vector 6:  External interrupt
  dw default_isr        # Vector 7:  Reserved
  dw default_isr        # Vector 8:  Overflow
  dw default_isr        # Vector 9:  Bounds check
  dw default_isr        # Vector 10: Trace
  dw default_isr        # Vector 11: Reserved
  dw default_isr        # Vector 12: Reserved
  dw default_isr        # Vector 13: Reserved
  dw default_isr        # Vector 14: Reserved
  dw default_isr        # Vector 15: Reserved


segment code

# ============================================================================
# boot — Reset vector handler (entry point)
# ============================================================================
#
# The very first code that executes when the CPU starts.
#
# At this point:
#   - CPU is in supervisor mode
#   - All registers are undefined
#   - We need to set up a stack before calling any Sysl code
#
# We set r7 (SSP) to the top of the kernel supervisor stack area,
# then jump to the Sysl kernel_main() function which handles
# everything else: creating threads, starting the timer, and
# launching the first thread.
#
# kernel_main() never returns — it ends by calling start_first_thread().
#
# ============================================================================

global boot, func
entry boot

boot
  # ---- Step 1: Initialize the kernel's supervisor stack pointer ----
  #
  # The kernel stack occupies 0xE000-0xEFFF (4 KB).
  # Stacks grow downward, so we set r7 to 0xF000 (one past the end).
  # The first push will write to 0xEFF8 (for an 8-byte push).

  movi r7, 0xF000         # r7 (SSP) = top of kernel supervisor stack

  # ---- Step 2: Jump to Sysl kernel_main() ----
  #
  # This is a normal function call using our calling convention:
  #   - r4 = function address (loaded via movi)
  #   - jalr r6, r4 = jump to r4, save return address in r6
  #
  # kernel_main() will:
  #   1. Create all threads (building their initial stack frames)
  #   2. Start the timer (for preemptive scheduling)
  #   3. Call start_first_thread() to begin running thread 0
  #   4. Never return

  movi r4, kernel_main    # r4 = address of kernel_main
  jalr r6, r4             # call kernel_main (never returns)


# ============================================================================
# timer_isr — Timer Interrupt Service Routine
# ============================================================================
#
# This is the heart of preemptive multitasking. The timer device fires
# this interrupt periodically (e.g., every 10ms). The CPU automatically:
#
#   1. Switches to supervisor mode
#   2. Swaps r7 with the SSP (if coming from user mode, the USP is
#      saved internally and r7 becomes the thread's supervisor stack)
#   3. Pushes the PC (return address) onto the supervisor stack
#   4. Pushes the PSR (processor status register) onto the stack
#   5. Jumps to this handler
#
# Our job is to:
#   a. Save all remaining registers (r1-r6, USP) onto the supervisor stack
#   b. Call the Sysl schedule() function to pick the next thread
#   c. Restore all registers from the new thread's supervisor stack
#   d. Execute RTE to return to the new thread's user code
#
# Stack frame layout after saving (lowest address = top of stack):
#
#   Offset  Contents     Saved by
#   ──────  ────────     ────────
#   SP+0    USP          ISR (this code)
#   SP+8    r6           ISR
#   SP+16   r5           ISR
#   SP+24   r4           ISR
#   SP+32   r3           ISR
#   SP+40   r2           ISR
#   SP+48   r1           ISR
#   SP+56   PC           Hardware (exception entry)
#   SP+64   PSR          Hardware (exception entry)
#
# This is the complete thread context. When we context-switch, we just
# swap the stack pointer — the entire thread state is on the stack.
#
# ============================================================================

global timer_isr, func

timer_isr
  # ---- Save current thread's registers ----
  #
  # The hardware already pushed PC and PSR. We push the rest.
  # Order matters: we'll pop in reverse order during restore.

  pshd r1                 # save general-purpose registers
  pshd r2
  pshd r3
  pshd r4
  pshd r5
  pshd r6
  gusp r1                 # r1 = user stack pointer (USP)
  pshd r1                 # save USP onto supervisor stack

  # ---- Call Sysl scheduler ----
  #
  # schedule() takes the current SSP (r7) as its argument in r1.
  # It saves this SSP into the current thread's TCB, picks the
  # next thread, and returns the new thread's SSP in r1.

  mov  r1, r7             # r1 = current supervisor stack pointer
  movi r4, schedule       # r4 = address of schedule()
  jalr r6, r4             # call schedule(current_ssp) → returns new SSP

  # ---- Switch to new thread's stack ----

  mov  r7, r1             # r7 = new thread's supervisor stack pointer

  # ---- Restore new thread's registers ----
  #
  # Pop in reverse order of the save above.

  popd r1                 # restore USP
  susp r1                 # set user stack pointer
  popd r6                 # restore general-purpose registers
  popd r5
  popd r4
  popd r3
  popd r2
  popd r1

  # ---- Return to new thread ----
  #
  # RTE (Return from Exception) pops PC and PSR from the stack.
  # If the PSR indicates user mode (which it will, since threads
  # run in user mode), the CPU also swaps r7 back to the USP,
  # restoring the thread's user stack pointer.

  rte


# ============================================================================
# trap_handler — System Call Handler
# ============================================================================
#
# Entered when user code executes a TRAP instruction.
# The trap number (0-7) identifies which system call is requested.
#
# For now, this is a placeholder. A real implementation would:
#   - Read the trap number from the instruction
#   - Dispatch to the appropriate kernel function
#   - Return the result to the calling thread
#
# System calls we might implement:
#   TRAP 0: yield      — voluntarily give up the CPU
#   TRAP 1: exit       — terminate the current thread
#   TRAP 2: sleep      — block for N milliseconds
#   TRAP 3: putchar    — write a character to stdout
#
# ============================================================================

global trap_handler, func

trap_handler
  # For now, just return to the caller without doing anything.
  # The TRAP instruction pushed PC and PSR onto the supervisor stack.
  rte


# ============================================================================
# start_first_thread — Begin executing the first thread
# ============================================================================
#
# Called from Sysl kernel_main() after all threads have been created
# and the timer has been started.
#
# Argument:
#   r1 = supervisor stack pointer of the first thread to run
#
# This function never returns. It performs the same register restore
# sequence as the timer ISR exit, but it's the first time — there's
# no previous thread to save. The thread's stack frame was built by
# create_thread() to look exactly like a saved context.
#
# ============================================================================

global start_first_thread, func

start_first_thread
  # ---- Load the thread's supervisor stack ----

  mov  r7, r1             # r7 = thread's saved SSP

  # ---- Restore the thread's context ----
  #
  # This is identical to the timer ISR restore sequence.
  # The "saved" context was actually constructed by create_thread().

  popd r1                 # restore USP
  susp r1                 # set user stack pointer
  popd r6                 # restore r6
  popd r5                 # restore r5
  popd r4                 # restore r4
  popd r3                 # restore r3
  popd r2                 # restore r2
  popd r1                 # restore r1

  # ---- RTE into user mode ----
  #
  # Pops the constructed PC (= thread entry point) and PSR
  # (= user mode, interrupts enabled). The CPU switches to
  # user mode and begins executing the thread's code.

  rte


# ============================================================================
# default_isr — Unhandled Exception Handler
# ============================================================================
#
# Catches any exception that doesn't have a specific handler.
# In a production OS, this would log the fault and kill the thread.
# For now, it just halts the CPU.
#
# ============================================================================

global default_isr, func

default_isr
  halt                    # stop the CPU — unhandled exception
