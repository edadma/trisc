; ============================================================================
; TOS (Tiny OS) — Boot and Interrupt Glue
; ============================================================================
;
; This file contains the parts of TOS that MUST be written in assembly:
;
;   1. Vector table — hardware requires this at address 0x0000
;   2. Boot entry   — sets up the initial supervisor stack before any
;                     Sysl code can run
;   3. Timer ISR    — saves/restores all registers around the Sysl
;                     scheduler call, then executes RTE
;   4. TRAP handler — system call entry point for user-kernel transitions
;   5. start_first_thread — performs the initial context restore + RTE
;                     to begin executing the first user thread
;   6. Default ISR  — catches unhandled interrupts
;
; Everything else (thread creation, scheduling policy, tasks) is in Sysl.
;
; ============================================================================
; Memory Map
; ============================================================================
;
;   Address Range     Purpose
;   0x0000 - 0x003F   Exception vector table (16 vectors x 4 bytes)
;   0x0100 - 0x3FFF   Kernel code + Sysl code + data
;   0x4000 - 0x4FFF   Thread 0 supervisor stack (4 KB, grows down from 0x5000)
;   0x5000 - 0x5FFF   Thread 0 user stack      (4 KB, grows down from 0x6000)
;   0x6000 - 0x6FFF   Thread 1 supervisor stack (4 KB, grows down from 0x7000)
;   0x7000 - 0x7FFF   Thread 1 user stack      (4 KB, grows down from 0x8000)
;   0x8000 - 0x8FFF   Thread 2 supervisor stack (4 KB, grows down from 0x9000)
;   0x9000 - 0x9FFF   Thread 2 user stack      (4 KB, grows down from 0xA000)
;   0xA000 - 0xAFFF   Thread 3 supervisor stack (4 KB, grows down from 0xB000)
;   0xB000 - 0xBFFF   Thread 3 user stack      (4 KB, grows down from 0xC000)
;   0xE000 - 0xEFFF   Kernel supervisor stack   (4 KB, grows down from 0xF000)
;   0xFFE0            Stdout device (write byte to print character)
;   0xFFE8            Timer device  (write interval in ms to start)
;
; ============================================================================


; ============================================================================
; Exception Vector Table
; ============================================================================
;
; The TRISC CPU reads this table on reset and on every exception.
; Each entry is a 4-byte (32-bit) absolute address of the handler.
;
; Vector  Exception
;   0     Reset (boot entry point)
;   1     Misaligned access
;   2     Illegal instruction
;   3     Privilege violation
;   4     System call (TRAP)
;   5     Timer interrupt
;   6     External interrupt
;   7-15  Reserved
;
; ============================================================================

segment vectors

; Vector 0: Reset
  dw boot
; Vector 1: Misaligned access
  dw default_isr
; Vector 2: Illegal instruction
  dw default_isr
; Vector 3: Privilege violation
  dw default_isr
; Vector 4: System call (TRAP instruction)
  dw trap_handler
; Vector 5: Timer interrupt
  dw timer_isr
; Vector 6: External interrupt
  dw default_isr
; Vector 7: Reserved
  dw default_isr
; Vectors 8-15: Reserved
  dw default_isr
  dw default_isr
  dw default_isr
  dw default_isr
  dw default_isr
  dw default_isr
  dw default_isr
  dw default_isr


segment code

; ============================================================================
; boot — Reset vector handler (entry point)
; ============================================================================
;
; The very first code that executes when the CPU starts.
; Sets r7 (SSP) to the top of the kernel supervisor stack,
; then calls Sysl kernel_main() which never returns.
;
; ============================================================================

global boot, func
entry boot

boot
  ; r7 (SSP) = top of kernel supervisor stack
  movi r7, 0xF000
  ; call kernel_main (never returns)
  movi r4, kernel_main
  jalr r6, r4


; ============================================================================
; timer_isr — Timer Interrupt Service Routine
; ============================================================================
;
; The heart of preemptive multitasking.
;
; On entry: supervisor mode, hardware pushed PC and PSR onto SSP.
;
; Stack frame layout after saving (lowest address = top of stack):
;
;   Offset  Contents     Saved by
;   SP+0    USP          ISR (gusp + pshd)
;   SP+8    r6           ISR (pshr r6 — pushes r1 through r6)
;   SP+16   r5           ISR
;   SP+24   r4           ISR
;   SP+32   r3           ISR
;   SP+40   r2           ISR
;   SP+48   r1           ISR
;   SP+56   PC           Hardware (exception entry)
;   SP+64   PSR          Hardware (exception entry)
;
; ============================================================================

global timer_isr, func

timer_isr
  ; Save r1-r6 in one instruction (r1 pushed first = deepest)
  pshr r6
  ; Save user stack pointer
  gusp r1
  pshd r1

  ; Call Sysl scheduler: r1 = current SSP
  mov  r1, r7
  movi r4, schedule
  jalr r6, r4

  ; Switch to new thread's stack: schedule() returned new SSP in r1
  mov  r7, r1

  ; Restore user stack pointer
  popd r1
  susp r1
  ; Restore r1-r6 in one instruction (r6 popped first = shallowest)
  popr r6

  ; Return to new thread (pops PC and PSR)
  rte


; ============================================================================
; trap_handler — System Call Handler (placeholder)
; ============================================================================

global trap_handler, func

trap_handler
  rte


; ============================================================================
; start_first_thread — Begin executing the first thread
; ============================================================================
;
; Argument: r1 = supervisor stack pointer of the first thread.
; Never returns. Restores the fake context built by create_thread()
; and RTEs into user mode.
;
; ============================================================================

global start_first_thread, func

start_first_thread
  ; Load the thread's supervisor stack
  mov  r7, r1

  ; Restore context (same sequence as timer ISR exit)
  popd r1
  susp r1
  popr r6

  ; RTE into user mode
  rte


; ============================================================================
; default_isr — Unhandled Exception Handler
; ============================================================================

global default_isr, func

default_isr
  halt
