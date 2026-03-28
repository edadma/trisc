; ============================================================================
; TOS (Tiny OS) — Boot and Interrupt Glue
; ============================================================================
;
; This file contains the parts of TOS that MUST be written in assembly:
;
;   1. Vector table — hardware requires this at address 0x0000
;   2. Boot entry   — calls kernel_main(), uses return value to start
;                     the first thread
;   3. Timer ISR    — saves/restores all registers around the Sysl
;                     scheduler call, then executes RTE
;   4. TRAP handler — system call entry point (placeholder)
;   5. Default ISR  — catches unhandled interrupts
;
; kernel_main() is provided by the application. It must:
;   - Call create_thread() for each task
;   - Start the timer
;   - Return the first thread's SSP
;
; ============================================================================
; Memory Map
; ============================================================================
;
;   Address Range     Purpose
;   0x0000 - 0x009F   Exception vector table (20 slots x 8 bytes)
;   0x00A0 - 0x3FFF   Kernel + Sysl code + data
;   0x4000 - 0x4FFF   Thread 0 supervisor stack (grows down from 0x5000)
;   0x5000 - 0x5FFF   Thread 0 user stack      (grows down from 0x6000)
;   0x6000 - 0x6FFF   Thread 1 supervisor stack (grows down from 0x7000)
;   0x7000 - 0x7FFF   Thread 1 user stack      (grows down from 0x8000)
;   0x8000 - 0x8FFF   Thread 2 supervisor stack (grows down from 0x9000)
;   0x9000 - 0x9FFF   Thread 2 user stack      (grows down from 0xA000)
;   0xA000 - 0xAFFF   Thread 3 supervisor stack (grows down from 0xB000)
;   0xB000 - 0xBFFF   Thread 3 user stack      (grows down from 0xC000)
;   0xE000 - 0xEFFF   Kernel supervisor stack   (grows down from 0xF000)
;   0xFF00            Stdout device (write byte to print character)
;   0xFFE8            Timer device  (write interval in ms to start)
;
; ============================================================================


; ============================================================================
; Exception Vector Table
; ============================================================================

segment vectors

  dl 0xF000                ; Slot 0:  Initial SSP (kernel stack top)
  dl boot                  ; Slot 1:  Initial PC
  dl timer_isr             ; Slot 2:  Interrupt
  dl default_isr           ; Slot 3:  InstructionAccess
  dl default_isr           ; Slot 4:  DataAccess
  dl default_isr           ; Slot 5:  MisalignedAccess
  dl default_isr           ; Slot 6:  UnimplementedOpcode
  dl default_isr           ; Slot 7:  PrivilegeViolation
  dl default_isr           ; Slot 8:  IllegalDivide
  dl trap_handler          ; Slot 9:  Trap0
  dl trap_handler          ; Slot 10: Trap1
  dl trap_handler          ; Slot 11: Trap2
  dl trap_handler          ; Slot 12: Trap3
  dl trap_handler          ; Slot 13: Trap4
  dl trap_handler          ; Slot 14: Trap5
  dl trap_handler          ; Slot 15: Trap6
  dl trap_handler          ; Slot 16: Trap7
  dl default_isr           ; Slot 17: Trace
  dl default_isr           ; Slot 18: Overflow
  dl default_isr           ; Slot 19: BoundsCheck


segment code

; ============================================================================
; boot — Reset vector handler
; ============================================================================
;
; Calls kernel_main() which returns the first thread's SSP in r1.
; Then restores the fake context and RTEs into user mode.
;
; ============================================================================

extern kernel_main

global boot, func
entry boot

boot
  movi r4, kernel_main
  jalr r6, r4
  ; r1 = first thread's SSP — fall through to start_first_thread

; ============================================================================
; start_first_thread — Begin executing the first thread
; ============================================================================
;
; r1 = supervisor stack pointer of the first thread.
; Restores the fake context built by create_thread() and RTEs into
; user mode. Never returns.
;
; ============================================================================

global start_first_thread, func

start_first_thread
  mov  r7, r1
  popd r1
  susp r1
  popr r6
  rte


; ============================================================================
; timer_isr — Timer Interrupt Service Routine
; ============================================================================
;
; Saves full context, calls schedule(), switches to new thread's stack,
; restores context, RTEs.
;
; ============================================================================

extern schedule

global timer_isr, func

timer_isr
  cli
  pshr r6
  gusp r1
  pshd r1

  mov  r1, r7
  movi r4, schedule
  jalr r6, r4

  mov  r7, r1

  popd r1
  susp r1
  popr r6

  sti
  rte


; ============================================================================
; trap_handler — System Call Handler (placeholder)
; ============================================================================

global trap_handler, func

trap_handler
  rte


; ============================================================================
; default_isr — Unhandled Exception Handler
; ============================================================================

global default_isr, func

default_isr
  halt
