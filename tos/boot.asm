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
;   0x0000 - 0x009F   Exception vector table (20 slots x 8 bytes = 160 bytes)
;   0x00A0 - 0x3FFF   Kernel code + Sysl code + data
;   0x4000 - 0x4FFF   Thread 0 supervisor stack (4 KB, grows down from 0x5000)
;   0x5000 - 0x5FFF   Thread 0 user stack      (4 KB, grows down from 0x6000)
;   0x6000 - 0x6FFF   Thread 1 supervisor stack (4 KB, grows down from 0x7000)
;   0x7000 - 0x7FFF   Thread 1 user stack      (4 KB, grows down from 0x8000)
;   0x8000 - 0x8FFF   Thread 2 supervisor stack (4 KB, grows down from 0x9000)
;   0x9000 - 0x9FFF   Thread 2 user stack      (4 KB, grows down from 0xA000)
;   0xA000 - 0xAFFF   Thread 3 supervisor stack (4 KB, grows down from 0xB000)
;   0xB000 - 0xBFFF   Thread 3 user stack      (4 KB, grows down from 0xC000)
;   0xE000 - 0xEFFF   Kernel supervisor stack   (4 KB, grows down from 0xF000)
;   0xFF00            Stdout device (write byte to print character)
;   0xFFE8            Timer device  (write interval in ms to start)
;
; ============================================================================


; ============================================================================
; Exception Vector Table
; ============================================================================
;
; The TRISC CPU reads this table on reset and on every exception.
; Each entry is an 8-byte (64-bit) absolute address.
;
; The vector layout matches the CPU's State enum ordering.
; Reset is special: slot 0 = initial SSP, slot 1 = initial PC.
; All other exceptions use: pc = mem.readLong((state.ordinal + 1) * 8)
;
; Slot  Address  State enum         Handler
;  0    0x0000   (Reset SSP)        —
;  1    0x0008   (Reset PC)         boot
;  2    0x0010   Interrupt          timer_isr
;  3    0x0018   InstructionAccess  default_isr
;  4    0x0020   DataAccess         default_isr
;  5    0x0028   MisalignedAccess   default_isr
;  6    0x0030   UnimplementedOp    default_isr
;  7    0x0038   PrivilegeViolation default_isr
;  8    0x0040   IllegalDivide      default_isr
;  9    0x0048   Trap0              trap_handler
; 10    0x0050   Trap1              trap_handler
; 11    0x0058   Trap2              trap_handler
; 12    0x0060   Trap3              trap_handler
; 13    0x0068   Trap4              trap_handler
; 14    0x0070   Trap5              trap_handler
; 15    0x0078   Trap6              trap_handler
; 16    0x0080   Trap7              trap_handler
; 17    0x0088   Trace              default_isr
; 18    0x0090   Overflow           default_isr
; 19    0x0098   BoundsCheck        default_isr
;
; ============================================================================

segment vectors

; Reset vector is special: slot 0 = initial SSP, slot 1 = initial PC
; (each slot is 8 bytes since CPU uses readLong)
  dl 0xF000                ; Slot 0:  Initial SSP (kernel stack top)
  dl boot                  ; Slot 1:  Initial PC (boot entry point)
; Exception vectors (8 bytes each)
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
; boot — Reset vector handler (entry point)
; ============================================================================
;
; The very first code that executes when the CPU starts.
; SSP is already loaded from vector slot 0 by the CPU reset sequence.
; Calls Sysl kernel_main() which never returns.
;
; ============================================================================

extern kernel_main

global boot, func
entry boot

boot
  ; SSP is already set from vector table slot 0 (0xF000)
  ; Call kernel_main (never returns)
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

extern schedule

global timer_isr, func

timer_isr
  ; Disable interrupts while we do the context switch
  cli

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

  ; Re-enable interrupts before returning
  sti

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
