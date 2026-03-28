; ============================================================================
; Minimal TOS boot stub
; ============================================================================
;
; A minimal boot image that provides the vector table, runtime I/O,
; and calls an extern "main" function. When main returns, the system
; halts.
;
; This is the simplest possible TOS configuration: no scheduling,
; no timer, no threads — just boot → main → halt.
;
; Vector layout matches the CPU's State enum:
;   Slot 0: Initial SSP, Slot 1: Initial PC (boot)
;   Slots 2-19: Exception handlers (all → default_isr)
;
; ============================================================================

STDOUT = 0x100000

segment vectors

  dl 0x0FFFF8            ; Slot 0:  Initial SSP (below devices at 0x100000)
  dl boot                ; Slot 1:  Initial PC
  dl default_isr         ; Slot 2:  Interrupt
  dl default_isr         ; Slot 3:  InstructionAccess
  dl default_isr         ; Slot 4:  DataAccess
  dl default_isr         ; Slot 5:  MisalignedAccess
  dl default_isr         ; Slot 6:  UnimplementedOpcode
  dl default_isr         ; Slot 7:  PrivilegeViolation
  dl default_isr         ; Slot 8:  IllegalDivide
  dl default_isr         ; Slot 9:  Trap0
  dl default_isr         ; Slot 10: Trap1
  dl default_isr         ; Slot 11: Trap2
  dl default_isr         ; Slot 12: Trap3
  dl default_isr         ; Slot 13: Trap4
  dl default_isr         ; Slot 14: Trap5
  dl default_isr         ; Slot 15: Trap6
  dl default_isr         ; Slot 16: Trap7
  dl default_isr         ; Slot 17: Trace
  dl default_isr         ; Slot 18: Overflow
  dl default_isr         ; Slot 19: BoundsCheck

segment code

extern main

global boot, func
entry boot

boot
  movi r4, main
  jalr r6, r4
  ; main returned — system is done
  halt

; ---- Runtime I/O ----

global putchar, func

putchar
  movi r2, STDOUT
  stb r1, r2, r0
  jalr r0, r6

global default_isr, func

default_isr
  halt
  align 8
