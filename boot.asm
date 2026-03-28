; ============================================================================
; TOS (Tiny OS) — Boot, ISR, Trap Handler, Syscall Wrappers
; ============================================================================
;
; Syscall convention:
;   r1 = syscall number, r2 = arg1
;   Return value in r1
;
; Syscall numbers:
;   0 = sleep(ticks)    — block current thread for N ticks
;   1 = putc(char)      — write character to stdout
;   2 = yield()         — voluntary context switch
;
; ============================================================================

STDOUT = 0xFF00

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

global start_first_thread, func

start_first_thread
  mov  r7, r1
  popd r1
  susp r1
  popr r6
  rte


; ============================================================================
; context_switch — Common save/schedule/restore sequence
; ============================================================================
;
; Called with interrupts disabled, in supervisor mode.
; Saves current thread's full context, calls schedule(),
; restores next thread's context and RTEs.
;
; ============================================================================

extern schedule
extern current_thread

context_switch
  pshr r6               ; save r1-r6
  gusp r1               ; get user stack pointer
  pshd r1               ; save USP

do_schedule
  mov  r1, r7           ; r1 = current SSP (with saved context)
  movi r4, schedule
  jalr r6, r4           ; r1 = next thread's SSP (or 0 = idle)

  bne  r1, r0, restore_thread

  ; No threads ready — kernel idle with wfi
  movi r7, 0xF000              ; clean kernel stack
  movi r2, current_thread
  ldi  r1, -1                  ; mark no current thread
  stw  r1, r2, r0
  sti                          ; enable interrupts for timer
idle_spin
  wfi                          ; halt until timer fires
  bra idle_spin                ; timer ISR will context_switch to a woken thread

restore_thread
  mov  r7, r1           ; switch to next thread's stack
  popd r1               ; restore USP
  susp r1
  popr r6               ; restore r1-r6
  sti                   ; re-enable interrupts
  rte                   ; return to next thread


; ============================================================================
; timer_isr — Timer Interrupt Service Routine
; ============================================================================

global timer_isr, func

timer_isr
  cli
  bra context_switch


; ============================================================================
; trap_handler — System Call Handler
; ============================================================================
;
; Entry: r1 = syscall number, r2 = arg1
; Hardware has pushed PC and PSR onto supervisor stack.
;
; ============================================================================

extern sleep_current

global trap_handler, func

trap_handler
  cli

  ; Dispatch on syscall number (r1)
  ; putc is fast — handle before saving full context
  ldi r3, 1
  beq r1, r3, .sys_putc        ; 1 = putc (no context switch needed)

  ; For sleep/yield, save full context first (before calling kernel)
  pshr r6                       ; save user's r1-r6
  gusp r1
  pshd r1                       ; save USP

  ; Reload syscall number and arg from saved context on stack
  ; Stack: [USP(+0), r6(+8), r5(+16), r4(+24), r3(+32), r2(+40), r1(+48), PC(+56), PSR(+64)]
  addi r3, r7, 48
  ldd r1, r3, r0               ; r1 = saved r1 (syscall number)
  addi r3, r7, 40
  ldd r2, r3, r0               ; r2 = saved r2 (arg)

  beq r1, r0, .sys_sleep       ; 0 = sleep
  ldi r3, 2
  beq r1, r3, .sys_yield       ; 2 = yield

  ; Unknown — restore and return
  popd r1
  susp r1
  popr r6
  sti
  rte

; --- putc: write r2 to stdout, return (no context switch) ---
.sys_putc
  movi r3, STDOUT
  stb  r2, r3, r0
  sti
  rte

; --- yield: voluntary context switch (context already saved) ---
.sys_yield
  ; Context already saved by pshr/pshd above.
  ; Jump to the schedule+dispatch part of context_switch.
  bra do_schedule

; --- sleep: block current thread, then context switch ---
.sys_sleep
  ; r2 = number of ticks to sleep (reloaded from saved context)
  mov  r1, r2                   ; r1 = ticks arg for sleep_current
  movi r4, sleep_current
  jalr r6, r4                   ; marks current thread BLOCKED
  bra do_schedule


; ============================================================================
; Syscall wrappers — called from user Sysl code
; ============================================================================
;
; Sysl calling convention: first arg in r1, rest on stack.
; These wrappers shuffle r1 → r2 (arg) and load syscall number into r1.
;
; ============================================================================

; sleep(ticks: int)
global sleep, func

sleep
  mov  r2, r1           ; r2 = ticks (was first Sysl arg)
  ldi  r1, 0            ; r1 = SYS_SLEEP
  trap 0
  jalr r0, r6           ; return to caller

; putc(ch: int)
global putc, func

putc
  mov  r2, r1           ; r2 = char
  ldi  r1, 1            ; r1 = SYS_PUTC
  trap 0
  jalr r0, r6

; yield()
global yield, func

yield
  ldi  r1, 2            ; r1 = SYS_YIELD
  trap 0
  jalr r0, r6


; ============================================================================
; default_isr — Unhandled Exception Handler
; ============================================================================

global default_isr, func

default_isr
  halt
