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

STDOUT = 0x100000

; ============================================================================
; Exception Vector Table
; ============================================================================

segment vectors

  dl 0x0FFFF8              ; Slot 0:  Initial SSP (kernel stack top, below devices)
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
  movi r7, 0x0FFFF8            ; clean kernel stack (below devices)
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

  ; Fast path: putc (syscall 1) — no context save needed.
  ; Only r3 is clobbered, which is fine since we RTE directly
  ; (hardware restores PC+PSR, user r1-r6 are untouched on stack).
  addi r3, r1, -1
  beq r3, r0, .sys_putc        ; r1 == 1 → putc

  ; Slow path: save full context for syscalls that context-switch
  pshr r6                       ; save user's r1-r6
  gusp r1
  pshd r1                       ; save USP

  ; Reload syscall number and arg from saved context
  ; Stack: [USP(+0), r6(+8), r5(+16), r4(+24), r3(+32), r2(+40), r1(+48), PC(+56), PSR(+64)]
  addi r3, r7, 48
  ldd r1, r3, r0               ; r1 = saved r1 (syscall number)
  addi r3, r7, 40
  ldd r2, r3, r0               ; r2 = saved r2 (arg)

  ; Dispatch
  beq r1, r0, .sys_sleep       ; 0 = sleep
  ldi r3, 2
  beq r1, r3, .sys_yield       ; 2 = yield
  ldi r3, 3
  beq r1, r3, .sys_exit        ; 3 = exit
  ldi r3, 4
  beq r1, r3, .sys_join        ; 4 = join
  ldi r3, 5
  beq r1, r3, .sys_thread_id   ; 5 = thread_id

  ; Unknown syscall — halt (indicates a bug)
  halt

; --- putc: fast path, no context save ---
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
  mov  r1, r2                   ; r1 = ticks arg for sleep_current
  movi r4, sleep_current
  jalr r6, r4                   ; marks current thread BLOCKED
  bra do_schedule

; --- exit: terminate current thread, context switch away ---
extern terminate_current

.sys_exit
  movi r4, terminate_current
  jalr r6, r4                   ; marks current thread TERMINATED
  bra do_schedule

; --- join: wait for thread r2 to terminate ---
extern join_current

.sys_join
  mov  r1, r2                   ; r1 = target thread id
  movi r4, join_current
  jalr r6, r4                   ; marks current thread JOINING
  bra do_schedule

; --- thread_id: return current thread index ---
; (current_thread already declared extern above)

.sys_thread_id
  ; Return current_thread in r1 via saved context
  ; Stack: [USP(+0), r6(+8), r5(+16), r4(+24), r3(+32), r2(+40), r1(+48)]
  movi r3, current_thread
  ldw  r1, r3, r0               ; r1 = current_thread
  addi r3, r7, 48
  std  r1, r3, r0               ; overwrite saved r1 with thread id
  ; Restore context and return (no context switch needed)
  popd r1
  susp r1                       ; restore USP
  popr r6                       ; restore r1-r6 (r1 now has thread id)
  sti
  rte


; ============================================================================
; Syscall wrappers — called from user Sysl code
; ============================================================================
;
; syscall(number: int, arg: int) -> int
;
; Generic syscall bridge. Sysl calling convention:
;   r1 = first arg (syscall number)
;   second arg pushed on stack by caller
;
; Trap convention: r1 = number, r2 = arg
;
; ============================================================================

global syscall, func

syscall
  ldd  r2, r7, r0      ; r2 = arg (on stack, pushed by caller)
  trap 0                ; r1 = number, r2 = arg
  jalr r0, r6           ; return (r1 = return value from trap handler)

; thread_exit — trampoline for tasks that return from their entry function.
; create_thread sets r6 in the fake context to this address, so when a
; task's main function does "jalr r0, r6" (return), it lands here.
global thread_exit, func

thread_exit
  ldi  r1, 3            ; r1 = SYS_EXIT
  ldi  r2, 0            ; r2 = unused
  trap 0
  ; never returns — schedule switches to another thread


; ============================================================================
; Atomic operations — ll/sc wrappers for Sysl code
; ============================================================================

; atomic_load(addr: *int) -> int
; Atomically read a 64-bit value.
global atomic_load, func

atomic_load
  ldd  r1, r1, r0
  jalr r0, r6

; atomic_dec_if_positive(addr: *int) -> int
; If *addr > 0, atomically decrement and return 1. Else return 0.
global atomic_dec_if_positive, func

atomic_dec_if_positive
  ll   r2, r1              ; r2 = *addr (load-linked)
  beq  r2, r0, .adip_fail  ; if zero, can't decrement
  addi r2, r2, -1          ; r2 = value - 1
  sc   r2, r1              ; try store-conditional
  beq  r2, r0, atomic_dec_if_positive  ; sc failed (r2=0), retry
  ldi  r1, 1               ; success
  jalr r0, r6
.adip_fail
  ldi  r1, 0               ; value was zero
  jalr r0, r6

; atomic_inc(addr: *int)
; Atomically increment *addr.
global atomic_inc, func

atomic_inc
  ll   r2, r1              ; r2 = *addr (load-linked)
  addi r2, r2, 1           ; r2 = value + 1
  sc   r2, r1              ; try store-conditional
  beq  r2, r0, atomic_inc  ; sc failed (r2=0), retry
  jalr r0, r6


; ============================================================================
; default_isr — Unhandled Exception Handler
; ============================================================================

global default_isr, func

default_isr
  halt
