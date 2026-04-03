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
  dl irq_handler           ; Slot 2:  Interrupt
  dl isr_insn_access       ; Slot 3:  InstructionAccess
  dl isr_data_access       ; Slot 4:  DataAccess
  dl isr_misaligned        ; Slot 5:  MisalignedAccess
  dl isr_unimpl            ; Slot 6:  UnimplementedOpcode
  dl isr_priv              ; Slot 7:  PrivilegeViolation
  dl isr_divzero           ; Slot 8:  IllegalDivide
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

extern kernel_init
extern kernel_main

global boot, func
entry boot

boot
  movi r4, kernel_init
  jalr r6, r4
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
extern syscall_table
extern syscall_ssp

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


global timer_isr, func

timer_isr
  cli
  bra context_switch


; ============================================================================
; irq_handler — Generic Interrupt Dispatcher
; ============================================================================

INTC_CLAIM = 0x100082    ; INTC base (0x100080) + offset 2

extern irq_handlers

global irq_handler, func

irq_handler
  cli
  ; Save full context before clobbering any registers
  pshr r6               ; save r1-r6
  gusp r1
  pshd r1               ; save USP

  ; Read CLAIM — identifies source and auto-clears pending
  movi r2, INTC_CLAIM
  ldb  r1, r2, r0        ; r1 = IRQ number (0xFF if spurious)

  ; Check for spurious interrupt
  ldi  r3, 0xFF
  beq  r1, r3, .irq_restore

  ; Save IRQ number
  pshd r1

  ; Look up handler: irq_handlers[r1] (array of 8-byte pointers)
  movi r2, irq_handlers
  ldi  r3, 3
  lsl  r3, r1, r3         ; r3 = r1 << 3 = r1 * 8
  add  r2, r2, r3
  ldd  r2, r2, r0         ; r2 = handler function pointer

  ; If no handler registered (null), skip
  beq  r2, r0, .irq_pop_restore

  ; Call the handler
  jalr r6, r2

  ; Recover IRQ number
  popd r1

  ; Timer (IRQ 0) triggers context switch for preemption
  beq  r1, r0, do_schedule

  ; Non-timer IRQs: restore and return
  bra .irq_restore

.irq_pop_restore
  popd r1               ; discard saved IRQ number
.irq_restore
  popd r1               ; restore USP
  susp r1
  popr r6               ; restore r1-r6
  sti
  rte


; ============================================================================
; trap_handler — System Call Handler
; ============================================================================
;
; Entry: r1 = syscall number, r2 = arg1
; Hardware has pushed PC and PSR onto supervisor stack.
;
; ============================================================================

global trap_handler, func

trap_handler
  cli

  ; Fast path: syscalls that don't context-switch.
  ; Return values go in r1. Only r3 clobbered.
  addi r3, r1, -1
  beq r3, r0, .sys_putc         ; 1 = putc
  ldi r3, 5
  beq r1, r3, .sys_thread_id    ; 5 = thread_id
  ldi r3, 6
  beq r1, r3, .sys_uptime       ; 6 = uptime
  ldi r3, 7
  beq r1, r3, .sys_thread_count ; 7 = thread_count
  ldi r3, 8
  beq r1, r3, .sys_thread_state ; 8 = thread_state(id)
  ldi r3, 9
  beq r1, r3, .sys_thread_name  ; 9 = thread_name(id)
  ldi r3, 10
  beq r1, r3, .sys_sleep_until  ; 10 = sleep_until(tick)
  ldi r3, 11
  beq r1, r3, .sys_kbhit        ; 11 = kbhit
  ldi r3, 12
  beq r1, r3, .sys_getkey       ; 12 = getkey
  ldi r3, 13
  beq r1, r3, .sys_ctx_switches ; 13 = ctx_switches(id)
  ldi r3, 14
  beq r1, r3, .sys_cpu_ticks    ; 14 = cpu_ticks(id)
  ldi r3, 15
  beq r1, r3, .sys_total_switches ; 15 = total_switches
  ldi r3, 16
  beq r1, r3, .sys_set_watchdog ; 16 = set_watchdog(limit)
  ldi r3, 17
  beq r1, r3, .sys_panic        ; 17 = panic
  ldi r3, 18
  beq r1, r3, .sys_check_stack  ; 18 = check_stack(addr)
  ldi r3, 19
  beq r1, r3, .sys_suspend      ; 19 = suspend(id)
  ldi r3, 20
  beq r1, r3, .sys_resume       ; 20 = resume(id)
  ldi r3, 21
  beq r1, r3, .sys_tls_set      ; 21 = tls_set(packed)
  ldi r3, 22
  beq r1, r3, .sys_tls_get      ; 22 = tls_get(slot)
  ldi r3, 23
  beq r1, r3, .sys_notify_send  ; 23 = notify_send(packed)
  ldi r3, 24
  beq r1, r3, .sys_notify_wait  ; 24 = notify_wait
  ldi r3, 25
  beq r1, r3, .sys_notify_read  ; 25 = notify_read
  ldi r3, 26
  beq r1, r3, .sys_event_wait   ; 26 = event_wait(packed)
  ldi r3, 27
  beq r1, r3, .sys_event_set    ; 27 = event_set(packed)
  ldi r3, 28
  beq r1, r3, .sys_event_clear  ; 28 = event_clear(packed)

  ; Slow path: save full context for syscalls that context-switch
  pshr r6                       ; save user's r1-r6
  gusp r1
  pshd r1                       ; save USP

  ; Reload syscall number and arg from saved context
  ; Stack: [USP(+0), r6(+8), r5(+16), r4(+24), r3(+32), r2(+40), r1(+48), PC(+56), PSR(+64)]
  addi r3, r7, 48
  ldd r1, r3, r0               ; r1 = saved r1 (syscall number)
  addi r3, r7, 40
  ldd r2, r3, r0               ; r2 = saved r2 (arg1)
  addi r3, r7, 32
  ldd r3, r3, r0               ; r3 = saved r3 (arg2, for multi-arg syscalls)

  ; Table dispatch: handler = syscall_table[r1]
  ; Bounds check
  ldi r4, 48
  slt r4, r1, r4
  beq r4, r0, .bad_syscall     ; syscall >= 64
  slt r4, r1, r0
  bne r4, r0, .bad_syscall     ; syscall < 0

  ; Load handler from table
  movi r4, syscall_table
  pshd r1                       ; save syscall number
  ldi r5, 3
  lsl r1, r1, r5               ; r1 = syscall_num * 8
  add r4, r4, r1               ; r4 = &syscall_table[num]
  ldd r4, r4, r0               ; r4 = handler address
  popd r1                       ; restore syscall number

  beq r4, r0, .bad_syscall     ; null handler

  ; Save SSP so handlers can write return values to saved context
  movi r5, syscall_ssp
  std r7, r5, r0               ; syscall_ssp = current SSP

  ; Call handler: r1 = arg1 (from saved r2), r2 = arg2 (from saved r3)
  mov r1, r2                   ; shift: r1 = first arg
  mov r2, r3                   ; r2 = second arg (if any)
  jalr r6, r4                  ; call handler
  bra do_schedule

.bad_syscall
  halt

; --- putc: fast path, no context save ---
.sys_putc
  movi r3, STDOUT
  stb  r2, r3, r0
  sti
  rte

; --- Fast-path query syscalls ---
; These don't context-switch. Result returned in r1, then sti + rte.

; thread_id: return current thread index
; (current_thread already declared extern above)

.sys_thread_id
  movi r3, current_thread
  ldw  r1, r3, r0
  sti
  rte

; uptime: return tick counter
extern ticks

.sys_uptime
  movi r3, ticks
  ldw  r1, r3, r0
  sti
  rte

; thread_count: return number of created threads
extern thread_count

.sys_thread_count
  movi r3, thread_count
  ldw  r1, r3, r0
  sti
  rte

; thread_state(id): return state of thread r2
; Calls a kernel function, so we must save/restore r2-r6.
extern query_thread_state

.sys_thread_state
  pshd r2
  pshd r4
  pshd r5
  pshd r6
  mov  r1, r2
  movi r4, query_thread_state
  jalr r6, r4
  popd r6
  popd r5
  popd r4
  popd r2
  sti
  rte

; thread_name(id): return name pointer of thread r2
extern query_thread_name

.sys_thread_name
  pshd r2
  pshd r4
  pshd r5
  pshd r6
  mov  r1, r2
  movi r4, query_thread_name
  jalr r6, r4
  popd r6
  popd r5
  popd r4
  popd r2
  sti
  rte

; kbhit: return 1 if keyboard event buffered (stub — returns 0)
.sys_kbhit
  ldi  r1, 0
  sti
  rte

; getkey: return next keyboard event (stub — returns 0)
.sys_getkey
  ldi  r1, 0
  sti
  rte

; sleep_until(tick): block until absolute tick — needs context switch
extern sleep_until_current

.sys_sleep_until
  ; This one DOES context-switch, but we put it in the fast-path
  ; dispatch for numbering. Save full context now.
  pshr r6
  gusp r1
  pshd r1
  ; Reload arg from saved r2
  addi r3, r7, 40
  ldd r1, r3, r0               ; r1 = saved r2 (target tick)
  movi r4, sleep_until_current
  jalr r6, r4
  bra do_schedule

; ctx_switches(id): return context switch count for thread r2
extern query_thread_ctx_switches

.sys_ctx_switches
  pshd r2
  pshd r4
  pshd r5
  pshd r6
  mov  r1, r2
  movi r4, query_thread_ctx_switches
  jalr r6, r4
  popd r6
  popd r5
  popd r4
  popd r2
  sti
  rte

; cpu_ticks(id): return CPU ticks for thread r2
extern query_thread_cpu_ticks

.sys_cpu_ticks
  pshd r2
  pshd r4
  pshd r5
  pshd r6
  mov  r1, r2
  movi r4, query_thread_cpu_ticks
  jalr r6, r4
  popd r6
  popd r5
  popd r4
  popd r2
  sti
  rte

; total_switches: return global context switch count
extern query_total_ctx_switches

.sys_total_switches
  pshd r4
  pshd r5
  pshd r6
  movi r4, query_total_ctx_switches
  jalr r6, r4
  popd r6
  popd r5
  popd r4
  sti
  rte

; set_watchdog(limit): set watchdog quanta limit
extern kernel_set_watchdog

.sys_set_watchdog
  pshd r2
  pshd r4
  pshd r5
  pshd r6
  mov  r1, r2
  movi r4, kernel_set_watchdog
  jalr r6, r4
  popd r6
  popd r5
  popd r4
  popd r2
  sti
  rte

; panic: terminate all threads — needs context switch
extern kernel_panic

.sys_panic
  pshr r6
  gusp r1
  pshd r1
  movi r4, kernel_panic
  jalr r6, r4
  bra do_schedule

; check_stack(addr): check canary at address r2
extern check_stack_at

.sys_check_stack
  pshd r2
  pshd r4
  pshd r5
  pshd r6
  mov  r1, r2
  movi r4, check_stack_at
  jalr r6, r4
  popd r6
  popd r5
  popd r4
  popd r2
  sti
  rte

; suspend(id): suspend thread r2 — needs context switch if suspending self
extern suspend_thread

.sys_suspend
  pshr r6
  gusp r1
  pshd r1
  addi r3, r7, 40
  ldd r1, r3, r0
  movi r4, suspend_thread
  jalr r6, r4
  bra do_schedule

; resume(id): resume suspended thread r2
extern resume_thread

.sys_resume
  pshd r2
  pshd r4
  pshd r5
  pshd r6
  mov  r1, r2
  movi r4, resume_thread
  jalr r6, r4
  popd r6
  popd r5
  popd r4
  popd r2
  sti
  rte

; tls_set(packed): set TLS value — packed = (slot << 24) | value
extern kernel_tls_set

.sys_tls_set
  pshd r2
  pshd r4
  pshd r5
  pshd r6
  mov  r1, r2
  movi r4, kernel_tls_set
  jalr r6, r4
  popd r6
  popd r5
  popd r4
  popd r2
  sti
  rte

; tls_get(slot): get TLS value for current thread
extern kernel_tls_get

.sys_tls_get
  pshd r2
  pshd r4
  pshd r5
  pshd r6
  mov  r1, r2
  movi r4, kernel_tls_get
  jalr r6, r4
  popd r6
  popd r5
  popd r4
  popd r2
  sti
  rte

; notify_send(packed): send notification — fast path, may wake target
extern notify_send

.sys_notify_send
  pshd r2
  pshd r4
  pshd r5
  pshd r6
  ; Unpack: r2 = (id << 24) | value
  mov  r1, r2
  ldi  r3, 24
  asr  r1, r1, r3            ; r1 = high 8 bits
  ldi  r3, 0xFF
  sli  r3, 0xFF
  sli  r3, 0xFF
  and  r2, r2, r3            ; r2 = value (low 24 bits)
  movi r4, notify_send
  jalr r6, r4
  popd r6
  popd r5
  popd r4
  popd r2
  sti
  rte

; notify_wait: block until notification — needs context switch
extern notify_wait_current

.sys_notify_wait
  pshr r6
  gusp r1
  pshd r1
  movi r4, notify_wait_current
  jalr r6, r4
  bra do_schedule

; notify_read: read and clear own notification — fast path
extern notify_read

.sys_notify_read
  pshd r4
  pshd r5
  pshd r6
  movi r2, current_thread
  ldw  r1, r2, r0
  movi r4, notify_read
  jalr r6, r4
  popd r6
  popd r5
  popd r4
  sti
  rte

; event_wait(packed): block until bits match — needs context switch
extern event_wait_current

.sys_event_wait
  pshr r6
  gusp r1
  pshd r1
  ; Reload saved r2 from stack
  addi r3, r7, 40
  ldd  r2, r3, r0            ; r2 = packed arg
  ; Unpack: wait_all = high 8 bits, mask = low 24 bits
  mov  r1, r2
  ldi  r3, 24
  asr  r1, r1, r3            ; r1 = wait_all
  ldi  r3, 0xFF
  sli  r3, 0xFF
  sli  r3, 0xFF
  and  r2, r2, r3            ; r2 = mask (low 24 bits)
  ; Call event_wait_current(mask, wait_all)
  ; But this is a 2-arg kernel function — r1=mask, r2=wait_all
  mov  r3, r1                ; r3 = wait_all
  mov  r1, r2                ; r1 = mask
  mov  r2, r3                ; r2 = wait_all
  movi r4, event_wait_current
  jalr r6, r4
  bra do_schedule

; event_set(packed): set bits on target — fast path
extern event_set_bits

.sys_event_set
  pshd r2
  pshd r4
  pshd r5
  pshd r6
  mov  r1, r2
  ldi  r3, 24
  asr  r1, r1, r3            ; r1 = target id
  ldi  r3, 0xFF
  sli  r3, 0xFF
  sli  r3, 0xFF
  and  r2, r2, r3            ; r2 = bits
  movi r4, event_set_bits
  jalr r6, r4
  popd r6
  popd r5
  popd r4
  popd r2
  sti
  rte

; event_clear(packed): clear bits on target — fast path
extern event_clear_bits

.sys_event_clear
  pshd r2
  pshd r4
  pshd r5
  pshd r6
  mov  r1, r2
  ldi  r3, 24
  asr  r1, r1, r3            ; r1 = target id
  ldi  r3, 0xFF
  sli  r3, 0xFF
  sli  r3, 0xFF
  and  r2, r2, r3            ; r2 = bits
  movi r4, event_clear_bits
  jalr r6, r4
  popd r6
  popd r5
  popd r4
  popd r2
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
;   r2 = second arg
;
; Trap convention: r1 = number, r2 = arg
;
; ============================================================================

global syscall, func

syscall
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

; --- Per-exception-type ISR handlers ---
; Each prints a unique marker to STDOUT so we know which exception fired.

global isr_insn_access, func
isr_insn_access
  movi r2, STDOUT
  ldi  r1, 73            ; 'I' = InstructionAccess
  stb  r1, r2, r0
  halt

global isr_data_access, func
isr_data_access
  movi r2, STDOUT
  ldi  r1, 68            ; 'D' = DataAccess
  stb  r1, r2, r0
  halt

global isr_misaligned, func
isr_misaligned
  movi r2, STDOUT
  ldi  r1, 65            ; 'A' = MisalignedAccess
  stb  r1, r2, r0
  halt

global isr_unimpl, func
isr_unimpl
  movi r2, STDOUT
  ldi  r1, 85            ; 'U' = UnimplementedOpcode
  stb  r1, r2, r0
  halt

global isr_priv, func
isr_priv
  movi r2, STDOUT
  ldi  r1, 80            ; 'P' = PrivilegeViolation
  stb  r1, r2, r0
  halt

global isr_divzero, func
isr_divzero
  movi r2, STDOUT
  ldi  r1, 90            ; 'Z' = IllegalDivide
  stb  r1, r2, r0
  halt

global default_isr, func
default_isr
  movi r2, STDOUT
  ldi  r1, 63            ; '?' = Unknown exception
  stb  r1, r2, r0
  halt
