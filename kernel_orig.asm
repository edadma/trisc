global MAX_THREADS, data, i32
global STATE_READY, data, i32
global STATE_RUNNING, data, i32
global STATE_BLOCKED, data, i32
global STATE_TERMINATED, data, i32
global tasks, data, arr 8 struct Task 4 ssp i32 state i32 wake_tick i32 name ptr i8
global current_thread, data, i32
global thread_count, data, i32
global ticks, data, i32
global create_thread, func, 4 i32 i32 i32 ptr i8 void
global sleep_current, func, 1 i32 void
global terminate_current, func, 0 void
global schedule, func, 1 i32 i32
global first_thread_ssp, func, 0 i32
  align 8
# global: MAX_THREADS
MAX_THREADS
  dw 8
# global: STATE_READY
STATE_READY
  dw 0
# global: STATE_RUNNING
STATE_RUNNING
  dw 1
# global: STATE_BLOCKED
STATE_BLOCKED
  dw 2
# global: STATE_TERMINATED
STATE_TERMINATED
  dw 3
# global: tasks
tasks
  resb 192
# global: current_thread
current_thread
  dw 0
# global: thread_count
thread_count
  dw 0
# global: ticks
ticks
  dw 0
# function: create_thread
create_thread
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  stw r1, r2, r0
  addi r2, r5, 24
  ldd r1, r2, r0
  addi r7, r7, -8
  addi r2, r5, -16
  stw r1, r2, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -16
  stw r1, r2, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -16
  stw r1, r2, r0
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -16
  stw r1, r2, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -16
  stw r1, r2, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -16
  stw r1, r2, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -16
  stw r1, r2, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -16
  stw r1, r2, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -16
  stw r1, r2, r0
  movi r1, thread_exit
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -16
  stw r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  movi r1, thread_count
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -24
  stw r1, r2, r0
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 24
  mul r2, r2, r3
  add r1, r1, r2
  addi r7, r7, -8
  addi r2, r5, -32
  std r1, r2, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -32
  ldd r1, r2, r0
  popd r2
  stw r2, r1, r0
  movi r1, STATE_READY
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -32
  ldd r1, r2, r0
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -32
  ldd r1, r2, r0
  addi r1, r1, 8
  popd r2
  stw r2, r1, r0
  addi r2, r5, 32
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -32
  ldd r1, r2, r0
  addi r1, r1, 12
  popd r2
  std r2, r1, r0
  ldi r1, 1
  pshd r1
  movi r2, thread_count
  ldw r1, r2, r0
  popd r3
  add r1, r1, r3
  movi r2, thread_count
  stw r1, r2, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: sleep_current
sleep_current
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  stw r1, r2, r0
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 24
  mul r2, r2, r3
  add r1, r1, r2
  addi r7, r7, -8
  addi r2, r5, -16
  std r1, r2, r0
  movi r1, STATE_BLOCKED
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -16
  ldd r1, r2, r0
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  movi r1, ticks
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  mov r2, r1
  popd r1
  add r1, r1, r2
  pshd r1
  addi r2, r5, -16
  ldd r1, r2, r0
  addi r1, r1, 8
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: terminate_current
terminate_current
  pshd r6
  pshd r5
  mov r5, r7
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 24
  mul r2, r2, r3
  add r1, r1, r2
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  movi r1, STATE_TERMINATED
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: schedule
schedule
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  stw r1, r2, r0
  ldi r1, 1
  pshd r1
  movi r2, ticks
  ldw r1, r2, r0
  popd r3
  add r1, r1, r3
  movi r2, ticks
  stw r1, r2, r0
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  ldi r1, 0
  mov r2, r1
  popd r1
  slt r1, r1, r2
  ldi r3, 1
  xor r1, r1, r3
  beq r1, r0, .else_1
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 24
  mul r2, r2, r3
  add r1, r1, r2
  addi r7, r7, -8
  addi r2, r5, -16
  std r1, r2, r0
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -16
  ldd r1, r2, r0
  popd r2
  stw r2, r1, r0
  addi r2, r5, -16
  ldd r1, r2, r0
  addi r1, r1, 4
  ldw r1, r1, r0
  pshd r1
  movi r1, STATE_RUNNING
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_5
  ldi r1, 0
  bra .end_6
.eq_5
  ldi r1, 1
.end_6
  beq r1, r0, .else_3
  movi r1, STATE_READY
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -16
  ldd r1, r2, r0
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  bra .endif_4
.else_3
.endif_4
  bra .endif_2
.else_1
.endif_2
  ldi r1, 0
  addi r7, r7, -8
  addi r2, r5, -24
  stw r1, r2, r0
.while_7
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  movi r1, thread_count
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  slt r1, r1, r2
  beq r1, r0, .endwhile_8
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 24
  mul r2, r2, r3
  add r1, r1, r2
  addi r7, r7, -8
  addi r2, r5, -32
  std r1, r2, r0
  addi r2, r5, -32
  ldd r1, r2, r0
  addi r1, r1, 4
  ldw r1, r1, r0
  pshd r1
  movi r1, STATE_BLOCKED
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_11
  ldi r1, 0
  bra .end_12
.eq_11
  ldi r1, 1
.end_12
  beq r1, r0, .else_9
  movi r1, ticks
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -32
  ldd r1, r2, r0
  addi r1, r1, 8
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  slt r1, r1, r2
  ldi r3, 1
  xor r1, r1, r3
  beq r1, r0, .else_13
  movi r1, STATE_READY
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -32
  ldd r1, r2, r0
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  bra .endif_14
.else_13
.endif_14
  bra .endif_10
.else_9
.endif_10
  ldi r1, 1
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  popd r3
  add r1, r1, r3
  addi r2, r5, -24
  stw r1, r2, r0
  bra .while_7
.endwhile_8
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  ldi r1, 1
  mov r2, r1
  popd r1
  add r1, r1, r2
  addi r7, r7, -8
  addi r2, r5, -40
  stw r1, r2, r0
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  movi r1, thread_count
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  slt r1, r1, r2
  ldi r3, 1
  xor r1, r1, r3
  beq r1, r0, .else_15
  ldi r1, 0
  addi r2, r5, -40
  stw r1, r2, r0
  bra .endif_16
.else_15
.endif_16
  ldi r1, 0
  addi r7, r7, -8
  addi r2, r5, -48
  stw r1, r2, r0
.while_17
  addi r2, r5, -48
  ldw r1, r2, r0
  pshd r1
  movi r1, thread_count
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  slt r1, r1, r2
  beq r1, r0, .endwhile_18
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 24
  mul r2, r2, r3
  add r1, r1, r2
  addi r7, r7, -8
  addi r2, r5, -56
  std r1, r2, r0
  addi r2, r5, -56
  ldd r1, r2, r0
  addi r1, r1, 4
  ldw r1, r1, r0
  pshd r1
  movi r1, STATE_READY
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_21
  ldi r1, 0
  bra .end_22
.eq_21
  ldi r1, 1
.end_22
  beq r1, r0, .else_19
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  movi r1, current_thread
  popd r2
  stw r2, r1, r0
  movi r1, STATE_RUNNING
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -56
  ldd r1, r2, r0
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  addi r2, r5, -56
  ldd r1, r2, r0
  ldw r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
  bra .endif_20
.else_19
.endif_20
  ldi r1, 1
  pshd r1
  addi r2, r5, -40
  ldw r1, r2, r0
  popd r3
  add r1, r1, r3
  addi r2, r5, -40
  stw r1, r2, r0
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  movi r1, thread_count
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  slt r1, r1, r2
  ldi r3, 1
  xor r1, r1, r3
  beq r1, r0, .else_23
  ldi r1, 0
  addi r2, r5, -40
  stw r1, r2, r0
  bra .endif_24
.else_23
.endif_24
  ldi r1, 1
  pshd r1
  addi r2, r5, -48
  ldw r1, r2, r0
  popd r3
  add r1, r1, r3
  addi r2, r5, -48
  stw r1, r2, r0
  bra .while_17
.endwhile_18
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: first_thread_ssp
first_thread_ssp
  pshd r6
  pshd r5
  mov r5, r7
  ldi r1, 0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 24
  mul r2, r2, r3
  add r1, r1, r2
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, -8
  ldd r1, r2, r0
  ldw r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
