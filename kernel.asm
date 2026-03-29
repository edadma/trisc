global MAX_THREADS, data, i32
global NUM_PRIORITIES, data, i32
global DEFAULT_QUANTUM, data, i32
global STATE_READY, data, i32
global STATE_RUNNING, data, i32
global STATE_BLOCKED, data, i32
global STATE_TERMINATED, data, i32
global STATE_JOINING, data, i32
global QUEUE_EMPTY, data, i32
global queue_head, data, arr 4 i32
global queue_tail, data, arr 4 i32
global ready_mask, data, i32
global queues_initialized, data, i32
global tasks, data, arr 8 struct Task 8 ssp i32 state i32 wake_tick i32 name ptr i8 priority i32 join_target i32 quantum i32 next i32
global current_thread, data, i32
global thread_count, data, i32
global ticks, data, i32
global ensure_queues_init, func, 0 void
global enqueue, func, 2 i32 i32 void
global dequeue, func, 1 i32 i32
global init_queues, func, 0 void
global build_stack_frame, func, 3 i32 i32 i32 i32
global create_thread, func, 4 i32 i32 i32 ptr i8 void
global create_thread_pri, func, 5 i32 i32 i32 ptr i8 i32 void
global sleep_current, func, 1 i32 void
global sleep_until_current, func, 1 i32 void
global query_thread_state, func, 1 i32 i32
global query_thread_name, func, 1 i32 ptr i8
global terminate_current, func, 0 void
global join_current, func, 1 i32 void
global schedule, func, 1 i32 i32
global first_thread_ssp, func, 0 i32
  align 4
# global: MAX_THREADS
MAX_THREADS:
  dw 8
  align 4
# global: NUM_PRIORITIES
NUM_PRIORITIES:
  dw 4
  align 4
# global: DEFAULT_QUANTUM
DEFAULT_QUANTUM:
  dw 5
  align 4
# global: STATE_READY
STATE_READY:
  dw 0
  align 4
# global: STATE_RUNNING
STATE_RUNNING:
  dw 1
  align 4
# global: STATE_BLOCKED
STATE_BLOCKED:
  dw 2
  align 4
# global: STATE_TERMINATED
STATE_TERMINATED:
  dw 3
  align 4
# global: STATE_JOINING
STATE_JOINING:
  dw 4
  align 4
# global: QUEUE_EMPTY
QUEUE_EMPTY:
  dw -1
  align 4
# global: queue_head
queue_head:
  resb 16
  align 4
# global: queue_tail
queue_tail:
  resb 16
  align 4
# global: ready_mask
ready_mask:
  dw 0
  align 4
# global: queues_initialized
queues_initialized:
  dw 0
  align 8
# global: tasks
tasks:
  resb 320
  align 4
# global: current_thread
current_thread:
  dw -1
  align 4
# global: thread_count
thread_count:
  dw 0
  align 4
# global: ticks
ticks:
  dw 0
# function: ensure_queues_init
ensure_queues_init:
  pshd r6
  pshd r5
  mov r5, r7
  movi r1, queues_initialized
  ldw r1, r1, r0
  pshd r1
  ldi r1, 0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_3
  ldi r1, 0
  bra .end_4
.eq_3
  ldi r1, 1
.end_4
  beq r1, r0, .else_1
  movi r4, init_queues
  jalr r6, r4
  ldi r1, 1
  pshd r1
  movi r1, queues_initialized
  popd r2
  stw r2, r1, r0
  bra .endif_2
.else_1
.endif_2
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: enqueue
enqueue:
  pshd r1
  pshd r2
  pshd r6
  pshd r5
  mov r5, r7
  movi r1, QUEUE_EMPTY
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 36
  popd r2
  stw r2, r1, r0
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  movi r1, queue_head
  popd r2
  ldi r3, 4
  mul r2, r2, r3
  add r1, r1, r2
  ldw r1, r1, r0
  pshd r1
  movi r1, QUEUE_EMPTY
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_7
  ldi r1, 0
  bra .end_8
.eq_7
  ldi r1, 1
.end_8
  beq r1, r0, .else_5
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  movi r1, queue_head
  popd r2
  ldi r3, 4
  mul r2, r2, r3
  add r1, r1, r2
  popd r2
  stw r2, r1, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  movi r1, queue_tail
  popd r2
  ldi r3, 4
  mul r2, r2, r3
  add r1, r1, r2
  popd r2
  stw r2, r1, r0
  bra .endif_6
.else_5
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  movi r1, queue_tail
  popd r2
  ldi r3, 4
  mul r2, r2, r3
  add r1, r1, r2
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 36
  popd r2
  stw r2, r1, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  movi r1, queue_tail
  popd r2
  ldi r3, 4
  mul r2, r2, r3
  add r1, r1, r2
  popd r2
  stw r2, r1, r0
.endif_6
  movi r1, ready_mask
  ldw r1, r1, r0
  pshd r1
  ldi r1, 1
  pshd r1
  addi r2, r5, 24
  ldd r1, r2, r0
  mov r2, r1
  popd r1
  lsl r1, r1, r2
  mov r2, r1
  popd r1
  or r1, r1, r2
  pshd r1
  movi r1, ready_mask
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  addi r7, r7, 16
  jalr r0, r6
# function: dequeue
dequeue:
  pshd r1
  pshd r6
  pshd r5
  mov r5, r7
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  movi r1, queue_head
  popd r2
  ldi r3, 4
  mul r2, r2, r3
  add r1, r1, r2
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -8
  stw r1, r2, r0
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, QUEUE_EMPTY
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
  movi r1, QUEUE_EMPTY
  ldw r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  addi r7, r7, 8
  jalr r0, r6
  bra .endif_10
.else_9
.endif_10
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 36
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  movi r1, queue_head
  popd r2
  ldi r3, 4
  mul r2, r2, r3
  add r1, r1, r2
  popd r2
  stw r2, r1, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  movi r1, queue_head
  popd r2
  ldi r3, 4
  mul r2, r2, r3
  add r1, r1, r2
  ldw r1, r1, r0
  pshd r1
  movi r1, QUEUE_EMPTY
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_15
  ldi r1, 0
  bra .end_16
.eq_15
  ldi r1, 1
.end_16
  beq r1, r0, .else_13
  movi r1, QUEUE_EMPTY
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  movi r1, queue_tail
  popd r2
  ldi r3, 4
  mul r2, r2, r3
  add r1, r1, r2
  popd r2
  stw r2, r1, r0
  movi r1, ready_mask
  ldw r1, r1, r0
  pshd r1
  ldi r1, 1
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  mov r2, r1
  popd r1
  lsl r1, r1, r2
  not r1, r1
  mov r2, r1
  popd r1
  and r1, r1, r2
  pshd r1
  movi r1, ready_mask
  popd r2
  stw r2, r1, r0
  bra .endif_14
.else_13
.endif_14
  movi r1, QUEUE_EMPTY
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 36
  popd r2
  stw r2, r1, r0
  addi r2, r5, -8
  ldw r1, r2, r0
  mov r7, r5
  popd r5
  popd r6
  addi r7, r7, 8
  jalr r0, r6
# function: init_queues
init_queues:
  pshd r6
  pshd r5
  mov r5, r7
  ldi r1, 0
  addi r7, r7, -8
  addi r2, r5, -8
  stw r1, r2, r0
.while_17
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, NUM_PRIORITIES
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  slt r1, r1, r2
  beq r1, r0, .endwhile_18
  movi r1, QUEUE_EMPTY
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, queue_head
  popd r2
  ldi r3, 4
  mul r2, r2, r3
  add r1, r1, r2
  popd r2
  stw r2, r1, r0
  movi r1, QUEUE_EMPTY
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, queue_tail
  popd r2
  ldi r3, 4
  mul r2, r2, r3
  add r1, r1, r2
  popd r2
  stw r2, r1, r0
  ldi r1, 1
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r3
  add r1, r1, r3
  addi r2, r5, -8
  stw r1, r2, r0
  bra .while_17
.endwhile_18
  ldi r1, 0
  pshd r1
  movi r1, ready_mask
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: build_stack_frame
build_stack_frame:
  pshd r1
  pshd r2
  pshd r3
  pshd r6
  pshd r5
  mov r5, r7
  addi r2, r5, 16
  ldd r1, r2, r0
  addi r7, r7, -8
  addi r2, r5, -8
  stw r1, r2, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -8
  stw r1, r2, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -8
  stw r1, r2, r0
  addi r2, r5, 32
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -8
  stw r1, r2, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -8
  stw r1, r2, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -8
  stw r1, r2, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -8
  stw r1, r2, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -8
  stw r1, r2, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -8
  stw r1, r2, r0
  movi r1, thread_exit
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  ldi r1, 8
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r3
  sub r1, r1, r3
  addi r2, r5, -8
  stw r1, r2, r0
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  addi r2, r5, -8
  ldw r1, r2, r0
  mov r7, r5
  popd r5
  popd r6
  addi r7, r7, 24
  jalr r0, r6
# function: create_thread
create_thread:
  pshd r1
  pshd r2
  pshd r3
  pshd r6
  pshd r5
  mov r5, r7
  ldi r1, 0
  pshd r1
  addi r2, r5, 40
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, 32
  ldd r1, r2, r0
  pshd r1
  popd r1
  popd r2
  popd r3
  movi r4, create_thread_pri
  jalr r6, r4
  addi r7, r7, 16
  mov r7, r5
  popd r5
  popd r6
  addi r7, r7, 24
  jalr r0, r6
# function: create_thread_pri
create_thread_pri:
  pshd r1
  pshd r2
  pshd r3
  pshd r6
  pshd r5
  mov r5, r7
  movi r4, ensure_queues_init
  jalr r6, r4
  movi r1, thread_count
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -8
  stw r1, r2, r0
  addi r2, r5, 16
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
  addi r2, r5, 32
  ldd r1, r2, r0
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
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r2
  std r2, r1, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  popd r2
  stw r2, r1, r0
  movi r1, STATE_READY
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 8
  popd r2
  stw r2, r1, r0
  addi r2, r5, 40
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 16
  popd r2
  std r2, r1, r0
  addi r2, r5, 48
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  ldi r1, 1
  neg r1, r1
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 28
  popd r2
  stw r2, r1, r0
  movi r1, DEFAULT_QUANTUM
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 32
  popd r2
  stw r2, r1, r0
  movi r1, QUEUE_EMPTY
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 36
  popd r2
  stw r2, r1, r0
  ldi r1, 1
  pshd r1
  movi r2, thread_count
  ldw r1, r2, r0
  popd r3
  add r1, r1, r3
  movi r2, thread_count
  stw r1, r2, r0
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, 48
  ldd r1, r2, r0
  pshd r1
  popd r1
  popd r2
  movi r4, enqueue
  jalr r6, r4
  mov r7, r5
  popd r5
  popd r6
  addi r7, r7, 24
  jalr r0, r6
# function: sleep_current
sleep_current:
  pshd r1
  pshd r6
  pshd r5
  mov r5, r7
  movi r1, STATE_BLOCKED
  ldw r1, r1, r0
  pshd r1
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  movi r1, ticks
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  mov r2, r1
  popd r1
  add r1, r1, r2
  pshd r1
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 8
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  addi r7, r7, 8
  jalr r0, r6
# function: sleep_until_current
sleep_until_current:
  pshd r1
  pshd r6
  pshd r5
  mov r5, r7
  movi r1, STATE_BLOCKED
  ldw r1, r1, r0
  pshd r1
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 8
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  addi r7, r7, 8
  jalr r0, r6
# function: query_thread_state
query_thread_state:
  pshd r1
  pshd r6
  pshd r5
  mov r5, r7
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  ldw r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  addi r7, r7, 8
  jalr r0, r6
# function: query_thread_name
query_thread_name:
  pshd r1
  pshd r6
  pshd r5
  mov r5, r7
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 16
  ldd r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  addi r7, r7, 8
  jalr r0, r6
# function: terminate_current
terminate_current:
  pshd r6
  pshd r5
  mov r5, r7
  movi r1, STATE_TERMINATED
  ldw r1, r1, r0
  pshd r1
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  addi r7, r7, -8
  addi r2, r5, -8
  stw r1, r2, r0
.while_19
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, thread_count
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  slt r1, r1, r2
  beq r1, r0, .endwhile_20
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  ldw r1, r1, r0
  pshd r1
  movi r1, STATE_JOINING
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_23
  ldi r1, 0
  bra .end_24
.eq_23
  ldi r1, 1
.end_24
  beq r1, r0, .else_21
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 28
  ldw r1, r1, r0
  pshd r1
  movi r1, current_thread
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_27
  ldi r1, 0
  bra .end_28
.eq_27
  ldi r1, 1
.end_28
  beq r1, r0, .else_25
  movi r1, STATE_READY
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  movi r1, DEFAULT_QUANTUM
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 32
  popd r2
  stw r2, r1, r0
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 24
  ldw r1, r1, r0
  pshd r1
  popd r1
  popd r2
  movi r4, enqueue
  jalr r6, r4
  bra .endif_26
.else_25
.endif_26
  bra .endif_22
.else_21
.endif_22
  ldi r1, 1
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r3
  add r1, r1, r3
  addi r2, r5, -8
  stw r1, r2, r0
  bra .while_19
.endwhile_20
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: join_current
join_current:
  pshd r1
  pshd r6
  pshd r5
  mov r5, r7
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  ldw r1, r1, r0
  pshd r1
  movi r1, STATE_TERMINATED
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_31
  ldi r1, 0
  bra .end_32
.eq_31
  ldi r1, 1
.end_32
  beq r1, r0, .else_29
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  addi r7, r7, 8
  jalr r0, r6
  bra .endif_30
.else_29
.endif_30
  movi r1, STATE_JOINING
  ldw r1, r1, r0
  pshd r1
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 28
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  addi r7, r7, 8
  jalr r0, r6
# function: schedule
schedule:
  pshd r1
  pshd r6
  pshd r5
  mov r5, r7
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
  beq r1, r0, .else_33
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  popd r2
  stw r2, r1, r0
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  ldw r1, r1, r0
  pshd r1
  movi r1, STATE_RUNNING
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_37
  ldi r1, 0
  bra .end_38
.eq_37
  ldi r1, 1
.end_38
  beq r1, r0, .else_35
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 32
  pshd r1
  ldw r1, r1, r0
  pshd r1
  ldi r1, 1
  popd r2
  sub r2, r2, r1
  popd r1
  stw r2, r1, r0
  movi r1, STATE_READY
  ldw r1, r1, r0
  pshd r1
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 32
  ldw r1, r1, r0
  pshd r1
  ldi r1, 0
  mov r2, r1
  popd r1
  slt r1, r2, r1
  ldi r3, 1
  xor r1, r1, r3
  beq r1, r0, .else_39
  movi r1, DEFAULT_QUANTUM
  ldw r1, r1, r0
  pshd r1
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 32
  popd r2
  stw r2, r1, r0
  bra .endif_40
.else_39
.endif_40
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, current_thread
  ldw r1, r1, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 24
  ldw r1, r1, r0
  pshd r1
  popd r1
  popd r2
  movi r4, enqueue
  jalr r6, r4
  bra .endif_36
.else_35
.endif_36
  bra .endif_34
.else_33
.endif_34
  ldi r1, 0
  addi r7, r7, -8
  addi r2, r5, -8
  stw r1, r2, r0
.while_41
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, thread_count
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  slt r1, r1, r2
  beq r1, r0, .endwhile_42
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  ldw r1, r1, r0
  pshd r1
  movi r1, STATE_BLOCKED
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_45
  ldi r1, 0
  bra .end_46
.eq_45
  ldi r1, 1
.end_46
  beq r1, r0, .else_43
  movi r1, ticks
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 8
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  slt r1, r1, r2
  ldi r3, 1
  xor r1, r1, r3
  beq r1, r0, .else_47
  movi r1, STATE_READY
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  movi r1, DEFAULT_QUANTUM
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 32
  popd r2
  stw r2, r1, r0
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 24
  ldw r1, r1, r0
  pshd r1
  popd r1
  popd r2
  movi r4, enqueue
  jalr r6, r4
  bra .endif_48
.else_47
.endif_48
  bra .endif_44
.else_43
.endif_44
  ldi r1, 1
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r3
  add r1, r1, r3
  addi r2, r5, -8
  stw r1, r2, r0
  bra .while_41
.endwhile_42
  ldi r1, 0
  addi r7, r7, -8
  addi r2, r5, -16
  stw r1, r2, r0
.while_49
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  movi r1, NUM_PRIORITIES
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  slt r1, r1, r2
  beq r1, r0, .endwhile_50
  movi r1, ready_mask
  ldw r1, r1, r0
  pshd r1
  ldi r1, 1
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  mov r2, r1
  popd r1
  lsl r1, r1, r2
  mov r2, r1
  popd r1
  and r1, r1, r2
  pshd r1
  ldi r1, 0
  mov r2, r1
  popd r1
  beq r1, r2, .ne_53
  ldi r1, 1
  bra .end_54
.ne_53
  ldi r1, 0
.end_54
  beq r1, r0, .else_51
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  popd r1
  movi r4, dequeue
  jalr r6, r4
  addi r7, r7, -8
  addi r2, r5, -24
  stw r1, r2, r0
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  movi r1, QUEUE_EMPTY
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .ne_57
  ldi r1, 1
  bra .end_58
.ne_57
  ldi r1, 0
.end_58
  beq r1, r0, .else_55
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  movi r1, current_thread
  popd r2
  stw r2, r1, r0
  movi r1, STATE_RUNNING
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  ldw r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  addi r7, r7, 8
  jalr r0, r6
  bra .endif_56
.else_55
.endif_56
  bra .endif_52
.else_51
.endif_52
  ldi r1, 1
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r3
  add r1, r1, r3
  addi r2, r5, -16
  stw r1, r2, r0
  bra .while_49
.endwhile_50
  ldi r1, 1
  neg r1, r1
  pshd r1
  movi r1, current_thread
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  addi r7, r7, 8
  jalr r0, r6
# function: first_thread_ssp
first_thread_ssp:
  pshd r6
  pshd r5
  mov r5, r7
  ldi r1, 0
  addi r7, r7, -8
  addi r2, r5, -8
  stw r1, r2, r0
.while_59
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  movi r1, NUM_PRIORITIES
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  slt r1, r1, r2
  beq r1, r0, .endwhile_60
  movi r1, ready_mask
  ldw r1, r1, r0
  pshd r1
  ldi r1, 1
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  mov r2, r1
  popd r1
  lsl r1, r1, r2
  mov r2, r1
  popd r1
  and r1, r1, r2
  pshd r1
  ldi r1, 0
  mov r2, r1
  popd r1
  beq r1, r2, .ne_63
  ldi r1, 1
  bra .end_64
.ne_63
  ldi r1, 0
.end_64
  beq r1, r0, .else_61
  addi r2, r5, -8
  ldw r1, r2, r0
  pshd r1
  popd r1
  movi r4, dequeue
  jalr r6, r4
  addi r7, r7, -8
  addi r2, r5, -16
  stw r1, r2, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  movi r1, QUEUE_EMPTY
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .ne_67
  ldi r1, 1
  bra .end_68
.ne_67
  ldi r1, 0
.end_68
  beq r1, r0, .else_65
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  movi r1, current_thread
  popd r2
  stw r2, r1, r0
  movi r1, STATE_RUNNING
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  movi r1, tasks
  popd r2
  ldi r3, 40
  mul r2, r2, r3
  add r1, r1, r2
  ldw r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
  bra .endif_66
.else_65
.endif_66
  bra .endif_62
.else_61
.endif_62
  ldi r1, 1
  pshd r1
  addi r2, r5, -8
  ldw r1, r2, r0
  popd r3
  add r1, r1, r3
  addi r2, r5, -8
  stw r1, r2, r0
  bra .while_59
.endwhile_60
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
