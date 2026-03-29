global RB_BLACK, data, i32
global RB_RED, data, i32
global NIL, data, i32
global tree_init, func, 3 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 i32 void
global alloc_node, func, 1 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32
global free_node, func, 2 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32 void
global node_color, func, 2 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32 i32
global set_color, func, 3 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32 i32 void
global rotate_left, func, 2 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32 void
global rotate_right, func, 2 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32 void
global insert_fixup, func, 2 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32 void
global rb_insert, func, 3 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i64 i32 i32
global rb_find_min, func, 1 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32
global transplant, func, 3 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32 i32 void
global remove_fixup, func, 3 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32 i32 void
global rb_remove, func, 2 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32 void
global rb_node_key, func, 2 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32 i64
global rb_node_value, func, 2 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32 i32
global rb_count, func, 1 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32
global rb_empty, func, 1 ptr struct Tree 4 root i32 count i32 pool ptr struct RBNode 7 key i64 value i32 left i32 right i32 parent i32 color i32 next_free i32 free_head i32 i32
  align 4
# global: RB_BLACK
RB_BLACK:
  dw 0
  align 4
# global: RB_RED
RB_RED:
  dw 1
  align 4
# global: NIL
NIL:
  dw 0
# function: tree_init
tree_init:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  movi r1, NIL
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 4
  popd r2
  stw r2, r1, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  popd r2
  std r2, r1, r0
  ldi r1, 0
  addi r7, r7, -8
  addi r2, r5, -16
  stw r1, r2, r0
.while_1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  ldi r1, 1
  mov r2, r1
  popd r1
  sub r1, r1, r2
  mov r2, r1
  popd r1
  slt r1, r1, r2
  beq r1, r0, .endwhile_2
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  ldi r1, 1
  mov r2, r1
  popd r1
  add r1, r1, r2
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 28
  popd r2
  stw r2, r1, r0
  ldi r1, 1
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  popd r3
  add r1, r1, r3
  addi r2, r5, -16
  stw r1, r2, r0
  bra .while_1
.endwhile_2
  movi r1, NIL
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  ldi r1, 1
  mov r2, r1
  popd r1
  sub r1, r1, r2
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 28
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 16
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: alloc_node
alloc_node:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -16
  stw r1, r2, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  movi r1, NIL
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
  movi r1, NIL
  ldw r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
  bra .endif_4
.else_3
.endif_4
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 28
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 16
  popd r2
  stw r2, r1, r0
  movi r1, NIL
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  popd r2
  stw r2, r1, r0
  movi r1, NIL
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  popd r2
  stw r2, r1, r0
  movi r1, NIL
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  popd r2
  stw r2, r1, r0
  movi r1, RB_RED
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  movi r1, NIL
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 28
  popd r2
  stw r2, r1, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: free_node
free_node:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 28
  popd r2
  stw r2, r1, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 16
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: node_color
node_color:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_9
  ldi r1, 0
  bra .end_10
.eq_9
  ldi r1, 1
.end_10
  beq r1, r0, .else_7
  movi r1, RB_BLACK
  ldw r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
  bra .endif_8
.else_7
.endif_8
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  ldw r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: set_color
set_color:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .ne_13
  ldi r1, 1
  bra .end_14
.ne_13
  ldi r1, 0
.end_14
  beq r1, r0, .else_11
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  bra .endif_12
.else_11
.endif_12
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: rotate_left
rotate_left:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -16
  stw r1, r2, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  popd r2
  stw r2, r1, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .ne_17
  ldi r1, 1
  bra .end_18
.ne_17
  ldi r1, 0
.end_18
  beq r1, r0, .else_15
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  popd r2
  stw r2, r1, r0
  bra .endif_16
.else_15
.endif_16
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  popd r2
  stw r2, r1, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  pshd r1
  movi r1, NIL
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
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  popd r2
  stw r2, r1, r0
  bra .endif_20
.else_19
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_25
  ldi r1, 0
  bra .end_26
.eq_25
  ldi r1, 1
.end_26
  beq r1, r0, .else_23
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  popd r2
  stw r2, r1, r0
  bra .endif_24
.else_23
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  popd r2
  stw r2, r1, r0
.endif_24
.endif_20
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  popd r2
  stw r2, r1, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: rotate_right
rotate_right:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -16
  stw r1, r2, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  popd r2
  stw r2, r1, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .ne_29
  ldi r1, 1
  bra .end_30
.ne_29
  ldi r1, 0
.end_30
  beq r1, r0, .else_27
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  popd r2
  stw r2, r1, r0
  bra .endif_28
.else_27
.endif_28
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  popd r2
  stw r2, r1, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_33
  ldi r1, 0
  bra .end_34
.eq_33
  ldi r1, 1
.end_34
  beq r1, r0, .else_31
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  popd r2
  stw r2, r1, r0
  bra .endif_32
.else_31
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
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
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  popd r2
  stw r2, r1, r0
  bra .endif_36
.else_35
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  popd r2
  stw r2, r1, r0
.endif_36
.endif_32
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  popd r2
  stw r2, r1, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: insert_fixup
insert_fixup:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  addi r7, r7, -8
  addi r2, r5, -16
  stw r1, r2, r0
.while_39
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .ne_41
  ldi r1, 1
  bra .end_42
.ne_41
  ldi r1, 0
.end_42
  beq r1, r0, .endwhile_40
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -24
  stw r1, r2, r0
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  ldw r1, r1, r0
  pshd r1
  movi r1, RB_RED
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .ne_45
  ldi r1, 1
  bra .end_46
.ne_45
  ldi r1, 0
.end_46
  beq r1, r0, .else_43
  bra .endwhile_40
  bra .endif_44
.else_43
.endif_44
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -32
  stw r1, r2, r0
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_49
  ldi r1, 0
  bra .end_50
.eq_49
  ldi r1, 1
.end_50
  beq r1, r0, .else_47
  bra .endwhile_40
  bra .endif_48
.else_47
.endif_48
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_53
  ldi r1, 0
  bra .end_54
.eq_53
  ldi r1, 1
.end_54
  beq r1, r0, .else_51
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -40
  stw r1, r2, r0
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, node_color
  jalr r6, r4
  addi r7, r7, 8
  pshd r1
  movi r1, RB_RED
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_57
  ldi r1, 0
  bra .end_58
.eq_57
  ldi r1, 1
.end_58
  beq r1, r0, .else_55
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, set_color
  jalr r6, r4
  addi r7, r7, 16
  movi r1, RB_RED
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  addi r2, r5, -32
  ldw r1, r2, r0
  addi r2, r5, -16
  stw r1, r2, r0
  bra .endif_56
.else_55
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_61
  ldi r1, 0
  bra .end_62
.eq_61
  ldi r1, 1
.end_62
  beq r1, r0, .else_59
  addi r2, r5, -24
  ldw r1, r2, r0
  addi r2, r5, -16
  stw r1, r2, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, rotate_left
  jalr r6, r4
  addi r7, r7, 8
  bra .endif_60
.else_59
.endif_60
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -48
  stw r1, r2, r0
  addi r2, r5, -48
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -56
  stw r1, r2, r0
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -48
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  movi r1, RB_RED
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -56
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  addi r2, r5, -56
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, rotate_right
  jalr r6, r4
  addi r7, r7, 8
.endif_56
  bra .endif_52
.else_51
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -64
  stw r1, r2, r0
  addi r2, r5, -64
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, node_color
  jalr r6, r4
  addi r7, r7, 8
  pshd r1
  movi r1, RB_RED
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_65
  ldi r1, 0
  bra .end_66
.eq_65
  ldi r1, 1
.end_66
  beq r1, r0, .else_63
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -64
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, set_color
  jalr r6, r4
  addi r7, r7, 16
  movi r1, RB_RED
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  addi r2, r5, -32
  ldw r1, r2, r0
  addi r2, r5, -16
  stw r1, r2, r0
  bra .endif_64
.else_63
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_69
  ldi r1, 0
  bra .end_70
.eq_69
  ldi r1, 1
.end_70
  beq r1, r0, .else_67
  addi r2, r5, -24
  ldw r1, r2, r0
  addi r2, r5, -16
  stw r1, r2, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, rotate_right
  jalr r6, r4
  addi r7, r7, 8
  bra .endif_68
.else_67
.endif_68
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  addi r7, r7, -8
  movi r3, 72
  sub r2, r5, r3
  stw r1, r2, r0
  movi r3, 72
  sub r2, r5, r3
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  addi r7, r7, -8
  movi r3, 80
  sub r2, r5, r3
  stw r1, r2, r0
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  movi r3, 72
  sub r2, r5, r3
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  movi r1, RB_RED
  ldw r1, r1, r0
  pshd r1
  movi r3, 80
  sub r2, r5, r3
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  movi r3, 80
  sub r2, r5, r3
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, rotate_left
  jalr r6, r4
  addi r7, r7, 8
.endif_64
.endif_52
  bra .while_39
.endwhile_40
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: rb_insert
rb_insert:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, alloc_node
  jalr r6, r4
  addi r7, r7, -8
  addi r2, r5, -16
  stw r1, r2, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_73
  ldi r1, 0
  bra .end_74
.eq_73
  ldi r1, 1
.end_74
  beq r1, r0, .else_71
  movi r1, NIL
  ldw r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
  bra .endif_72
.else_71
.endif_72
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  popd r2
  std r2, r1, r0
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 8
  popd r2
  stw r2, r1, r0
  movi r1, NIL
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -24
  stw r1, r2, r0
  addi r2, r5, -8
  ldd r1, r2, r0
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -32
  stw r1, r2, r0
.while_75
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .ne_77
  ldi r1, 1
  bra .end_78
.ne_77
  ldi r1, 0
.end_78
  beq r1, r0, .endwhile_76
  addi r2, r5, -32
  ldw r1, r2, r0
  addi r2, r5, -24
  stw r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  ldd r1, r1, r0
  mov r2, r1
  popd r1
  slt r1, r1, r2
  beq r1, r0, .else_79
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  addi r2, r5, -32
  stw r1, r2, r0
  bra .endif_80
.else_79
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  addi r2, r5, -32
  stw r1, r2, r0
.endif_80
  bra .while_75
.endwhile_76
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  popd r2
  stw r2, r1, r0
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_83
  ldi r1, 0
  bra .end_84
.eq_83
  ldi r1, 1
.end_84
  beq r1, r0, .else_81
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  popd r2
  stw r2, r1, r0
  bra .endif_82
.else_81
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  ldd r1, r1, r0
  mov r2, r1
  popd r1
  slt r1, r1, r2
  beq r1, r0, .else_85
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  popd r2
  stw r2, r1, r0
  bra .endif_86
.else_85
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  popd r2
  stw r2, r1, r0
.endif_86
.endif_82
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 4
  pshd r1
  ldw r1, r1, r0
  pshd r1
  ldi r1, 1
  popd r2
  add r2, r2, r1
  popd r1
  stw r2, r1, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, insert_fixup
  jalr r6, r4
  addi r7, r7, 8
  addi r2, r5, -16
  ldw r1, r2, r0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: rb_find_min
rb_find_min:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, -8
  ldd r1, r2, r0
  ldw r1, r1, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_89
  ldi r1, 0
  bra .end_90
.eq_89
  ldi r1, 1
.end_90
  beq r1, r0, .else_87
  movi r1, NIL
  ldw r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
  bra .endif_88
.else_87
.endif_88
  addi r2, r5, -8
  ldd r1, r2, r0
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -16
  stw r1, r2, r0
.while_91
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .ne_93
  ldi r1, 1
  bra .end_94
.ne_93
  ldi r1, 0
.end_94
  beq r1, r0, .endwhile_92
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  addi r2, r5, -16
  stw r1, r2, r0
  bra .while_91
.endwhile_92
  addi r2, r5, -16
  ldw r1, r2, r0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: transplant
transplant:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_97
  ldi r1, 0
  bra .end_98
.eq_97
  ldi r1, 1
.end_98
  beq r1, r0, .else_95
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  popd r2
  stw r2, r1, r0
  bra .endif_96
.else_95
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -16
  stw r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_101
  ldi r1, 0
  bra .end_102
.eq_101
  ldi r1, 1
.end_102
  beq r1, r0, .else_99
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  popd r2
  stw r2, r1, r0
  bra .endif_100
.else_99
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  popd r2
  stw r2, r1, r0
.endif_100
.endif_96
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .ne_105
  ldi r1, 1
  bra .end_106
.ne_105
  ldi r1, 0
.end_106
  beq r1, r0, .else_103
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, 24
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  popd r2
  stw r2, r1, r0
  bra .endif_104
.else_103
.endif_104
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: remove_fixup
remove_fixup:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  addi r7, r7, -8
  addi r2, r5, -16
  stw r1, r2, r0
  addi r2, r5, 24
  ldd r1, r2, r0
  addi r7, r7, -8
  addi r2, r5, -24
  stw r1, r2, r0
.while_107
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .ne_109
  ldi r1, 1
  bra .end_110
.ne_109
  ldi r1, 0
.end_110
  beq r1, r0, .endwhile_108
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, node_color
  jalr r6, r4
  addi r7, r7, 8
  pshd r1
  movi r1, RB_RED
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_113
  ldi r1, 0
  bra .end_114
.eq_113
  ldi r1, 1
.end_114
  beq r1, r0, .else_111
  bra .endwhile_108
  bra .endif_112
.else_111
.endif_112
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_117
  ldi r1, 0
  bra .end_118
.eq_117
  ldi r1, 1
.end_118
  beq r1, r0, .else_115
  bra .endwhile_108
  bra .endif_116
.else_115
.endif_116
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_121
  ldi r1, 0
  bra .end_122
.eq_121
  ldi r1, 1
.end_122
  beq r1, r0, .else_119
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -32
  stw r1, r2, r0
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_125
  ldi r1, 0
  bra .end_126
.eq_125
  ldi r1, 1
.end_126
  beq r1, r0, .else_123
  bra .endwhile_108
  bra .endif_124
.else_123
.endif_124
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  ldw r1, r1, r0
  pshd r1
  movi r1, RB_RED
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_129
  ldi r1, 0
  bra .end_130
.eq_129
  ldi r1, 1
.end_130
  beq r1, r0, .else_127
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  movi r1, RB_RED
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, rotate_left
  jalr r6, r4
  addi r7, r7, 8
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  addi r2, r5, -32
  stw r1, r2, r0
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_133
  ldi r1, 0
  bra .end_134
.eq_133
  ldi r1, 1
.end_134
  beq r1, r0, .else_131
  bra .endwhile_108
  bra .endif_132
.else_131
.endif_132
  bra .endif_128
.else_127
.endif_128
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, node_color
  jalr r6, r4
  addi r7, r7, 8
  pshd r1
  movi r1, RB_BLACK
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_137
  ldi r1, 0
  bra .end_138
.eq_137
  ldi r1, 1
.end_138
  beq r1, r0, .else_135
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, node_color
  jalr r6, r4
  addi r7, r7, 8
  pshd r1
  movi r1, RB_BLACK
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_141
  ldi r1, 0
  bra .end_142
.eq_141
  ldi r1, 1
.end_142
  beq r1, r0, .else_139
  movi r1, RB_RED
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  addi r2, r5, -24
  ldw r1, r2, r0
  addi r2, r5, -16
  stw r1, r2, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  addi r2, r5, -24
  stw r1, r2, r0
  bra .while_107
  bra .endif_140
.else_139
.endif_140
  bra .endif_136
.else_135
.endif_136
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, node_color
  jalr r6, r4
  addi r7, r7, 8
  pshd r1
  movi r1, RB_BLACK
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_145
  ldi r1, 0
  bra .end_146
.eq_145
  ldi r1, 1
.end_146
  beq r1, r0, .else_143
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, set_color
  jalr r6, r4
  addi r7, r7, 16
  movi r1, RB_RED
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, rotate_right
  jalr r6, r4
  addi r7, r7, 8
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  addi r2, r5, -32
  stw r1, r2, r0
  bra .endif_144
.else_143
.endif_144
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, set_color
  jalr r6, r4
  addi r7, r7, 16
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, rotate_left
  jalr r6, r4
  addi r7, r7, 8
  addi r2, r5, -8
  ldd r1, r2, r0
  ldw r1, r1, r0
  addi r2, r5, -16
  stw r1, r2, r0
  bra .endwhile_108
  bra .endif_120
.else_119
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -40
  stw r1, r2, r0
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_149
  ldi r1, 0
  bra .end_150
.eq_149
  ldi r1, 1
.end_150
  beq r1, r0, .else_147
  bra .endwhile_108
  bra .endif_148
.else_147
.endif_148
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  ldw r1, r1, r0
  pshd r1
  movi r1, RB_RED
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_153
  ldi r1, 0
  bra .end_154
.eq_153
  ldi r1, 1
.end_154
  beq r1, r0, .else_151
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  movi r1, RB_RED
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, rotate_right
  jalr r6, r4
  addi r7, r7, 8
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  addi r2, r5, -40
  stw r1, r2, r0
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_157
  ldi r1, 0
  bra .end_158
.eq_157
  ldi r1, 1
.end_158
  beq r1, r0, .else_155
  bra .endwhile_108
  bra .endif_156
.else_155
.endif_156
  bra .endif_152
.else_151
.endif_152
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, node_color
  jalr r6, r4
  addi r7, r7, 8
  pshd r1
  movi r1, RB_BLACK
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_161
  ldi r1, 0
  bra .end_162
.eq_161
  ldi r1, 1
.end_162
  beq r1, r0, .else_159
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, node_color
  jalr r6, r4
  addi r7, r7, 8
  pshd r1
  movi r1, RB_BLACK
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_165
  ldi r1, 0
  bra .end_166
.eq_165
  ldi r1, 1
.end_166
  beq r1, r0, .else_163
  movi r1, RB_RED
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  addi r2, r5, -24
  ldw r1, r2, r0
  addi r2, r5, -16
  stw r1, r2, r0
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  addi r2, r5, -24
  stw r1, r2, r0
  bra .while_107
  bra .endif_164
.else_163
.endif_164
  bra .endif_160
.else_159
.endif_160
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, node_color
  jalr r6, r4
  addi r7, r7, 8
  pshd r1
  movi r1, RB_BLACK
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_169
  ldi r1, 0
  bra .end_170
.eq_169
  ldi r1, 1
.end_170
  beq r1, r0, .else_167
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, set_color
  jalr r6, r4
  addi r7, r7, 16
  movi r1, RB_RED
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, rotate_left
  jalr r6, r4
  addi r7, r7, 8
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  addi r2, r5, -40
  stw r1, r2, r0
  bra .endif_168
.else_167
.endif_168
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, set_color
  jalr r6, r4
  addi r7, r7, 16
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, rotate_right
  jalr r6, r4
  addi r7, r7, 8
  addi r2, r5, -8
  ldd r1, r2, r0
  ldw r1, r1, r0
  addi r2, r5, -16
  stw r1, r2, r0
  bra .endwhile_108
.endif_120
  bra .while_107
.endwhile_108
  movi r1, RB_BLACK
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, set_color
  jalr r6, r4
  addi r7, r7, 16
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: rb_remove
rb_remove:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -16
  stw r1, r2, r0
  movi r1, NIL
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -24
  stw r1, r2, r0
  movi r1, NIL
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -32
  stw r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_173
  ldi r1, 0
  bra .end_174
.eq_173
  ldi r1, 1
.end_174
  beq r1, r0, .else_171
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  addi r2, r5, -24
  stw r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  addi r2, r5, -32
  stw r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, transplant
  jalr r6, r4
  addi r7, r7, 16
  bra .endif_172
.else_171
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_177
  ldi r1, 0
  bra .end_178
.eq_177
  ldi r1, 1
.end_178
  beq r1, r0, .else_175
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  addi r2, r5, -24
  stw r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  addi r2, r5, -32
  stw r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, transplant
  jalr r6, r4
  addi r7, r7, 16
  bra .endif_176
.else_175
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -40
  stw r1, r2, r0
.while_179
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .ne_181
  ldi r1, 1
  bra .end_182
.ne_181
  ldi r1, 0
.end_182
  beq r1, r0, .endwhile_180
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  addi r2, r5, -40
  stw r1, r2, r0
  bra .while_179
.endwhile_180
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  ldw r1, r1, r0
  addi r7, r7, -8
  addi r2, r5, -48
  stw r1, r2, r0
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  addi r2, r5, -24
  stw r1, r2, r0
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_185
  ldi r1, 0
  bra .end_186
.eq_185
  ldi r1, 1
.end_186
  beq r1, r0, .else_183
  addi r2, r5, -40
  ldw r1, r2, r0
  addi r2, r5, -32
  stw r1, r2, r0
  bra .endif_184
.else_183
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  ldw r1, r1, r0
  addi r2, r5, -32
  stw r1, r2, r0
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, transplant
  jalr r6, r4
  addi r7, r7, 16
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  popd r2
  stw r2, r1, r0
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 16
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  popd r2
  stw r2, r1, r0
.endif_184
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, transplant
  jalr r6, r4
  addi r7, r7, 16
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  popd r2
  stw r2, r1, r0
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 12
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 20
  popd r2
  stw r2, r1, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  ldw r1, r1, r0
  pshd r1
  addi r2, r5, -40
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 24
  popd r2
  stw r2, r1, r0
  addi r2, r5, -48
  ldw r1, r2, r0
  pshd r1
  movi r1, RB_BLACK
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_189
  ldi r1, 0
  bra .end_190
.eq_189
  ldi r1, 1
.end_190
  beq r1, r0, .else_187
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, remove_fixup
  jalr r6, r4
  addi r7, r7, 16
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, free_node
  jalr r6, r4
  addi r7, r7, 8
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 4
  pshd r1
  ldw r1, r1, r0
  pshd r1
  ldi r1, 1
  popd r2
  sub r2, r2, r1
  popd r1
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
  bra .endif_188
.else_187
.endif_188
.endif_176
.endif_172
  addi r2, r5, -16
  ldw r1, r2, r0
  pshd r1
  movi r1, RB_BLACK
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_193
  ldi r1, 0
  bra .end_194
.eq_193
  ldi r1, 1
.end_194
  beq r1, r0, .else_191
  addi r2, r5, -32
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -24
  ldw r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, remove_fixup
  jalr r6, r4
  addi r7, r7, 16
  bra .endif_192
.else_191
.endif_192
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  movi r4, free_node
  jalr r6, r4
  addi r7, r7, 8
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 4
  pshd r1
  ldw r1, r1, r0
  pshd r1
  ldi r1, 1
  popd r2
  sub r2, r2, r1
  popd r1
  stw r2, r1, r0
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: rb_node_key
rb_node_key:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  ldd r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: rb_node_value
rb_node_value:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, 16
  ldd r1, r2, r0
  pshd r1
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 8
  ldd r1, r1, r0
  popd r2
  ldi r3, 32
  mul r2, r2, r3
  add r1, r1, r2
  ldd r1, r1, r0
  addi r1, r1, 8
  ldw r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: rb_count
rb_count:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, -8
  ldd r1, r2, r0
  addi r1, r1, 4
  ldw r1, r1, r0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
# function: rb_empty
rb_empty:
  pshd r6
  pshd r5
  mov r5, r7
  addi r7, r7, -8
  addi r2, r5, -8
  std r1, r2, r0
  addi r2, r5, -8
  ldd r1, r2, r0
  ldw r1, r1, r0
  pshd r1
  movi r1, NIL
  ldw r1, r1, r0
  mov r2, r1
  popd r1
  beq r1, r2, .eq_197
  ldi r1, 0
  bra .end_198
.eq_197
  ldi r1, 1
.end_198
  beq r1, r0, .else_195
  ldi r1, 1
  bra .endif_196
.else_195
  ldi r1, 0
.endif_196
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
