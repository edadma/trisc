entry main
global main, func, 0 i32
# function: main
main:
  pshd r6
  pshd r5
  mov r5, r7
  ldi r1, 0
  pshd r1
  addi r7, r7, -16
  mov r1, r7
  movi r2, __str_1
  std r2, r1, r0
  ldi r2, 4
  addi r3, r1, 8
  stw r2, r3, r0
  pshd r1
  ldi r1, 3
  pshd r1
  ldi r1, 2
  pshd r1
  ldi r1, 1
  pshd r1
  popd r1
  popd r2
  popd r3
  movi r4, do_thing
  jalr r6, r4
  addi r7, r7, 32
  ldi r1, 1
  pshd r1
  addi r7, r7, -16
  mov r1, r7
  movi r2, __str_2
  std r2, r1, r0
  ldi r2, 3
  addi r3, r1, 8
  stw r2, r3, r0
  pshd r1
  ldi r1, 6
  pshd r1
  ldi r1, 5
  pshd r1
  ldi r1, 4
  pshd r1
  popd r1
  popd r2
  popd r3
  movi r4, do_thing
  jalr r6, r4
  addi r7, r7, 32
  ldi r1, 2
  pshd r1
  addi r7, r7, -16
  mov r1, r7
  movi r2, __str_3
  std r2, r1, r0
  ldi r2, 3
  addi r3, r1, 8
  stw r2, r3, r0
  pshd r1
  ldi r1, 9
  pshd r1
  ldi r1, 8
  pshd r1
  ldi r1, 7
  pshd r1
  popd r1
  popd r2
  popd r3
  movi r4, do_thing
  jalr r6, r4
  addi r7, r7, 32
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
global __str_1, data, 5
global __str_2, data, 4
global __str_3, data, 4
  align 8
__str_1:
  db 104
  db 105
  db 103
  db 104
  db 0
__str_2:
  db 109
  db 101
  db 100
  db 0
__str_3:
  db 108
  db 111
  db 119
  db 0
