entry main
global main, func, 0 i32
# function: main
main:
  pshd r6
  pshd r5
  mov r5, r7
  ldi r1, 10
  pshd r1
  ldi r1, 4
  pshd r1
  ldi r1, 3
  pshd r1
  ldi r1, 2
  pshd r1
  ldi r1, 1
  movi r4, do_thing
  jalr r6, r4
  addi r7, r7, 32
  ldi r1, 20
  pshd r1
  ldi r1, 8
  pshd r1
  ldi r1, 7
  pshd r1
  ldi r1, 6
  pshd r1
  ldi r1, 5
  movi r4, do_thing
  jalr r6, r4
  addi r7, r7, 32
  ldi r1, 30
  pshd r1
  ldi r1, 12
  pshd r1
  ldi r1, 11
  pshd r1
  ldi r1, 10
  pshd r1
  ldi r1, 9
  movi r4, do_thing
  jalr r6, r4
  addi r7, r7, 32
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
