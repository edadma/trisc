entry main
global main, func, 0 i32
# function: main
main
  pshd r6
  pshd r5
  mov r5, r7
  ldi r1, 72
  movi r4, putchar
  jalr r6, r4
  ldi r1, 101
  movi r4, putchar
  jalr r6, r4
  ldi r1, 108
  movi r4, putchar
  jalr r6, r4
  ldi r1, 108
  movi r4, putchar
  jalr r6, r4
  ldi r1, 111
  movi r4, putchar
  jalr r6, r4
  ldi r1, 10
  movi r4, putchar
  jalr r6, r4
  ldi r1, 0
  mov r7, r5
  popd r5
  popd r6
  jalr r0, r6
