RRR
---

    00 o ddd aaa bbb oooo

    ldb 00000
    stb 00001
    lds 00010
    sts 00011
    ldw
    stw
    ldd
    std
    add
    sub
    mul
    div
    rem
    and
    or
    xor
    asr
    lsr
    lsl
    
    fadd
    fsub
    fmul
    fdiv

RRI
---

    010 aaa bbb iiiiiii     beq
    011 aaa bbb iiiiiii     blu
    100 aaa bbb iiiiiii     bls
    101 aaa bbb iiiiiii     addi
    110 aaa bbb 0000000     jalr
    110 aaa bbb 00 ooooo    (ooooo != 0) 32 RR instructions
    110 000 000 01 iiiii    trap
    110 000 000 10 ooooo    32 no operand instructions
      brk   00000
      rts   00001
      rte   00010
      sei   00011
      cli   00100

RI
--

    111 rrr oo iiiiiiii (r != 0)

    ldi     00
    ---     01  // can be done using ADDI
    sli     10
    sti     11

R
-

    111 000 rrr ooooooo
