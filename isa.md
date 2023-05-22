RRR
---

    00 o ddd aaa bbb oooo

    ldb 
    stb
    ldw
    stw
    ldd
    std
    ldl
    stl
    add
    sub
    mul
    div
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
    110 aaa bbb ooooooo     
    110 000 000 01 iiiii    trap
    110 000 000 11 ooooo    32 no operand instructions
      brk
      rts
      rti

RI
--

    111 rrr oo iiiiiiii

    ldi
    sli
    -
    -

