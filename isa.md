RRR
---

    000 ddd aaa bbb oooo

    ldb 0000
    stb 0001
    lds 0010
    sts 0011
    ldw 0100
    stw 0101
    ldd 0110
    std 0111
    add
    sub
    mul
    div
    rem
    and
    or
    xor

    001 ddd aaa bbb oooo

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

RR
--

    110 aaa bbb 00 ooooo    32 RR instructions
        jalr    00000
        zeb     00001
        zes     00010
        zew     00011
        seb     00100
        ses     00101
        sew     00110
        neg     00111
        not     01000
        fneg    01001
        finv    01010

    110 000 000 01 iiiii    trap
    110 000 000 10 ooooo    32 no operand instructions
        ---   00000
        ---   00001
        rte   00010
        sei   00011
        cli   00100

RI
--

    111 rrr oo iiiiiiii (r != 0)

    ldi     00
    ---     01
    sli     10
    sti     11

R
-

    111 000 rrr ooooooo
