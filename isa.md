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
    add 1000
    sub 1001
    mul 1010
    div 1011
    rem 1100
    and 1101
    or  1110
    xor 1111

    001 ddd aaa bbb oooo

    asr  0000
    lsr  0001
    lsl  0010
    slt  0011
    sltu 0100
    fadd 1000
    fsub 1001
    fmul 1010
    fdiv 1011
    fpow 1100

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
        cvt     01001
        fneg    01010
        finv    01011
        fint    01100

    110 000 000 01 iiiii    trap
    110 aaa bbb 10 iiiii    ld
    110 aaa bbb 11 iiiii    st

RI
--

    111 rrr oo iiiiiiii (r != 0)

    ldi     00
    auipc   01
    sli     10
    sti     11

R
-

    111 000 rrr ooooooo
    
    pshb 0000000
    popb 0000001
    pshs 0000010
    pops 0000011
    pshw 0000100
    popw 0000101
    pshd 0000110
    popd 0000111
    spsr 0001000
    gpsr 0001001
    rte  0001010
