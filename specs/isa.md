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
    add 1000    (sets carry)
    sub 1001    (sets carry/borrow)
    mul 1010    (low → rd, high → rd+1)
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
    adc  0101    (add with carry, sets carry)
    sbc  0110    (subtract with borrow, sets carry)
    mulu 0111    (unsigned, low → rd, high → rd+1)
    divu 1000
    remu 1001
    fslt 1010
    fadd 1011
    fsub 1100
    fmul 1101
    fdiv 1110
    fseq 1111

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
        fsqrt   01101
        fabs    01110
        ll      01111    (load-linked, sets reservation)
        sc      10000    (store-conditional, clears reservation)
        clz     10001
        ctz     10010
        chk     10011    (traps if ra < 0 or ra > rb)
        btst    10100    (ra = (ra >> rb) & 1)
        bset    10101    (ra = ra | (1 << rb))
        bclr    10110    (ra = ra & ~(1 << rb))
        rol     10111    (ra = ra rotate left by rb)
        ror     11000    (ra = ra rotate right by rb)
        cnt     11001    (ra = popcount of rb)
        rev     11010    (ra = byte-reverse of rb)
        sext    11011    (sign-extend ra from bit width rb)
        mov     11100    (ra = rb)
        min     11101    (ra = min(ra, rb) signed)
        max     11110    (ra = max(ra, rb) signed)
        exg     11111    (swap ra and rb)

    110 aaa bbb 01 ooooo    31 RR01 instructions
        fpow    00000    (ra = pow(ra, rb), destructive)
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

    pshb  0000000
    popb  0000001
    pshs  0000010
    pops  0000011
    pshw  0000100
    popw  0000101
    pshd  0000110
    popd  0000111
    spsr  0001000    (supervisor only)
    gpsr  0001001
    rte   0001010    (supervisor only)
    fence 0001011
    wfi   0001100    (supervisor only)
    gusp  0001101    (supervisor only)
    susp  0001110    (supervisor only)
    trapv   0001111    (traps if V flag set)
    pshr    0010000    (push r1-rN onto stack)
    popr    0010001    (pop rN-r1 from stack)
    cli     0010010    (disable interrupts, supervisor only)
    sti     0010011    (enable interrupts, supervisor only)
    swsp    0010100    (swap r7 and usp, supervisor only)
    tsr     0010101    (read cycle counter into rN)
    trap0   0011000    (system call 0)
    trap1   0011001    (system call 1)
    trap2   0011010    (system call 2)
    trap3   0011011    (system call 3)
    trap4   0011100    (system call 4)
    trap5   0011101    (system call 5)
    trap6   0011110    (system call 6)
    trap7   0011111    (system call 7)
