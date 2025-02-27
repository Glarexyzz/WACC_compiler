.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, x9, [sp, #-32]!
    stp x10, x11, [sp, #16]
    mov fp, sp
    mov w24, #1
    mov w16, #2
    adds w25, w24, w16
    b.vs _errOverflow
    mov w13, #3
    adds w22, w25, w13
    b.vs _errOverflow
    mov w14, #4
    adds w23, w22, w14
    b.vs _errOverflow
    mov w11, #5
    adds w20, w23, w11
    b.vs _errOverflow
    mov w12, #6
    adds w10, w20, w12
    b.vs _errOverflow
    mov w21, #7
    adds w9, w10, w21
    b.vs _errOverflow
    mov w19, #8
    adds w28, w9, w19
    b.vs _errOverflow
    mov w8, #9
    adds w26, w28, w8
    b.vs _errOverflow
    mov w27, #10
    adds w24, w26, w27
    b.vs _errOverflow
    mov w25, #11
    adds w22, w24, w25
    b.vs _errOverflow
    mov w23, #12
    adds w20, w22, w23
    b.vs _errOverflow
    mov w21, #13
    adds w18, w20, w21
    b.vs _errOverflow
    mov w19, #14
    adds w16, w18, w19
    b.vs _errOverflow
    mov w17, #15
    adds w14, w16, w17
    b.vs _errOverflow
    mov w15, #16
    adds w12, w14, w15
    b.vs _errOverflow
    mov w13, #17
    adds w9, w12, w13
    b.vs _errOverflow
    mov w9, #-1
    mov w19, #2
    sub w24, w9, w19
    b.vs _errOverflow
    mov w8, #3
    sub w13, w24, w8
    b.vs _errOverflow
    mov w25, #4
    sub w14, w13, w25
    b.vs _errOverflow
    mov w22, #5
    sub w11, w14, w22
    b.vs _errOverflow
    mov w23, #6
    sub w12, w11, w23
    b.vs _errOverflow
    mov w20, #7
    sub w21, w12, w20
    b.vs _errOverflow
    mov w10, #8
    sub w19, w21, w10
    b.vs _errOverflow
    mov w9, #9
    sub w8, w19, w9
    b.vs _errOverflow
    mov w28, #10
    sub w27, w8, w28
    b.vs _errOverflow
    mov w26, #11
    sub w22, w27, w26
    b.vs _errOverflow
    mov w25, #12
    sub w20, w22, w25
    b.vs _errOverflow
    mov w23, #13
    sub w18, w20, w23
    b.vs _errOverflow
    mov w21, #14
    sub w16, w18, w21
    b.vs _errOverflow
    mov w19, #15
    sub w14, w16, w19
    b.vs _errOverflow
    mov w17, #16
    sub w12, w14, w17
    b.vs _errOverflow
    mov w15, #17
    sub w10, w12, w15
    b.vs _errOverflow
    mov w20, #1
    mov w12, #2
    smull x23, w20, w12
    cmp x23, w23, sxtw
    b.ne _errOverflow
    mov w10, w23
    mov w21, #3
    smull x12, w10, w21
    cmp x12, w12, sxtw
    b.ne _errOverflow
    mov w28, w12
    mov w19, #4
    smull x21, w28, w19
    cmp x21, w21, sxtw
    b.ne _errOverflow
    mov w26, w21
    mov w27, #5
    smull x19, w26, w27
    cmp x19, w19, sxtw
    b.ne _errOverflow
    mov w20, w19
    mov w22, #6
    smull x27, w20, w22
    cmp x27, w27, sxtw
    b.ne _errOverflow
    mov w18, w27
    mov w23, #7
    smull x20, w18, w23
    cmp x20, w20, sxtw
    b.ne _errOverflow
    mov w16, w20
    mov w21, #8
    smull x23, w16, w21
    cmp x23, w23, sxtw
    b.ne _errOverflow
    mov w17, w23
    mov w19, #9
    smull x21, w17, w19
    cmp x21, w21, sxtw
    b.ne _errOverflow
    mov w15, w21
    mov w12, #10
    smull x19, w15, w12
    cmp x19, w19, sxtw
    b.ne _errOverflow
    mov w11, w19
    mov w8, #10
    mov w19, w9
    mov w17, w10
    adds w12, w19, w17
    b.vs _errOverflow
    mov w17, w11
    mov w19, w8
    cmp w19, #0
    b.eq _errDivZero
    sdiv w15, w17, w19
    adds w0, w12, w15
    b.vs _errOverflow
    bl _printi
    bl _println
    mov w21, w9
    mov w16, w10
    adds w19, w21, w16
    b.vs _errOverflow
    mov w16, w11
    mov w21, w8
    cmp w21, #0
    b.eq _errDivZero
    sdiv w17, w16, w21
    adds w15, w19, w17
    b.vs _errOverflow
    mov w12, #256
    cmp w12, #0
    b.eq _errDivZero
    sdiv w1, w15, w12
    msub w0, w1, w12, w15
    bl _printi
    bl _println
    mov w17, w9
    mov w19, w10
    adds w12, w17, w19
    b.vs _errOverflow
    mov w19, w11
    mov w17, w8
    cmp w17, #0
    b.eq _errDivZero
    sdiv w15, w19, w17
    adds w0, w12, w15
    b.vs _errOverflow
    bl exit
    mov x0, #0
    // Function epilogue
    ldp x10, x11, [sp]
    ldp x8, x9, [sp], #32
    ldp fp, lr, [sp], #16
    ret

// length of .L._errOverflow_str0
    .word 52
.L._errOverflow_str0:
    .asciz "fatal error: integer overflow or underflow occurred\n"
.align 4
_errOverflow:
    adr x0, .L._errOverflow_str0
    bl _prints
    mov w0, #-1
    bl exit
// length of .L._printi_str0
    .word 2
.L._printi_str0:
    .asciz "%d"
.align 4
_printi:
    stp lr, xzr, [sp, #-16]!
    mov x1, x0
    adr x0, .L._printi_str0
    bl printf
    mov x0, #0
    bl fflush
    ldp lr, xzr, [sp], #16
    ret

// length of .L._prints_str0
    .word 4
.L._prints_str0:
    .asciz "%.*s"
.align 4
_prints:
    stp lr, xzr, [sp, #-16]!
    mov x2, x0
    ldur w1, [x0, #-4]
    adr x0, .L._prints_str0
    bl printf
    mov x0, #0
    bl fflush
    ldp lr, xzr, [sp], #16
    ret

// length of .L._println_str0
    .word 0
.L._println_str0:
    .asciz ""
.align 4
_println:
    stp lr, xzr, [sp, #-16]!
    adr x0, .L._println_str0
    bl puts
    mov x0, #0
    bl fflush
    ldp lr, xzr, [sp], #16
    ret

// length of .L._errDivZero_str0
    .word 40
.L._errDivZero_str0:
    .asciz "fatal error: division or modulo by zero\n"
.align 4
_errDivZero:
    adr x0, .L._errDivZero_str0
    bl _prints
    mov w0, #-1
    bl exit
