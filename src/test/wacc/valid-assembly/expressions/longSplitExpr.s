.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, x9, [sp, #-80]!
    stp x10, x11, [sp, #16]
    stp x12, x13, [sp, #32]
    stp x14, x15, [sp, #48]
    stur x16, [sp, #64]
    mov fp, sp
    mov w17, #1
    mov w18, #2
    adds w10, w17, w18
    b.vs _errOverflow
    mov w18, #3
    mov w17, #4
    adds w12, w18, w17
    b.vs _errOverflow
    mov w17, #5
    mov w18, #6
    adds w15, w17, w18
    b.vs _errOverflow
    mov w18, #7
    mov w17, #8
    adds w14, w18, w17
    b.vs _errOverflow
    mov w17, #9
    mov w18, #10
    adds w8, w17, w18
    b.vs _errOverflow
    mov w18, #11
    mov w17, #12
    adds w9, w18, w17
    b.vs _errOverflow
    mov w17, #13
    mov w18, #14
    adds w13, w17, w18
    b.vs _errOverflow
    mov w18, #15
    mov w17, #16
    adds w16, w18, w17
    b.vs _errOverflow
    mov w11, #17
    mov w19, w10
    mov w10, w12
    adds w8, w19, w10
    b.vs _errOverflow
    mov w9, w15
    adds w27, w8, w9
    b.vs _errOverflow
    mov w28, w14
    adds w25, w27, w28
    b.vs _errOverflow
    mov w26, w8
    adds w23, w25, w26
    b.vs _errOverflow
    mov w24, w9
    adds w21, w23, w24
    b.vs _errOverflow
    mov w22, w13
    adds w19, w21, w22
    b.vs _errOverflow
    mov w20, w16
    adds w17, w19, w20
    b.vs _errOverflow
    mov w18, w11
    adds w0, w17, w18
    b.vs _errOverflow
    bl exit
    mov x0, #0
    // Function epilogue
    ldur x16, [sp, #16]
    ldp x14, x15, [sp]
    ldp x12, x13, [sp]
    ldp x10, x11, [sp]
    ldp x8, x9, [sp], #80
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

