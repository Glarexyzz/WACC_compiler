.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, xzr, [sp, #-16]!
    mov fp, sp
    mov w14, #1
    mov w25, #2
    sub w11, w14, w25
    b.vs _errOverflow
    mov w22, #3
    adds w12, w11, w22
    b.vs _errOverflow
    mov w23, #4
    sub w21, w12, w23
    b.vs _errOverflow
    mov w20, #5
    adds w19, w21, w20
    b.vs _errOverflow
    mov w10, #6
    sub w8, w19, w10
    b.vs _errOverflow
    mov w9, #7
    adds w27, w8, w9
    b.vs _errOverflow
    mov w28, #8
    sub w25, w27, w28
    b.vs _errOverflow
    mov w26, #9
    adds w23, w25, w26
    b.vs _errOverflow
    mov w24, #10
    sub w21, w23, w24
    b.vs _errOverflow
    mov w22, #11
    adds w19, w21, w22
    b.vs _errOverflow
    mov w20, #12
    sub w17, w19, w20
    b.vs _errOverflow
    mov w18, #13
    adds w15, w17, w18
    b.vs _errOverflow
    mov w16, #14
    sub w13, w15, w16
    b.vs _errOverflow
    mov w14, #15
    adds w11, w13, w14
    b.vs _errOverflow
    mov w12, #16
    sub w9, w11, w12
    b.vs _errOverflow
    mov w10, #17
    adds w8, w9, w10
    b.vs _errOverflow
    mov w0, w8
    bl exit
    mov x0, #0
    // Function epilogue
    ldp x8, xzr, [sp], #16
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

