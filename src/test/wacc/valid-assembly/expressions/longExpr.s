.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, xzr, [sp, #-16]!
    mov fp, sp
    mov w9, #1
    mov w11, #2
    mov w13, #3
    mov w15, #4
    mov w17, #5
    mov w19, #6
    mov w21, #7
    mov w23, #8
    mov w25, #9
    mov w27, #10
    mov w8, #11
    mov w19, #12
    mov w21, #13
    mov w12, #14
    mov w11, #15
    mov w14, #16
    mov w25, #17
    adds w22, w14, w25
    b.vs _errOverflow
    adds w23, w11, w22
    b.vs _errOverflow
    adds w20, w12, w23
    b.vs _errOverflow
    adds w10, w21, w20
    b.vs _errOverflow
    adds w9, w19, w10
    b.vs _errOverflow
    adds w28, w8, w9
    b.vs _errOverflow
    adds w26, w27, w28
    b.vs _errOverflow
    adds w24, w25, w26
    b.vs _errOverflow
    adds w22, w23, w24
    b.vs _errOverflow
    adds w20, w21, w22
    b.vs _errOverflow
    adds w18, w19, w20
    b.vs _errOverflow
    adds w16, w17, w18
    b.vs _errOverflow
    adds w14, w15, w16
    b.vs _errOverflow
    adds w12, w13, w14
    b.vs _errOverflow
    adds w10, w11, w12
    b.vs _errOverflow
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

