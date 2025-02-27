.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, xzr, [sp, #-16]!
    mov fp, sp
    mov w19, #2
    mov w20, #3
    adds w17, w19, w20
    b.vs _errOverflow
    mov w18, #2
    adds w15, w17, w18
    b.vs _errOverflow
    mov w16, #1
    adds w13, w15, w16
    b.vs _errOverflow
    mov w14, #1
    adds w11, w13, w14
    b.vs _errOverflow
    mov w12, #1
    adds w9, w11, w12
    b.vs _errOverflow
    mov w16, #1
    mov w15, #2
    adds w14, w16, w15
    b.vs _errOverflow
    mov w15, #3
    mov w18, #4
    mov w17, #6
    cmp w17, #0
    b.eq _errDivZero
    sdiv w16, w18, w17
    sub w13, w15, w16
    b.vs _errOverflow
    smull x16, w14, w13
    cmp x16, w16, sxtw
    b.ne _errOverflow
    mov w12, w16
    mov w16, #2
    mov w17, #18
    mov w18, #17
    sub w15, w17, w18
    b.vs _errOverflow
    smull x18, w16, w15
    cmp x18, w18, sxtw
    b.ne _errOverflow
    mov w13, w18
    mov w20, #3
    mov w19, #4
    smull x21, w20, w19
    cmp x21, w21, sxtw
    b.ne _errOverflow
    mov w18, w21
    mov w17, #4
    cmp w17, #0
    b.eq _errDivZero
    sdiv w15, w18, w17
    mov w16, #6
    adds w14, w15, w16
    b.vs _errOverflow
    adds w11, w13, w14
    b.vs _errOverflow
    cmp w11, #0
    b.eq _errDivZero
    sdiv w10, w12, w11
    sub w8, w9, w10
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
