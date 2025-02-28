.data
// length of .L.str0
    .word 12
.L.str0:
    .asciz "min value = "
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x19, x20, [sp, #-32]!
    stur x21, [sp, #16]
    mov fp, sp
    mov w20, #0
    mov w19, #10
    mov w21, #17
    b .L0
.L0:
    mov w22, w19
    mov w23, #1
    sub w19, w22, w23
    b.vs _errOverflow
    mov w23, w21
    mov w22, #1
    sub w21, w23, w22
    b.vs _errOverflow
    mov w22, w20
    mov w23, #1
    adds w20, w22, w23
    b.vs _errOverflow
.L1:
    mov w24, w21
    mov w25, #0
    cmp w24, w25
    cset w8, gt
    mov w23, w8
    mov w25, w19
    mov w24, #0
    cmp w25, w24
    cset w8, gt
    mov w22, w8
    cmp w23, #1
    b.ne .L2
    cmp w22, #1
.L2:
    cset w8, eq
    cmp w8, #1
    b.eq .L3
.L3:
    adrp x0, .L.str0
    add x0, x0, :lo12:.L.str0
    bl _prints
    mov w0, w20
    bl _printi
    bl _println
    mov x0, #0
    // Function epilogue
    ldur x21, [sp, #16]
    ldp x19, x20, [sp], #32
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

