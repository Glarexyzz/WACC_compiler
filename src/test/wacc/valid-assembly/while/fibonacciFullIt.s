.data
// length of .L.str0
    .word 61
.L.str0:
    .asciz "This program calculates the nth fibonacci number iteratively."
// length of .L.str2
    .word 15
.L.str2:
    .asciz "The input n is "
// length of .L.str1
    .word 42
.L.str1:
    .asciz "Please enter n (should not be too large): "
// length of .L.str3
    .word 28
.L.str3:
    .asciz "The nth fibonacci number is "
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x19, x20, [sp, #-32]!
    stp x21, x22, [sp, #16]
    mov fp, sp
    adrp x0, .L.str0
    add x0, x0, :lo12:.L.str0
    bl _prints
    bl _println
    adrp x0, .L.str1
    add x0, x0, :lo12:.L.str1
    bl _prints
    mov w22, #0
    mov w0, w22
    bl _readi
    mov w22, w0
    adrp x0, .L.str2
    add x0, x0, :lo12:.L.str2
    bl _prints
    mov w0, w22
    bl _printi
    bl _println
    adrp x0, .L.str3
    add x0, x0, :lo12:.L.str3
    bl _prints
    mov w20, #0
    mov w21, #1
    mov w19, #0
    b .L0
.L0:
    mov w19, w20
    mov w20, w21
    mov w23, w19
    mov w24, w21
    adds w21, w23, w24
    b.vs _errOverflow
    mov w24, w22
    mov w23, #1
    sub w22, w24, w23
    b.vs _errOverflow
.L1:
    mov w23, w22
    mov w24, #0
    cmp w23, w24
    cset w8, gt
    mov w8, w8
    cmp w8, #1
    b.eq .L2
.L2:
    mov w0, w20
    bl _printi
    bl _println
    mov x0, #0
    // Function epilogue
    ldp x21, x22, [sp]
    ldp x19, x20, [sp], #32
    ldp fp, lr, [sp], #16
    ret

// length of .L._readi_str0
    .word 2
.L._readi_str0:
    .asciz "%d"
.align 4
_readi:
    stp x0, lr, [sp, #-16]!
    mov x1, sp
    adr x0, .L._readi_str0
    bl scanf
    ldp x0, lr, [sp], #16
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

