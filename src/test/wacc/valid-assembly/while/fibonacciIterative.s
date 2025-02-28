.data
// length of .L.str0
    .word 35
.L.str0:
    .asciz "The first 20 fibonacci numbers are:"
// length of .L.str2
    .word 3
.L.str2:
    .asciz "..."
// length of .L.str1
    .word 2
.L.str1:
    .asciz ", "
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x19, x20, [sp, #-32]!
    stp x21, x22, [sp, #16]
    mov fp, sp
    mov w20, #0
    mov w21, #0
    mov w22, #1
    mov w19, #0
    adrp x0, .L.str0
    add x0, x0, :lo12:.L.str0
    bl _prints
    bl _println
    b .L0
.L0:
    mov w0, w21
    bl _printi
    adrp x0, .L.str1
    add x0, x0, :lo12:.L.str1
    bl _prints
    mov w19, w21
    mov w21, w22
    mov w23, w19
    mov w24, w22
    adds w22, w23, w24
    b.vs _errOverflow
    mov w24, w20
    mov w23, #1
    adds w20, w24, w23
    b.vs _errOverflow
.L1:
    mov w23, w20
    mov w24, #20
    cmp w23, w24
    cset w8, lt
    mov w8, w8
    cmp w8, #1
    b.eq .L2
.L2:
    adrp x0, .L.str2
    add x0, x0, :lo12:.L.str2
    bl _prints
    bl _println
    mov x0, #0
    // Function epilogue
    ldp x21, x22, [sp]
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

