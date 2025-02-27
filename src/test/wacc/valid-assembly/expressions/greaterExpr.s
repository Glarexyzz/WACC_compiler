.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, x9, [sp, #-32]!
    stur x10, [sp, #16]
    mov fp, sp
    mov w8, #2
    mov w9, #6
    mov w10, #4
    mov w11, w8
    mov w12, w9
    cmp w11, w12
    cset w13, gt
    mov w0, w13
    bl _printi
    bl _println
    mov w12, w9
    mov w11, w10
    cmp w12, w11
    cset w13, gt
    mov w0, w13
    bl _printi
    bl _println
    mov x0, #0
    // Function epilogue
    ldur x10, [sp, #16]
    ldp x8, x9, [sp], #32
    ldp fp, lr, [sp], #16
    ret

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

