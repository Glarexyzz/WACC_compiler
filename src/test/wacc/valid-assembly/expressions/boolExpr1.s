.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, xzr, [sp, #-16]!
    mov fp, sp
    mov w12, #1
    cmp w12, #1
    b.ne .L0
    mov w13, #0
    cmp w13, #1
    cset w10, eq
    cmp w10, #1
    b.eq .L0
    mov w13, #1
    cmp w13, #1
    b.ne .L0
    mov w12, #0
    cmp w12, #1
    cset w11, eq
    cmp w11, #1
    cset w9, eq
    cmp w9, #1
    cset w8, ne
    mov x0, #0
    // Function epilogue
    ldp x8, xzr, [sp], #16
    ldp fp, lr, [sp], #16
    ret

