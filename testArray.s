.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x19, x20, [sp, #-48]!
    stp x21, x22, [sp, #16]
    stur x23, [sp, #32]
    mov fp, sp
    mov w0, #20
    bl _malloc
    mov x16, x0
    add x16, x16, #4
    mov w8, #4
    stur w8, [x16, #-4]
    mov w8, #4
    str w8, [x16]
    mov w8, #9
    str w8, [x16, #4]
    mov w8, #3
    str w8, [x16, #8]
    mov w8, #4
    str w8, [x16, #12]
    mov x19, x16
    mov w0, #16
    bl _malloc
    mov x16, x0
    add x16, x16, #4
    mov w8, #3
    stur w8, [x16, #-4]
    mov w8, #4
    str w8, [x16]
    mov w8, #5
    str w8, [x16, #4]
    mov w8, #63
    str w8, [x16, #8]
    mov x19, x16
    mov w0, #12
    bl _malloc
    mov x16, x0
    add x16, x16, #4
    mov w8, #2
    stur w8, [x16, #-4]
    mov w8, #1
    str w8, [x16]
    mov w8, #2
    str w8, [x16, #4]
    mov x19, x16
    mov w0, #12
    bl _malloc
    mov x16, x0
    add x16, x16, #4
    mov w8, #2
    stur w8, [x16, #-4]
    mov w8, #1
    str w8, [x16]
    mov w8, #2
    str w8, [x16, #4]
    mov x19, x16
    mov w0, #16
    bl _malloc
    mov x16, x0
    add x16, x16, #4
    mov w8, #3
    stur w8, [x16, #-4]
    mov w8, #1
    str w8, [x16]
    mov w8, #4
    str w8, [x16, #4]
    mov w8, #5
    str w8, [x16, #8]
    mov x19, x16
    mov x0, #0
    // Function epilogue
    ldur x23, [sp, #16]
    ldp x21, x22, [sp]
    ldp x19, x20, [sp], #48
    ldp fp, lr, [sp], #16
    ret

