.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x19, xzr, [sp, #-16]!
    mov fp, sp
    mov w0, #8
    bl _malloc
    mov x16, x0
    add x16, x16, #4
    mov w8, #1
    stur w8, [x16, #-4]
    mov w8, #0
    str w8, [x16]
    mov x19, x16
    mov x0, #0
    // Function epilogue
    ldp x19, xzr, [sp], #16
    ldp fp, lr, [sp], #16
    ret

