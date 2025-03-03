.data
.align 4
.text
.global main
main:
    // Main/Branch prologue
    stp fp, lr, [sp, #-16]!
    stp x19, xzr, [sp, #-16]!
    mov fp, sp
    mov w0, #16
    bl _malloc
    mov x16, x0
    mov w8, #10
    str w8, [x16]
    mov w8, #97
    str w8, [x16, #8]
    mov x19, x16
    sub x0, x19, #4
    bl free
    // Main/Branch epilogue
    mov x0, #0
    ldp x19, xzr, [sp], #16
    ldp fp, lr, [sp], #16
    ret

