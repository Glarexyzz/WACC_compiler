.data
.align 4
.text
.global main
main:
    // Main/Branch prologue
    stp fp, lr, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    mov fp, sp
    mov w0, #16
    bl _malloc
    mov x16, x0
    mov w8, #2
    str w8, [x16]
    mov w8, #3
    str w8, [x16, #8]
    mov x19, x16
    mov w0, #16
    bl _malloc
    mov x16, x0
    mov w8, #1
    str w8, [x16]
    mov w8, w19
    str w8, [x16, #8]
    mov x20, x16
    // Main/Branch epilogue
    mov x0, #0
    ldp x19, x20, [sp], #16
    ldp fp, lr, [sp], #16
    ret

