.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, xzr, [sp, #-16]!
    mov fp, sp
    mov w8, #10
    mov w8, #20
    mov w0, w8
    bl exit
    mov x0, #0
    // Function epilogue
    ldp x8, xzr, [sp], #16
    ldp fp, lr, [sp], #16
    ret
