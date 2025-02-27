.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, x9, [sp, #-16]!
    mov fp, sp
    mov w8, #89
    mov w9, #0
    mov x0, #0
    // Function epilogue
    ldp x8, x9, [sp], #16
    ldp fp, lr, [sp], #16
    ret
