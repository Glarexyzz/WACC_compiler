.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    mov fp, sp
    b .L0
.L0:

.L1:
    mov w8, #0
    cmp w8, #1
    b.eq .L2
.L2:
    mov x0, #0
    // Function epilogue
    ldp fp, lr, [sp], #16
    ret

