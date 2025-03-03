.data
.align 4
.text
.global main
main:
    // Main/Branch prologue
    stp fp, lr, [sp, #-16]!
    stp x19, xzr, [sp, #-16]!
    mov fp, sp
    mov w0, w19
    bl _println
    mov w0, w19
    bl _println
    // Main/Branch epilogue
    mov x0, #0
    ldp x19, xzr, [sp], #16
    ldp fp, lr, [sp], #16
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

