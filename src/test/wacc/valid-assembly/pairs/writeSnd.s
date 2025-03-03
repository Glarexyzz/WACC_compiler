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
    mov w8, #10
    str w8, [x16]
    mov w8, #97
    str w8, [x16, #8]
    mov x19, x16
    mov w0, w20
    bl _printc
    bl _println
    mov w0, w20
    bl _printc
    bl _println
    // Main/Branch epilogue
    mov x0, #0
    ldp x19, x20, [sp], #16
    ldp fp, lr, [sp], #16
    ret

// length of .L._printc_str0
    .word 2
.L._printc_str0:
    .asciz "%c"
.align 4
_printc:
    stp lr, xzr, [sp, #-16]!
    mov x1, x0
    adr x0, .L._printc_str0
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

