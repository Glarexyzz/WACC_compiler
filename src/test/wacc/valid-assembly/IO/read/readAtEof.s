.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, x9, [sp, #-16]!
    mov fp, sp
    mov w8, #90
    mov w0, w8
    bl _readc
    mov w8, w0
    mov w0, w8
    bl _printc
    mov w9, #90
    mov w0, w9
    bl _readc
    mov w9, w0
    mov w0, w9
    bl _printc
    bl _println
    mov x0, #0
    // Function epilogue
    ldp x8, x9, [sp], #16
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

// length of .L._readc_str0
    .word 3
.L._readc_str0:
    .asciz " %c"
.align 4
_readc:
    stp x0, lr, [sp, #-16]!
    mov x1, sp
    adr x0, .L._readc_str0
    bl scanf
    ldp x0, lr, [sp], #16
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
