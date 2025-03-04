.data
.align 4
.text
.global main
main:
    // Main/Branch prologue
    stp fp, lr, [sp, #-16]!
    stp x19, x20, [sp, #-32]!
    stp x21, x22, [sp, #16]
    mov fp, sp
    mov w19, #1
    mov w20, #1
    mov w21, #0
    mov w22, #1
    mov w24, w19
    mov w26, w20
    mov w27, w21
    cmp w26, w27
    cset w8, ne
    mov w25, w8
    cmp w24, #1
    b.ne .L0
    cmp w25, #1
.L0:
    cset w23, eq
    mov w25, w22
    cmp w23, #1
    cset w23, eq
    cmp w25, #1
    cset w25, eq
    orr w0, w23, w25
    bl _printb
    bl _println
    mov w23, w19
    mov w24, w20
    cmp w23, #1
    b.ne .L1
    cmp w24, #1
.L1:
    cset w25, eq
    mov w23, w21
    mov w27, w22
    cmp w23, #1
    cset w23, eq
    cmp w27, #1
    cset w27, eq
    orr w24, w23, w27
    cmp w25, w24
    cset w8, ne
    mov w0, w8
    bl _printb
    bl _println
    // Main/Branch epilogue
    mov x0, #0
    ldp x21, x22, [sp]
    ldp x19, x20, [sp], #32
    ldp fp, lr, [sp], #16
    ret

// length of .L._printb_str0
    .word 5
.L._printb_str0:
    .asciz "false"
// length of .L._printb_str1
    .word 4
.L._printb_str1:
    .asciz "true"
// length of .L._printb_str2
    .word 4
.L._printb_str2:
    .asciz "%.*s"
.align 4
_printb:
    stp lr, xzr, [sp, #-16]!
    cmp w0, #0
    b.ne .L_printb0
    adr x2, .L._printb_str0
    b .L_printb1
.L_printb0:
    adr x2, .L._printb_str1
.L_printb1:
    ldur w1, [x2, #-4]
    adr x0, .L._printb_str2
    bl printf
    mov x0, #0
    bl fflush
    ldp lr, xzr, [sp], #16
    ret

// length of .L._prints_str0
    .word 4
.L._prints_str0:
    .asciz "%.*s"
.align 4
_prints:
    stp lr, xzr, [sp, #-16]!
    mov x2, x0
    ldur w1, [x0, #-4]
    adr x0, .L._prints_str0
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

// length of .L._errDivZero_str0
    .word 40
.L._errDivZero_str0:
    .asciz "fatal error: division or modulo by zero\n"
.align 4
_errDivZero:
    adr x0, .L._errDivZero_str0
    bl _prints
    mov w0, #-1
    bl exit
