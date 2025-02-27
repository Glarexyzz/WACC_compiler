.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, x9, [sp, #-16]!
    mov fp, sp
    mov w8, #97
    mov w9, #122
    mov w10, w8
    mov w11, w9
    cmp w10, w11
    cset w12, eq
    mov w0, w12
    bl _printc
    bl _println
    mov w11, w8
    mov w10, w9
    cmp w11, w10
    cset w12, ne
    mov w0, w12
    bl _printc
    bl _println
    mov w10, w8
    mov w11, w9
    cmp w10, w11
    cset w12, lt
    mov w0, w12
    bl _printc
    bl _println
    mov w11, w8
    mov w10, w9
    cmp w11, w10
    cset w12, le
    mov w0, w12
    bl _printc
    bl _println
    mov w10, w8
    mov w11, w9
    cmp w10, w11
    cset w12, gt
    mov w0, w12
    bl _printc
    bl _println
    mov w11, w8
    mov w10, w9
    cmp w11, w10
    cset w12, ge
    mov w0, w12
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

