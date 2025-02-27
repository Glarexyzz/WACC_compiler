.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, x9, [sp, #-32]!
    stp x10, x11, [sp, #16]
    mov fp, sp
    mov w9, #2
    mov w10, #4
    mov w11, #4
    mov w12, w9
    mov w13, w10
    cmp w12, w13
    cset w14, ne
    mov w8, w14
    mov w0, w8
    bl _printb
    bl _println
    mov w13, w9
    mov w12, w10
    cmp w13, w12
    cset w14, ne
    mov w0, w14
    bl _printi
    bl _println
    mov w12, w10
    mov w13, w11
    cmp w12, w13
    cset w14, ne
    mov w0, w14
    bl _printi
    bl _println
    mov x0, #0
    // Function epilogue
    ldp x10, x11, [sp]
    ldp x8, x9, [sp], #32
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

// length of .L._printi_str0
    .word 2
.L._printi_str0:
    .asciz "%d"
.align 4
_printi:
    stp lr, xzr, [sp, #-16]!
    mov x1, x0
    adr x0, .L._printi_str0
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

