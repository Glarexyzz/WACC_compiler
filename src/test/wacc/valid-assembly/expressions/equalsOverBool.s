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
    mov w8, #1
    mov w9, #1
    mov w10, #0
    mov w11, #1
    mov w14, w8
    cmp w14, #1
    b.ne .L0
    mov w16, w9
    mov w17, w10
    cmp w16, w17
    cset w18, ne
    mov w15, w18
    cmp w15, #1
    cset w12, eq
    cmp w12, #1
    b.eq .L0
    mov w13, w11
    cmp w13, #1
    cset w0, eq
    bl _printb
    bl _println
    mov w15, w8
    cmp w15, #1
    b.ne .L0
    mov w14, w9
    cmp w14, #1
    cset w13, eq
    mov w14, w10
    cmp w14, #1
    b.eq .L0
    mov w15, w11
    cmp w15, #1
    cset w12, eq
    cmp w13, w12
    cset w15, ne
    mov w0, w15
    bl _printb
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

