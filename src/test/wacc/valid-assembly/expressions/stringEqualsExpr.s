.data
// length of .L.str0
    .word 5
.L.str0:
    .asciz "Hello"
// length of .L.str2
    .word 3
.L.str2:
    .asciz "bar"
// length of .L.str1
    .word 3
.L.str1:
    .asciz "foo"
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, x9, [sp, #-32]!
    stp x10, x11, [sp, #16]
    mov fp, sp
    adrp x0, .L.str0
    add x0, x0, :lo12:.L.str0
    adrp x0, .L.str1
    add x0, x0, :lo12:.L.str1
    adrp x0, .L.str2
    add x0, x0, :lo12:.L.str2
    mov w12, w10
    mov w13, w10
    cmp w12, w13
    cset w14, eq
    mov w9, w14
    mov w0, w9
    bl _printb
    bl _println
    mov w13, w10
    mov w12, w11
    cmp w13, w12
    cset w14, eq
    mov w0, w14
    bl _prints
    bl _println
    mov w12, w11
    mov w13, w8
    cmp w12, w13
    cset w14, eq
    mov w0, w14
    bl _prints
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

