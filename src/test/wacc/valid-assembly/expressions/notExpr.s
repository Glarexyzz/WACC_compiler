.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, x9, [sp, #-16]!
    mov fp, sp
    mov w8, #1
    mov w9, #0
    mov w10, w8
    cmp w10, #1
    cset w0, ne
    bl _printb
    bl _println
    mov w10, w9
    cmp w10, #1
    cset w0, ne
    bl _printb
    bl _println
    mov x0, #0
    // Function epilogue
    ldp x8, x9, [sp], #16
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

