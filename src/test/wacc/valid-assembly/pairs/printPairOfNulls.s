.data
// length of .L.str0
    .word 4
.L.str0:
    .asciz " = ("
// length of .L.str2
    .word 1
.L.str2:
    .asciz ")"
// length of .L.str1
    .word 1
.L.str1:
    .asciz ","
.align 4
.text
.global main
main:
    // Main/Branch prologue
    stp fp, lr, [sp, #-16]!
    stp x19, x20, [sp, #-32]!
    stur x21, [sp, #16]
    mov fp, sp
    mov w0, #16
    bl _malloc
    mov x16, x0
    str w8, [x16]
    str w8, [x16, #8]
    mov x19, x16
    mov w0, w19
    adrp x0, .L.str0
    add x0, x0, :lo12:.L.str0
    bl _prints
    mov w0, w20
    adrp x0, .L.str1
    add x0, x0, :lo12:.L.str1
    bl _prints
    mov w0, w21
    adrp x0, .L.str2
    add x0, x0, :lo12:.L.str2
    bl _prints
    bl _println
    // Main/Branch epilogue
    mov x0, #0
    ldur x21, [sp, #16]
    ldp x19, x20, [sp], #32
    ldp fp, lr, [sp], #16
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

