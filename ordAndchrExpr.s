.data
// length of .L.str0
    .word 4
.L.str0:
    .asciz " is "
// length of .L.str1
    .word 4
.L.str1:
    .asciz " is "
.align 4
.text
.global main
main:
    // Main/Branch prologue
    stp fp, lr, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    mov fp, sp
    mov w19, #97
    mov w20, #99
    mov w0, w19
    bl _printc
    adrp x0, .L.str0
    add x0, x0, :lo12:.L.str0
    bl _prints
    mov w21, w19
    mov w0, w19
    bl _printi
    bl _println
    mov w0, w20
    bl _printi
    adrp x0, .L.str1
    add x0, x0, :lo12:.L.str1
    bl _prints
    mov w21, w20
    tst w21, #-128
    csel x1, x0, x1, ne
    b.ne _errBadChar
    mov w0, w21
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

// length of .L._errBadChar_str0
    .word 49
.L._errBadChar_str0:
    .asciz "fatal error: int %d is not ascii character 0-127\n"
.align 4
_errBadChar:
    adr x0, .L._errBadChar_str0
    bl printf
    mov x0, #0
    bl fflush
    mov w0, #-1
    bl exit
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

