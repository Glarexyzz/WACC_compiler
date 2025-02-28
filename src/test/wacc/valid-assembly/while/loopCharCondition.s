.data
// length of .L.str0
    .word 8
.L.str0:
    .asciz "Change c"
// length of .L.str1
    .word 36
.L.str1:
    .asciz "Should print "Change c" once before."
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x19, xzr, [sp, #-16]!
    mov fp, sp
    mov w19, #0
    b .L0
.L0:
    mov w19, #97
    adrp x0, .L.str0
    add x0, x0, :lo12:.L.str0
    bl _prints
    bl _println
.L1:
    mov w20, w19
    mov w21, #0
    cmp w20, w21
    cset w8, eq
    mov w8, w8
    cmp w8, #1
    b.eq .L2
.L2:
    adrp x0, .L.str1
    add x0, x0, :lo12:.L.str1
    bl _prints
    bl _println
    mov x0, #0
    // Function epilogue
    ldp x19, xzr, [sp], #16
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

