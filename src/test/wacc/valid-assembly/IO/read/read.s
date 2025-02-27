.data
// length of .L.str0
    .word 31
.L.str0:
    .asciz "input an integer to continue..."
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, xzr, [sp, #-16]!
    mov fp, sp
    mov w8, #10
    adrp x0, .L.str0
    add x0, x0, :lo12:.L.str0
    bl _prints
    bl _println
    mov w0, w8
    bl _readi
    mov w8, w0
    mov x0, #0
    // Function epilogue
    ldp x8, xzr, [sp], #16
    ldp fp, lr, [sp], #16
    ret

// length of .L._readi_str0
    .word 2
.L._readi_str0:
    .asciz "%d"
.align 4
_readi:
    stp x0, lr, [sp, #-16]!
    mov x1, sp
    adr x0, .L._readi_str0
    bl scanf
    ldp x0, lr, [sp], #16
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
