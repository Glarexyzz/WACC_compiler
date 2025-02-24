.data
// length of .L.str0
    .word 14
.L.str0:
    .asciz "Hello World!
"
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    mov fp, sp
    adrp x0, .L.str0
    add x0, x0, :lo12:.L.str0
    bl prints
    mov x0, #0
    // Function epilogue
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
