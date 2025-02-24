.data
// length of .L.str0
    .word 8
.L.str0:
    .asciz "True is "
// length of .L.str1
    .word 9
.L.str1:
    .asciz "False is "
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
    mov w0, #1
    bl printb
    bl println
    adrp x0, .L.str1
    add x0, x0, :lo12:.L.str1
    bl prints
    mov w0, #0
    bl printb
    bl println
    mov x0, #0
    // Function epilogue
    ldp fp, lr, [sp], #16
    ret

// length of .L.printb_str0
    .word 5
.L.printb_str0:
    .asciz "false"
// length of .L.printb_str1
    .word 4
.L.printb_str1:
    .asciz "true"
// length of .L.printb_str2
    .word 4
.L.printb_str2:
    .asciz "%.*s"
.align 4
printb:
    stp lr, xzr, [sp, #-16]!
    cmp w0, #0
    b.ne .L_printb0
    adr x2, .L.printb_str0
    b .L_printb1
.L_printb0:
    adr x2, .L.printb_str1
.L_printb1:
    ldur w1, [x2, #-4]
    adr x0, .L.printb_str2
    bl printf
    mov x0, #0
    bl fflush
    ldp lr, xzr, [sp], #16
    ret

// length of .L.println_str0
    .word 0
.L.println_str0:
    .asciz ""
.align 4
_println:
    stp lr, xzr, [sp, #-16]!
    adr x0, .L.println_str0
    bl puts
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
