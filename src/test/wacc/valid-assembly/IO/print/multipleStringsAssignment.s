.data
// length of .L.str0
    .word 6
.L.str0:
    .asciz "s1 is "
// length of .L.str2
    .word 16
.L.str2:
    .asciz "Now make s1 = s2"
// length of .L.str1
    .word 6
.L.str1:
    .asciz "s2 is "
// length of .L.str4
    .word 6
.L.str4:
    .asciz "s2 is "
// length of .L.str3
    .word 6
.L.str3:
    .asciz "s1 is "
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
    bl printi
    bl println
    adrp x0, .L.str1
    add x0, x0, :lo12:.L.str1
    bl prints
    bl printi
    bl println
    adrp x0, .L.str2
    add x0, x0, :lo12:.L.str2
    bl prints
    bl println
    adrp x0, .L.str3
    add x0, x0, :lo12:.L.str3
    bl prints
    bl printi
    bl println
    adrp x0, .L.str4
    add x0, x0, :lo12:.L.str4
    bl prints
    bl printi
    bl println
    mov x0, #0
    // Function epilogue
    ldp fp, lr, [sp], #16
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
