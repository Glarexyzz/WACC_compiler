.data
// length of .L.str0
    .word 20
.L.str0:
    .asciz "Can you count to 10?"
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x8, xzr, [sp, #-16]!
    mov fp, sp
    mov w8, #1
    adrp x0, .L.str0
    add x0, x0, :lo12:.L.str0
    bl _prints
    bl _println
    mov w0, w8
    bl _printi
    bl _println
    mov w9, w8
    mov w10, #1
    adds w8, w9, w10
    b.vs _errOverflow
    mov w0, w8
    bl _printi
    bl _println
    mov w10, w8
    mov w9, #1
    adds w8, w10, w9
    b.vs _errOverflow
    mov w0, w8
    bl _printi
    bl _println
    mov w9, w8
    mov w10, #1
    adds w8, w9, w10
    b.vs _errOverflow
    mov w0, w8
    bl _printi
    bl _println
    mov w10, w8
    mov w9, #1
    adds w8, w10, w9
    b.vs _errOverflow
    mov w0, w8
    bl _printi
    bl _println
    mov w9, w8
    mov w10, #1
    adds w8, w9, w10
    b.vs _errOverflow
    mov w0, w8
    bl _printi
    bl _println
    mov w10, w8
    mov w9, #1
    adds w8, w10, w9
    b.vs _errOverflow
    mov w0, w8
    bl _printi
    bl _println
    mov w9, w8
    mov w10, #1
    adds w8, w9, w10
    b.vs _errOverflow
    mov w0, w8
    bl _printi
    bl _println
    mov w10, w8
    mov w9, #1
    adds w8, w10, w9
    b.vs _errOverflow
    mov w0, w8
    bl _printi
    bl _println
    mov w9, w8
    mov w10, #1
    adds w8, w9, w10
    b.vs _errOverflow
    mov w0, w8
    bl _printi
    bl _println
    mov x0, #0
    // Function epilogue
    ldp x8, xzr, [sp], #16
    ldp fp, lr, [sp], #16
    ret

// length of .L._errOverflow_str0
    .word 52
.L._errOverflow_str0:
    .asciz "fatal error: integer overflow or underflow occurred\n"
.align 4
_errOverflow:
    adr x0, .L._errOverflow_str0
    bl _prints
    mov w0, #-1
    bl exit
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

