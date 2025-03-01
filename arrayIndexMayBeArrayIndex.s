.data
.align 4
.text
.global main
main:
    // Main/Branch prologue
    stp fp, lr, [sp, #-16]!
    stp x19, x20, [sp, #-32]!
    stp x21, x22, [sp, #16]
    mov fp, sp
    mov w0, #16
    bl _malloc
    mov x16, x0
    add x16, x16, #4
    mov w8, #3
    stur w8, [x16, #-4]
    mov w8, #2
    str w8, [x16]
    mov w8, #0
    str w8, [x16, #4]
    mov w8, #1
    str w8, [x16, #8]
    mov x22, x16
    mov w0, #16
    bl _malloc
    mov x16, x0
    add x16, x16, #4
    mov w8, #3
    stur w8, [x16, #-4]
    mov w8, #1
    str w8, [x16]
    mov w8, #2
    str w8, [x16, #4]
    mov w8, #0
    str w8, [x16, #8]
    mov x20, x16
    mov w0, #16
    bl _malloc
    mov x16, x0
    add x16, x16, #4
    mov w8, #3
    stur w8, [x16, #-4]
    mov w8, #5
    str w8, [x16]
    mov w8, #6
    str w8, [x16, #4]
    mov w8, #7
    str w8, [x16, #8]
    mov x21, x16
    mov w19, #0
    b .L1
.L0:
    mov w17, w19
    mov x7, x20
    bl _arrLoad4
    mov w17, w7
    mov x7, x22
    bl _arrLoad4
    mov w17, w7
    mov w17, w19
    mov x7, x20
    bl _arrLoad4
    mov w17, w7
    mov x7, x22
    bl _arrLoad4
    mov w17, w7
    mov x7, x21
    bl _arrLoad4
    mov w23, w7
    adds w23, w23, #1
    b.vs _errOverflow
    mov x7, x21
    bl _arrStore4
    mov w17, w19
    mov x7, x20
    bl _arrLoad4
    mov w17, w7
    mov x7, x22
    bl _arrLoad4
    mov w17, w7
    mov x7, x21
    bl _arrLoad4
    mov w0, w7
    bl _printi
    bl _println
    adds w19, w19, #1
    b.vs _errOverflow
.L1:
    mov w23, w19
    mov w24, #3
    cmp w23, w24
    cset w8, ne
    mov w8, w8
    cmp w8, #1
    b.eq .L0
    // Main/Branch epilogue
    mov x0, #0
    ldp x21, x22, [sp]
    ldp x19, x20, [sp], #32
    ldp fp, lr, [sp], #16
    ret

// length of .L._errOutOfBounds_str0
    .word 42
.L._errOutOfBounds_str0:
    .asciz "fatal error: array index %d out of bounds\n"
.align 4
_errOutOfBounds:
    adr x0, .L._errOutOfBounds_str0
    bl printf
    mov x0, #0
    bl fflush
    mov w0, #-1
    bl exit
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

// length of .L._errOutOfMemory_str0
    .word 27
.L._errOutOfMemory_str0:
    .asciz "fatal error: out of memory\n"
.align 4
_errOutOfMemory:
    adr x0, .L._errOutOfMemory_str0
    bl _prints
    mov w0, #-1
    bl exit
_arrLoad4:
    stp lr, xzr, [sp, #-16]!
    cmp w17, #0
    csel x1, x17, x1, lt
    b.lt _errOutOfBounds
    ldur w30, [x7, #-4]
    cmp w17, w30
    csel x1, x17, x1, ge
    b.ge _errOutOfBounds
    ldr w7, [x7, x17, lsl #2]
    ldp lr, xzr, [sp], #16
    ret

// length of .L._errOutOfBounds_str0
    .word 42
.L._errOutOfBounds_str0:
    .asciz "fatal error: array index %d out of bounds\n"
.align 4
_errOutOfBounds:
    adr x0, .L._errOutOfBounds_str0
    bl printf
    mov x0, #0
    bl fflush
    mov w0, #-1
    bl exit
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

_malloc:
    stp lr, xzr, [sp, #-16]!
    bl malloc
    cbz x0, _errOutOfMemory
    ldp lr, xzr, [sp], #16
    ret

_arrStore4:
    stp lr, xzr, [sp, #-16]!
    cmp w17, #0
    csel x1, x17, x1, lt
    b.lt _errOutOfBounds
    ldur w30, [x7, #-4]
    cmp w17, w30
    csel x1, x17, x1, ge
    b.ge _errOutOfBounds
    strb w23, [x7, x17, lsl #2]
    ldp lr, xzr, [sp], #16
    ret

