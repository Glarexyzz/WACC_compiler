.data
.align 4
.text
.global main
main:
    // Function prologue
    stp fp, lr, [sp, #-16]!
    stp x19, xzr, [sp, #-16]!
    mov fp, sp
    mov w0, #0
    bl _malloc
    mov x16, x0
    add x16, x16, #4
    mov w8, #2
    stur w8, [x16, #-4]
    mov w8, #1
    str w8, [x16]
    mov w8, #0
    str w8, [x16, #1]
    mov x19, x16
    sub x0, x19, #4
    bl free
    mov x0, #0
    // Function epilogue
    ldp x19, xzr, [sp], #16
    ldp fp, lr, [sp], #16
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

_malloc:
    stp lr, xzr, [sp, #-16]!
    bl malloc
    cbz x0, _errOutOfMemory
    ldp lr, xzr, [sp], #16
    ret

