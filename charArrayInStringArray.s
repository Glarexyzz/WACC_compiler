.data
// length of .L.str0
    .word 3
.L.str0:
    .asciz "box"
// length of .L.str2
    .word 3
.L.str2:
    .asciz "bar"
// length of .L.str1
    .word 3
.L.str1:
    .asciz "foo"
.align 4
.text
.global main
main:
    // Main/Branch prologue
    stp fp, lr, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    mov fp, sp
    mov w0, #7
    bl _malloc
    mov x16, x0
    add x16, x16, #4
    mov w8, #3
    stur w8, [x16, #-4]
    mov w8, #97
    strb w8, [x16]
    mov w8, #98
    strb w8, [x16, #1]
    mov w8, #99
    strb w8, [x16, #2]
    mov x20, x16
    mov w0, #36
    bl _malloc
    mov x16, x0
    add x16, x16, #4
    mov w8, #4
    stur w8, [x16, #-4]
    str x20, [x16]
    adrp x0, .L.str0
    add x0, x0, :lo12:.L.str0
    strb w8, [x16, #1]
    adrp x0, .L.str1
    add x0, x0, :lo12:.L.str1
    strb w8, [x16, #2]
    adrp x0, .L.str2
    add x0, x0, :lo12:.L.str2
    strb w8, [x16, #3]
    mov x19, x16
    // Main/Branch epilogue
    mov x0, #0
    ldp x19, x20, [sp], #16
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

