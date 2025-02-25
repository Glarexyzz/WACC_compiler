.data
// length of .L.str0
	.word 61
.L.str0:
	.asciz "This program calculates the nth fibonacci number recursively."
// length of .L.str1
	.word 42
.L.str1:
	.asciz "Please enter n (should not be too large): "
// length of .L.str2
	.word 15
.L.str2:
	.asciz "The input n is "
// length of .L.str3
	.word 28
.L.str3:
	.asciz "The nth fibonacci number is "
.align 4
.text
.global main
main:
	// push {fp, lr}
	stp fp, lr, [sp, #-16]!
	// push {x19, x20}
	stp x19, x20, [sp, #-16]!
	mov fp, sp
	adrp x0, .L.str0
	add x0, x0, :lo12:.L.str0
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	bl _println
	adrp x0, .L.str1
	add x0, x0, :lo12:.L.str1
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	mov w19, #0
	// load the current value in the destination of the read so it supports defaults
	mov w0, w19
	bl _readi
	mov w19, w0
	adrp x0, .L.str2
	add x0, x0, :lo12:.L.str2
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	mov w0, w19
	// statement primitives do not return results (but will clobber r0/rax)
	bl _printi
	bl _println
	adrp x0, .L.str3
	add x0, x0, :lo12:.L.str3
	// statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	mov w0, w19
	bl wacc_fibonacci
	mov w20, w0
	// statement primitives do not return results (but will clobber r0/rax)
	bl _printi
	bl _println
	mov x0, #0
	// pop {x19, x20}
	ldp x19, x20, [sp], #16
	// pop {fp, lr}
	ldp fp, lr, [sp], #16
	ret

wacc_fibonacci:
	// push {fp, lr}
	stp fp, lr, [sp, #-16]!
	// push {x19, x20}
	stp x19, x20, [sp, #-16]!
	mov fp, sp
	cmp w0, #1
	b.le .L0
	b .L1
.L0:
	// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
	mov sp, fp
	// pop {x19, x20}
	ldp x19, x20, [sp], #16
	// pop {fp, lr}
	ldp fp, lr, [sp], #16
	ret
.L1:
	// push {x0}
	stp x0, xzr, [sp, #-16]!
	// Set up X16 as a temporary second base pointer for the caller saved things
	mov x16, sp
	subs w0, w0, #1
	b.vs _errOverflow
	bl wacc_fibonacci
	mov w16, w0
	// pop {x0}
	ldp x0, xzr, [sp], #16
	mov w19, w16
	// push {x0}
	stp x0, xzr, [sp, #-16]!
	// Set up X16 as a temporary second base pointer for the caller saved things
	mov x16, sp
	subs w0, w0, #2
	b.vs _errOverflow
	bl wacc_fibonacci
	mov w16, w0
	// pop {x0}
	ldp x0, xzr, [sp], #16
	mov w20, w16
	adds w0, w19, w20
	b.vs _errOverflow
	// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
	mov sp, fp
	// pop {x19, x20}
	ldp x19, x20, [sp], #16
	// pop {fp, lr}
	ldp fp, lr, [sp], #16
	ret
	// 'ere be dragons: this is 100% dead code, functions always end in returns!

// length of .L._printi_str0
	.word 2
.L._printi_str0:
	.asciz "%d"
.align 4
_printi:
	// push {lr}
	stp lr, xzr, [sp, #-16]!
	mov x1, x0
	adr x0, .L._printi_str0
	bl printf
	mov x0, #0
	bl fflush
	// pop {lr}
	ldp lr, xzr, [sp], #16
	ret

// length of .L._prints_str0
	.word 4
.L._prints_str0:
	.asciz "%.*s"
.align 4
_prints:
	// push {lr}
	stp lr, xzr, [sp, #-16]!
	mov x2, x0
	ldur w1, [x0, #-4]
	adr x0, .L._prints_str0
	bl printf
	mov x0, #0
	bl fflush
	// pop {lr}
	ldp lr, xzr, [sp], #16
	ret

// length of .L._println_str0
	.word 0
.L._println_str0:
	.asciz ""
.align 4
_println:
	// push {lr}
	stp lr, xzr, [sp, #-16]!
	adr x0, .L._println_str0
	bl puts
	mov x0, #0
	bl fflush
	// pop {lr}
	ldp lr, xzr, [sp], #16
	ret

// length of .L._readi_str0
	.word 2
.L._readi_str0:
	.asciz "%d"
.align 4
_readi:
	// X0 contains the "original" value of the destination of the read
	// allocate space on the stack to store the read: preserve alignment!
	// the passed default argument should be stored in case of EOF
	// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
	// push {x0, lr}
	stp x0, lr, [sp, #-16]!
	mov x1, sp
	adr x0, .L._readi_str0
	bl scanf
	// pop {x0, lr}
	ldp x0, lr, [sp], #16
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
