	.globl _main
	.p2align	2
_main:
	sub sp, sp, #32
	stp x29, x30, [sp, #16]
	add x29, sp, #16
	adrp x0, l_.str@PAGE
	add x0, x0, l_.str@PAGEOFF
	bl _printf
	str w0 , [x29, #-4]
	mov w9, #0
	mov w0, w9
	ldp x29, x30, [sp, #16]
	add sp, sp, #32
	ret

l_.str:
	 .asciz "Hello world"

