	.globl _main
	.p2align	2
_main:
	sub sp, sp, #48
	stp x29, x30, [sp, #32]
	add x29, sp, #16
	mov w9, #10
	str w9 , [x29, #-8]
	ldrsb w9 , [x29, #-20]
	cmp w9, #1
	b.eq if.0.0
	b if.0.1
if.0.0:
	adrp x0, l_.str@PAGE
	add x0, x0, l_.str@PAGEOFF
	bl _printf
	str w0 , [x29, #-4]
	ldr w9 , [x29, #-4]
	str w9 , [x29, #-16]
	b if.0.end
if.0.1:
	adrp x0, l_.str.1@PAGE
	add x0, x0, l_.str.1@PAGEOFF
	bl _printf
	str w0 , [x29, #-12]
	ldr w9 , [x29, #-12]
	str w9 , [x29, #-16]
	b if.0.end
if.0.end:
	mov w9, #0
	mov w0, w9
	ldp x29, x30, [sp, #32]
	add sp, sp, #48
	ret

l_.str.1:
	 .asciz "Hello"

l_.str:
	 .asciz "Hello world\\n"

