_Helloworl.main_
	stp x29, x30, [sp, #2]
	sub sp, sp, #4
main:
	adrp x0, l_.str
	add x0, x0, l_.str
	bl _printf
	str w0 , [x30, #-4]
	mov w9, #0
	mov w0, w9
	ldp x29, x30, [sp, #2]
	add sp, sp, #4
	ret

l_.str:
	 .asciz "Hello world"

