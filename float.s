	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 13, 0	sdk_version 13, 3
	.globl	_test                           ## -- Begin function test
	.p2align	4, 0x90
_test:                                  ## @test
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movss	%xmm0, -4(%rbp)
	movss	-4(%rbp), %xmm0                 ## xmm0 = mem[0],zero,zero,zero
	cvtss2sd	%xmm0, %xmm0
	leaq	L_.str(%rip), %rdi
	movb	$1, %al
	callq	_printf
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_testd                          ## -- Begin function testd
	.p2align	4, 0x90
_testd:                                 ## @testd
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movsd	%xmm0, -8(%rbp)
	movsd	-8(%rbp), %xmm0                 ## xmm0 = mem[0],zero
	leaq	L_.str(%rip), %rdi
	movb	$1, %al
	callq	_printf
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3                               ## -- Begin function main
LCPI2_0:
	.quad	0x40095c28f5c28f5c              ## double 3.1699999999999999
	.section	__TEXT,__literal4,4byte_literals
	.p2align	2
LCPI2_1:
	.long	0x4048f5c3                      ## float 3.1400001
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movss	LCPI2_1(%rip), %xmm0            ## xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, -4(%rbp)
	movsd	LCPI2_0(%rip), %xmm0            ## xmm0 = mem[0],zero
	movsd	%xmm0, -16(%rbp)
	movss	-4(%rbp), %xmm0                 ## xmm0 = mem[0],zero,zero,zero
	cvtss2sd	%xmm0, %xmm0
	movsd	-16(%rbp), %xmm1                ## xmm1 = mem[0],zero
	leaq	L_.str.1(%rip), %rdi
	movl	$10, %esi
	movb	$2, %al
	callq	_printf
	movsd	-16(%rbp), %xmm0                ## xmm0 = mem[0],zero
	callq	_testd
	movss	-4(%rbp), %xmm0                 ## xmm0 = mem[0],zero,zero,zero
	callq	_test
	xorl	%eax, %eax
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"%f"

L_.str.1:                               ## @.str.1
	.asciz	"%d, %f, %f\n"

.subsections_via_symbols
