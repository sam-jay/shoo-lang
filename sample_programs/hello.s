	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 13
	.globl	_main                   ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%r14
	.cfi_def_cfa_offset 16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	pushq	%rax
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -24
	.cfi_offset %r14, -16
	movl	$8, %edi
	callq	_malloc
	movq	$0, (%rax)
	movl	$16, %edi
	callq	_malloc
	leaq	_main(%rip), %rcx
	movq	%rcx, (%rax)
	movq	$0, 8(%rax)
	xorl	%edi, %edi
	callq	_malloc
	movq	%rax, %rbx
	leaq	_f0(%rip), %r14
	movl	$16, %edi
	callq	_malloc
	movq	%r14, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rbx, %rdi
	callq	*%r14
	movq	%rax, %rcx
	leaq	L_fmt(%rip), %rdi
	xorl	%eax, %eax
	movq	%rcx, %rsi
	callq	_printf
	xorl	%eax, %eax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_f0                     ## -- Begin function f0
	.p2align	4, 0x90
_f0:                                    ## @f0
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	$8, %edi
	callq	_malloc
	movq	%rbx, (%rax)
	movl	$16, %edi
	callq	_malloc
	leaq	_f0(%rip), %rcx
	movq	%rbx, 8(%rax)
	movq	%rcx, (%rax)
	leaq	L_str(%rip), %rax
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_fmt:                                  ## @fmt
	.asciz	"%s\n"

L_fmt.1:                                ## @fmt.1
	.asciz	"%s\n"

L_str:                                  ## @str
	.asciz	"Hello World"


.subsections_via_symbols
