	.file	"templ.c"
	.section	.rodata
.LC0:
	.string	"%d\n"
	.text
	.globl	print
	.type	print, @function
print:
.LFB2:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, %esi
	movl	$.LC0, %edi
	movl	$0, %eax
	call	printf
	movl	$0, %edi
	call	exit
	.cfi_endproc
.LFE2:
	.size	print, .-print
	.globl	main
	.type	main, @function
factorial:
	pushq	%rbp
	movq %rsp, %rbp
	# offset 0
	mov	16(%rbp), %rax
	push	%rax
	push	$1
	pop	%rax
	pop	%rbx
	sub	%rax, %rbx
	mov	$0, %rax
	cmp	%rax, %rbx
	je	.BOP0
	push	$0
	jmp	.CONT_BOP0
.BOP0:
	push	$1
.CONT_BOP0:
	pop	%rax
	mov	$0, %rbx
	cmp	%rax, %rbx
	jne	.IE0
### If false: 
	# offset 0
	mov	16(%rbp), %rax
	push	%rax
	# offset 0
	mov	16(%rbp), %rax
	push	%rax
	push	$1
	pop	%rax
	pop	%rbx
	sub	%rax, %rbx
	push	%rbx
	call	factorial
### Discard the arguments that were pushed onto the stack. Num arguments discarded: 1
	pop	%rbx
	push	%rax
### Stack Pointer: 3
	pop	%rax
	pop	%rbx
	imul	%rax, %rbx
	push	%rbx
	jmp	.CONT_IE0
### If true: 
.IE0:
	push	$1
.CONT_IE0:
	popq	%rax
	movq %rbp, %rsp
	popq	%rbp
	ret
foo:
	pushq	%rbp
	movq %rsp, %rbp
	push	$5
	# offset 0
	mov	16(%rbp), %rax
	push	%rax
	pop	%rax
	pop	%rbx
	add	%rax, %rbx
	push	%rbx
	popq	%rax
	movq %rbp, %rsp
	popq	%rbp
	ret
main:
.LFB3:
	.cfi_startproc
	pushq	%rbp									## Frame used by the call of the call to main. "This is where we're starting the stack now. Everything between the stack pointer and the base pointer is our stack"
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	$260, -4(%rbp)
	## We will put our assembly code here. Store the value to print in the %edi register


	# Generated code START
	push	$1
	push	$5
	call	foo
### Discard the arguments that were pushed onto the stack. Num arguments discarded: 1
	pop	%rbx
	push	%rax
### Stack Pointer: 2
	# offset 2
	mov	-32(%rbp), %rax
	push	%rax
	call	factorial
### Discard the arguments that were pushed onto the stack. Num arguments discarded: 1
	pop	%rbx
	push	%rax
### Stack Pointer: 3
	# offset 3
	mov	-40(%rbp), %rax
	push	%rax
# Let
	pop	%rax
	pop	%rbx
	push	%rax
# Let
	pop	%rax
	pop	%rbx
	push	%rax
# Let
	pop	%rax
	pop	%rbx
	push	%rax

	# Push the top of the stack onto the output register
	pop	-4(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, %edi
	# Generated code END


	call	print
	movl	$1, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE3:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 5.4.0-6ubuntu1~16.04.2) 5.4.0 20160609"
	.section	.note.GNU-stack,"",@progbits
