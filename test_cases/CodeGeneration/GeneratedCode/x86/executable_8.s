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
sum_10:
	pushq	%rbp
	movq %rsp, %rbp
	# offset 0
	mov	88(%rbp), %rax
	push	%rax
	# offset 1
	mov	80(%rbp), %rax
	push	%rax
	pop	%rax
	pop	%rbx
	add	%rax, %rbx
	push	%rbx
### Incrementing Stack Pointer: 13
	# offset 2
	mov	72(%rbp), %rax
	push	%rax
	pop	%rax
	pop	%rbx
	add	%rax, %rbx
	push	%rbx
### Incrementing Stack Pointer: 15
	# offset 3
	mov	64(%rbp), %rax
	push	%rax
	pop	%rax
	pop	%rbx
	add	%rax, %rbx
	push	%rbx
### Incrementing Stack Pointer: 17
	# offset 4
	mov	56(%rbp), %rax
	push	%rax
	pop	%rax
	pop	%rbx
	add	%rax, %rbx
	push	%rbx
### Incrementing Stack Pointer: 19
	# offset 5
	mov	48(%rbp), %rax
	push	%rax
	pop	%rax
	pop	%rbx
	add	%rax, %rbx
	push	%rbx
### Incrementing Stack Pointer: 21
	# offset 6
	mov	40(%rbp), %rax
	push	%rax
	pop	%rax
	pop	%rbx
	add	%rax, %rbx
	push	%rbx
### Incrementing Stack Pointer: 23
	# offset 7
	mov	32(%rbp), %rax
	push	%rax
	pop	%rax
	pop	%rbx
	add	%rax, %rbx
	push	%rbx
### Incrementing Stack Pointer: 25
	# offset 8
	mov	24(%rbp), %rax
	push	%rax
	pop	%rax
	pop	%rbx
	add	%rax, %rbx
	push	%rbx
### Incrementing Stack Pointer: 27
	# offset 9
	mov	16(%rbp), %rax
	push	%rax
	pop	%rax
	pop	%rbx
	add	%rax, %rbx
	push	%rbx
### Incrementing Stack Pointer: 29
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
	push	$2
	push	$3
	push	$4
	push	$5
	push	$6
	push	$7
	push	$8
	push	$9
	push	$10
	call	sum_10
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
