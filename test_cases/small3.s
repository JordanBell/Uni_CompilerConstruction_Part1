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
doub:
	pushq	%rbp
	movq %rsp, %rbp
	# offset 0
	mov	16(%rbp), %rax
	push	%rax
### Incrementing Stack Pointer: 2
	# offset 0
	mov	16(%rbp), %rax
	push	%rax
### Incrementing Stack Pointer: 3
	pop	%rax
	pop	%rbx
	imul	%rax, %rbx
	push	%rbx
### Decrementing Stack Pointer: 2
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
	push	$0
### Incrementing Stack Pointer: 1
	push	$0
### Incrementing Stack Pointer: 2
	push	$0
### Incrementing Stack Pointer: 3
	# offset 1
	mov	-24(%rbp), %rax
	push	%rax
### Incrementing Stack Pointer: 4
	# offset 2
	mov	-32(%rbp), %rax
	push	%rax
### Incrementing Stack Pointer: 5
	pop	%rax
	pop	%rbx
	imul	%rax, %rbx
	push	%rbx
### Decrementing Stack Pointer: 4
	# offset 3
	mov	-40(%rbp), %rax
	push	%rax
### Incrementing Stack Pointer: 5
	pop	%rax
	pop	%rbx
	imul	%rax, %rbx
	push	%rbx
### Decrementing Stack Pointer: 4
	# offset 4
	mov	-48(%rbp), %rax
	push	%rax
### Incrementing Stack Pointer: 5
	call	doub
	push	%rax
	# write offset 4
	pop	%rax
	mov	%rax, -48(%rbp)
	push	-48(%rbp)
	# offset 4
	mov	-48(%rbp), %rax
	push	%rax
### Incrementing Stack Pointer: 6
	pop	-4(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, %edi
	call	print
	push	$3
### Incrementing Stack Pointer: 7
	pop	-4(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, %edi
	call	print
	# offset 4
	mov	-48(%rbp), %rax
	push	%rax
### Incrementing Stack Pointer: 8
	pop	%rax
	pop	%rbx
	push	%rax
	pop	%rax
	pop	%rbx
	push	%rax
	pop	%rax
	pop	%rbx
	push	%rax
	pop	%rax
	pop	%rbx
	push	%rax

	# Push the top of the stack onto the output register
	pop	-4(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, %edi
	# Generated code END


	call	print

	movl	$77, %eax
	movl	%eax, %edi
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
