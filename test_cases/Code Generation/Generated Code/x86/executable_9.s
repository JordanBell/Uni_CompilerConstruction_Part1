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
### Incrementing Stack Pointer: 2
	push	$2
### Incrementing Stack Pointer: 3
	pop	%rax
	pop	%rbx
	sub	%rax, %rbx
	mov	$0, %rax
	cmp	%rax, %rbx
	jle	.BOP0
	push	$0
	jmp	.CONT_BOP0
.BOP0:
	push	$1
.CONT_BOP0:
### Incrementing Stack Pointer: 4
	pop	%rax
	mov	$0, %rbx
	cmp	%rax, %rbx
	jne	.IE0
	# offset 0
	mov	16(%rbp), %rax
	push	%rax
### Incrementing Stack Pointer: 5
	push	$1
### Incrementing Stack Pointer: 6
	pop	%rax
	pop	%rbx
	sub	%rax, %rbx
	push	%rbx
### Incrementing Stack Pointer: 7
	# offset 0
	mov	16(%rbp), %rax
	push	%rax
### Incrementing Stack Pointer: 8
	# offset 7
	mov	-40(%rbp), %rax
	push	%rax
### Incrementing Stack Pointer: 9
	call	factorial
	push	%rax
	pop	%rax
	pop	%rbx
	imul	%rax, %rbx
	push	%rbx
### Incrementing Stack Pointer: 10
	pop	%rax
	pop	%rbx
	push	%rax
	jmp	.CONT_IE0
.IE0:
	# offset 0
	mov	16(%rbp), %rax
	push	%rax
### Incrementing Stack Pointer: 11
.CONT_IE0:
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
	push	$5
### Incrementing Stack Pointer: 1
	call	factorial
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
