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
main:
.LFB3:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	$260, -4(%rbp)
	## We will put our assembly code here. Store the value to print in the %edi register

	# movl	$246, -8(%rbp)
	# movl	$357, -4(%rbp)
	# movl	-8(%rbp), %eax
	# movl	%eax, %edi
	# call	print
	# movl	-4(%rbp), %eax
	# movl	%eax, %edi
	# call	print
	# movl	$1, %eax

	# movl	-8(%rbp), %eax
	# movl	%eax, %esi
	# movl	$.LC0, %edi
	# movl	$0, %eax
	# call	printf
	# movl	-4(%rbp), %eax
	# movl	%eax, %esi
	# movl	$.LC0, %edi
	# movl	$0, %eax
	# call	printf

	# Generated code START
	push	$246
### Incrementing Stack Pointer: 1
	push	$357
### Incrementing Stack Pointer: 2
	# offset 1
	movl	-24(%rbp), %eax
	movl	%eax, %esi
	movl	$.LC0, %edi
	movl	$0, %eax
	call	printf

	# offset 2
	movl	-32(%rbp), %eax
	movl	%eax, %esi
	movl	$.LC0, %edi
	movl	$0, %eax
	call	printf

### Decrementing Stack Pointer: 2
	push	$1
### Incrementing Stack Pointer: 3
	pop	%rax
	pop	%rbx
	push	%rax
### Decrementing Stack Pointer: 2
	pop	%rax
	pop	%rbx
	push	%rax
### Decrementing Stack Pointer: 1

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
