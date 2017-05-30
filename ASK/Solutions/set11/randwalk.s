	.file	"randwalk.c"
	.text
	.globl	fill
	.type	fill, @function
fill:
.LFB5:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -40(%rbp)
	movl	%esi, -44(%rbp)
	movl	$0, -20(%rbp)
	jmp	.L2
.L3:
	movl	-20(%rbp), %eax
	movslq	%eax, %rdx
	movq	-40(%rbp), %rax
	leaq	(%rdx,%rax), %rbx
	call	rand
	movb	%al, (%rbx)
	addl	$1, -20(%rbp)
.L2:
	movl	-44(%rbp), %eax
	imull	-44(%rbp), %eax
	cmpl	-20(%rbp), %eax
	jg	.L3
	nop
	addq	$40, %rsp
	popq	%rbx
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE5:
	.size	fill, .-fill
	.globl	randwalk1
	.type	randwalk1, @function
randwalk1:
.LFB6:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movq	%rdi, -40(%rbp)
	movl	%esi, -44(%rbp)
	movl	%edx, -48(%rbp)
	movl	$0, -12(%rbp)
	movl	$0, -16(%rbp)
	movq	$0, -24(%rbp)
	movl	$0, %eax
	call	fast_random
	movq	%rax, %rdx
	movl	-44(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rdx, %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	movl	%eax, -4(%rbp)
	movl	$0, %eax
	call	fast_random
	movq	%rax, %rdx
	movl	-44(%rbp), %eax
	movslq	%eax, %rcx
	movq	%rdx, %rax
	movl	$0, %edx
	divq	%rcx
	movq	%rdx, %rax
	movl	%eax, -8(%rbp)
.L15:
	subl	$2, -12(%rbp)
	cmpl	$0, -12(%rbp)
	jns	.L5
	movl	$62, -12(%rbp)
	movl	$0, %eax
	call	fast_random
	movq	%rax, -24(%rbp)
.L5:
	movl	-4(%rbp), %eax
	imull	-44(%rbp), %eax
	movl	%eax, %edx
	movl	-8(%rbp), %eax
	addl	%edx, %eax
	movslq	%eax, %rdx
	movq	-40(%rbp), %rax
	addq	%rdx, %rax
	movzbl	(%rax), %eax
	movzbl	%al, %eax
	addl	%eax, -16(%rbp)
	movl	-12(%rbp), %eax
	movq	-24(%rbp), %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	andl	$3, %eax
	cmpq	$1, %rax
	je	.L7
	cmpq	$1, %rax
	jb	.L8
	cmpq	$2, %rax
	je	.L9
	cmpq	$3, %rax
	je	.L10
	jmp	.L6
.L8:
	cmpl	$0, -4(%rbp)
	jle	.L17
	subl	$1, -4(%rbp)
	jmp	.L17
.L7:
	movl	-44(%rbp), %eax
	subl	$1, %eax
	cmpl	-4(%rbp), %eax
	jle	.L18
	addl	$1, -4(%rbp)
	jmp	.L18
.L9:
	cmpl	$0, -8(%rbp)
	jle	.L19
	subl	$1, -8(%rbp)
	jmp	.L19
.L10:
	movl	-44(%rbp), %eax
	subl	$1, %eax
	cmpl	-8(%rbp), %eax
	jle	.L20
	addl	$1, -8(%rbp)
	jmp	.L20
.L17:
	nop
	jmp	.L6
.L18:
	nop
	jmp	.L6
.L19:
	nop
	jmp	.L6
.L20:
	nop
.L6:
	subl	$1, -48(%rbp)
	cmpl	$0, -48(%rbp)
	jne	.L15
	movl	-16(%rbp), %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE6:
	.size	randwalk1, .-randwalk1
	.globl	randwalk2
	.type	randwalk2, @function
randwalk2:
.LFB7:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$64, %rsp
	movq	%rdi, -56(%rbp)
	movl	%esi, -60(%rbp)
	movl	%edx, -64(%rbp)
	movl	$0, -12(%rbp)
	movl	$0, -16(%rbp)
	movq	$0, -24(%rbp)
	movl	$0, %eax
	call	fast_random
	movl	%eax, %edx
	movl	-60(%rbp), %eax
	subl	$1, %eax
	andl	%edx, %eax
	movl	%eax, -4(%rbp)
	movl	$0, %eax
	call	fast_random
	movl	%eax, %edx
	movl	-60(%rbp), %eax
	subl	$1, %eax
	andl	%edx, %eax
	movl	%eax, -8(%rbp)
.L23:
	subl	$2, -12(%rbp)
	cmpl	$0, -12(%rbp)
	jns	.L22
	movl	$62, -12(%rbp)
	movl	$0, %eax
	call	fast_random
	movq	%rax, -24(%rbp)
.L22:
	movl	-4(%rbp), %eax
	imull	-60(%rbp), %eax
	movl	%eax, %edx
	movl	-8(%rbp), %eax
	addl	%edx, %eax
	movslq	%eax, %rdx
	movq	-56(%rbp), %rax
	addq	%rdx, %rax
	movzbl	(%rax), %eax
	movzbl	%al, %eax
	addl	%eax, -16(%rbp)
	movl	-12(%rbp), %eax
	movq	-24(%rbp), %rdx
	movl	%eax, %ecx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	andl	$3, %eax
	movq	%rax, -32(%rbp)
	movl	-60(%rbp), %eax
	subl	$1, %eax
	movl	%eax, -36(%rbp)
	movq	-32(%rbp), %rax
	andl	$1, %eax
	movl	-8(%rbp), %edx
	cmpl	-36(%rbp), %edx
	setl	%dl
	andl	%edx, %eax
	movzbl	%al, %edx
	movl	-8(%rbp), %eax
	addl	%edx, %eax
	movl	%eax, -8(%rbp)
	cmpq	$1, -32(%rbp)
	sete	%dl
	movl	-4(%rbp), %eax
	cmpl	-36(%rbp), %eax
	setl	%al
	andl	%edx, %eax
	movzbl	%al, %edx
	cmpq	$0, -32(%rbp)
	sete	%cl
	cmpl	$0, -4(%rbp)
	setg	%al
	andl	%ecx, %eax
	movzbl	%al, %eax
	subl	%eax, %edx
	movl	%edx, %eax
	addl	%eax, -4(%rbp)
	subl	$1, -64(%rbp)
	cmpl	$0, -64(%rbp)
	jne	.L23
	movl	-16(%rbp), %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE7:
	.size	randwalk2, .-randwalk2
	.section	.rodata
.LC0:
	.string	"n:s:t:v:"
	.align 8
.LC1:
	.string	"Usage: %s -n log2(size) -l log2(length) -t log2(times) -v variant\n"
	.align 8
.LC2:
	.string	"Generate matrix %d x %d (%d KiB)\n"
	.align 8
.LC3:
	.string	"Performing %d random walks of %d steps.\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB8:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$104, %rsp
	.cfi_offset 3, -24
	movl	%edi, -100(%rbp)
	movq	%rsi, -112(%rbp)
	movl	$-1, -20(%rbp)
	movl	$-1, -24(%rbp)
	movl	$-1, -28(%rbp)
	movl	$-1, -32(%rbp)
	movb	$0, -33(%rbp)
	jmp	.L26
.L31:
	cmpl	$110, -44(%rbp)
	jne	.L27
	movq	optarg(%rip), %rax
	movq	%rax, %rdi
	call	atoi
	movl	$1, %edx
	movl	%eax, %ecx
	sall	%cl, %edx
	movl	%edx, %eax
	movl	%eax, -20(%rbp)
	jmp	.L26
.L27:
	cmpl	$115, -44(%rbp)
	jne	.L28
	movq	optarg(%rip), %rax
	movq	%rax, %rdi
	call	atoi
	movl	$1, %edx
	movl	%eax, %ecx
	sall	%cl, %edx
	movl	%edx, %eax
	movl	%eax, -24(%rbp)
	jmp	.L26
.L28:
	cmpl	$116, -44(%rbp)
	jne	.L29
	movq	optarg(%rip), %rax
	movq	%rax, %rdi
	call	atoi
	movl	$1, %edx
	movl	%eax, %ecx
	sall	%cl, %edx
	movl	%edx, %eax
	movl	%eax, -28(%rbp)
	jmp	.L26
.L29:
	cmpl	$118, -44(%rbp)
	jne	.L30
	movq	optarg(%rip), %rax
	movq	%rax, %rdi
	call	atoi
	movl	%eax, -32(%rbp)
	jmp	.L26
.L30:
	movb	$1, -33(%rbp)
.L26:
	movq	-112(%rbp), %rcx
	movl	-100(%rbp), %eax
	movl	$.LC0, %edx
	movq	%rcx, %rsi
	movl	%eax, %edi
	call	getopt
	movl	%eax, -44(%rbp)
	cmpl	$-1, -44(%rbp)
	jne	.L31
	cmpb	$0, -33(%rbp)
	jne	.L32
	cmpl	$0, -20(%rbp)
	js	.L32
	cmpl	$0, -24(%rbp)
	js	.L32
	cmpl	$0, -28(%rbp)
	js	.L32
	cmpl	$0, -32(%rbp)
	js	.L32
	cmpl	$1, -32(%rbp)
	jle	.L33
.L32:
	movq	-112(%rbp), %rax
	movq	(%rax), %rdx
	movq	stderr(%rip), %rax
	movl	$.LC1, %esi
	movq	%rax, %rdi
	movl	$0, %eax
	call	fprintf
	movl	$1, %edi
	call	exit
.L33:
	movq	$0, -56(%rbp)
	movl	-20(%rbp), %eax
	imull	-20(%rbp), %eax
	movslq	%eax, %rbx
	call	getpagesize
	movslq	%eax, %rcx
	leaq	-56(%rbp), %rax
	movq	%rbx, %rdx
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	posix_memalign
	movl	-20(%rbp), %eax
	imull	-20(%rbp), %eax
	sarl	$10, %eax
	movl	%eax, %ecx
	movl	-20(%rbp), %edx
	movl	-20(%rbp), %eax
	movl	%eax, %esi
	movl	$.LC2, %edi
	movl	$0, %eax
	call	printf
	movq	-56(%rbp), %rax
	movl	-20(%rbp), %edx
	movl	%edx, %esi
	movq	%rax, %rdi
	call	fill
	movl	$0, %eax
	call	flush_cache
	movl	-24(%rbp), %edx
	movl	-28(%rbp), %eax
	movl	%eax, %esi
	movl	$.LC3, %edi
	movl	$0, %eax
	call	printf
	leaq	-96(%rbp), %rax
	movq	%rax, %rdi
	call	timer_reset
	leaq	-96(%rbp), %rax
	movq	%rax, %rdi
	call	timer_start
	movl	$0, -40(%rbp)
	jmp	.L34
.L37:
	cmpl	$0, -32(%rbp)
	jne	.L35
	movq	-56(%rbp), %rax
	movl	-24(%rbp), %edx
	movl	-20(%rbp), %ecx
	movl	%ecx, %esi
	movq	%rax, %rdi
	call	randwalk1
	jmp	.L36
.L35:
	movq	-56(%rbp), %rax
	movl	-24(%rbp), %edx
	movl	-20(%rbp), %ecx
	movl	%ecx, %esi
	movq	%rax, %rdi
	call	randwalk2
.L36:
	addl	$1, -40(%rbp)
.L34:
	movl	-40(%rbp), %eax
	cmpl	-28(%rbp), %eax
	jl	.L37
	leaq	-96(%rbp), %rax
	movq	%rax, %rdi
	call	timer_stop
	leaq	-96(%rbp), %rax
	movq	%rax, %rdi
	call	timer_print
	movq	-56(%rbp), %rax
	movq	%rax, %rdi
	call	free
	movl	$0, %eax
	addq	$104, %rsp
	popq	%rbx
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE8:
	.size	main, .-main
	.ident	"GCC: (GNU) 6.3.1 20170306"
	.section	.note.GNU-stack,"",@progbits
