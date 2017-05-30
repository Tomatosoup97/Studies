	.file	"common.c"
	.section	.rodata
.LC0:
	.string	"Failure: "
	.text
	.globl	fail
	.type	fail, @function
fail:
.LFB5:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$224, %rsp
	movq	%rdi, -216(%rbp)
	movq	%rsi, -168(%rbp)
	movq	%rdx, -160(%rbp)
	movq	%rcx, -152(%rbp)
	movq	%r8, -144(%rbp)
	movq	%r9, -136(%rbp)
	testb	%al, %al
	je	.L3
	movaps	%xmm0, -128(%rbp)
	movaps	%xmm1, -112(%rbp)
	movaps	%xmm2, -96(%rbp)
	movaps	%xmm3, -80(%rbp)
	movaps	%xmm4, -64(%rbp)
	movaps	%xmm5, -48(%rbp)
	movaps	%xmm6, -32(%rbp)
	movaps	%xmm7, -16(%rbp)
.L3:
	movl	$8, -200(%rbp)
	movl	$48, -196(%rbp)
	leaq	16(%rbp), %rax
	movq	%rax, -192(%rbp)
	leaq	-176(%rbp), %rax
	movq	%rax, -184(%rbp)
	movq	stderr(%rip), %rax
	movq	%rax, %rcx
	movl	$9, %edx
	movl	$1, %esi
	movl	$.LC0, %edi
	call	fwrite
	movq	stderr(%rip), %rax
	leaq	-200(%rbp), %rdx
	movq	-216(%rbp), %rcx
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	vfprintf
	movq	stderr(%rip), %rax
	movq	%rax, %rsi
	movl	$10, %edi
	call	fputc
	movl	$1, %edi
	call	exit
	.cfi_endproc
.LFE5:
	.size	fail, .-fail
	.local	__state
	.comm	__state,8,8
	.globl	__init_fast_random
	.type	__init_fast_random, @function
__init_fast_random:
.LFB6:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$0, %edi
	call	time
	movq	%rax, __state(%rip)
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE6:
	.size	__init_fast_random, .-__init_fast_random
	.section	.init_array,"aw"
	.align 8
	.quad	__init_fast_random
	.text
	.globl	fast_srandom
	.type	fast_srandom, @function
fast_srandom:
.LFB7:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, __state(%rip)
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE7:
	.size	fast_srandom, .-fast_srandom
	.globl	fast_random
	.type	fast_random, @function
fast_random:
.LFB8:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	__state(%rip), %rax
	shrq	$12, %rax
	movq	%rax, %rdx
	movq	__state(%rip), %rax
	xorq	%rdx, %rax
	movq	%rax, __state(%rip)
	movq	__state(%rip), %rax
	salq	$25, %rax
	movq	%rax, %rdx
	movq	__state(%rip), %rax
	xorq	%rdx, %rax
	movq	%rax, __state(%rip)
	movq	__state(%rip), %rax
	shrq	$27, %rax
	movq	%rax, %rdx
	movq	__state(%rip), %rax
	xorq	%rdx, %rax
	movq	%rax, __state(%rip)
	movq	__state(%rip), %rdx
	movabsq	$2685821657736338717, %rax
	imulq	%rdx, %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE8:
	.size	fast_random, .-fast_random
	.section	.rodata
	.align 8
.LC1:
	.string	"Failed to allocate cache flush buffer!"
	.text
	.globl	flush_cache
	.type	flush_cache, @function
flush_cache:
.LFB9:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	$0, -8(%rbp)
	call	getpagesize
	movslq	%eax, %rcx
	leaq	-8(%rbp), %rax
	movl	$16777216, %edx
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	posix_memalign
	movq	-8(%rbp), %rax
	testq	%rax, %rax
	jne	.L9
	movl	$.LC1, %edi
	movl	$0, %eax
	call	fail
.L9:
	movq	-8(%rbp), %rax
	movl	$16777216, %esi
	movq	%rax, %rdi
	call	bzero
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	free
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE9:
	.size	flush_cache, .-flush_cache
	.globl	timer_reset
	.type	timer_reset, @function
timer_reset:
.LFB10:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movl	$32, %edx
	movl	$0, %esi
	movq	%rax, %rdi
	call	memset
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE10:
	.size	timer_reset, .-timer_reset
	.globl	timer_start
	.type	timer_start, @function
timer_start:
.LFB11:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movl	$0, %esi
	movq	%rax, %rdi
	call	gettimeofday
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE11:
	.size	timer_start, .-timer_start
	.globl	timer_stop
	.type	timer_stop, @function
timer_stop:
.LFB12:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movq	%rdi, -40(%rbp)
	leaq	-16(%rbp), %rax
	movl	$0, %esi
	movq	%rax, %rdi
	call	gettimeofday
	movq	-16(%rbp), %rdx
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	subq	%rax, %rdx
	movq	%rdx, %rax
	movq	%rax, -32(%rbp)
	movq	-8(%rbp), %rdx
	movq	-40(%rbp), %rax
	movq	8(%rax), %rax
	subq	%rax, %rdx
	movq	%rdx, %rax
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	testq	%rax, %rax
	jns	.L13
	movq	-32(%rbp), %rax
	subq	$1, %rax
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rax
	addq	$1000000, %rax
	movq	%rax, -24(%rbp)
.L13:
	movq	-40(%rbp), %rax
	movq	16(%rax), %rdx
	movq	-32(%rbp), %rax
	addq	%rax, %rdx
	movq	-40(%rbp), %rax
	movq	%rdx, 16(%rax)
	movq	-40(%rbp), %rax
	movq	24(%rax), %rdx
	movq	-24(%rbp), %rax
	addq	%rax, %rdx
	movq	-40(%rbp), %rax
	movq	%rdx, 24(%rax)
	movq	-40(%rbp), %rax
	movq	24(%rax), %rax
	cmpq	$999999, %rax
	jle	.L15
	movq	-40(%rbp), %rax
	movq	16(%rax), %rax
	leaq	1(%rax), %rdx
	movq	-40(%rbp), %rax
	movq	%rdx, 16(%rax)
	movq	-40(%rbp), %rax
	movq	24(%rax), %rax
	leaq	-1000000(%rax), %rdx
	movq	-40(%rbp), %rax
	movq	%rdx, 24(%rax)
.L15:
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE12:
	.size	timer_stop, .-timer_stop
	.section	.rodata
	.align 8
.LC2:
	.string	"Time elapsed: %ld.%06ld seconds.\n"
	.text
	.globl	timer_print
	.type	timer_print, @function
timer_print:
.LFB13:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	24(%rax), %rdx
	movq	-8(%rbp), %rax
	movq	16(%rax), %rax
	movq	%rax, %rsi
	movl	$.LC2, %edi
	movl	$0, %eax
	call	printf
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE13:
	.size	timer_print, .-timer_print
	.ident	"GCC: (GNU) 6.3.1 20170306"
	.section	.note.GNU-stack,"",@progbits
