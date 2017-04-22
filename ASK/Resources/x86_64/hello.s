        .global _start

write = 1
exit  = 60

        .section .text
_start:
        # write(int fd, const void *buf, size_t count)
        mov     $write,%rax
        mov     $1,%rdi
        lea     msg,%rsi
        mov     $14,%rdx
        syscall

# exit(int status)
        mov     $exit,%rax
        mov     $1,%rdi
        syscall

        .section .rodata
msg:
        .asciz "hello, world!\n"

# vim: ts=8 sw=8 et
