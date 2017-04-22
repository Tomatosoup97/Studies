    .global fibonacci
    .type fibonacci, @function
    .text

n = %rdi

fibonacci:
    cmp $2, n        # n < 2 ?
    jae reccall         # if not, do recursive call

    mov $1, %rax        # else return 1
    ret

reccall:

    push %rbp
    push %rbx
    sub $8, %rsp        # align stack
    mov n, %rbx

    dec n

    call fibonacci

    mov %rax, %rbp
    lea -2(%rbx), n  # n - 2

    call fibonacci
    add %rbp, %rax
    add $8, %rsp        # resume previously aligned stack
    pop %rbx
    pop %rbp
    ret

.size fibonacci, . - fibonacci
