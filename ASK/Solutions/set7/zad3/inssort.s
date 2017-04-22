    .global insertion_sort
    .type insertion_sort, @function

    .text

first = %rdi
last = %rsi
i = %rbx
j = %r8
current = %rcx

insertion_sort:
    xor j, j                    # j = 0
    mov $1, i                   # i = 1

iter_all:
    mov i, %r9
    sub $1, %r9
    lea (,%r9, 8), %r9
    add first, %r9
    cmp %r9, last               # (first+i-1) != last ?
    jz end

    mov (first, i, 8), current  # current = *(first+i);
    mov i, j
    sub $1, j                   # j = i - 1

sort:
    test j, j
    jl after_loop               # j >= 0 ?
    mov (first, j, 8), %rdx
    cmp current, %rdx
    jle after_loop              # *(first+j) > current ?

loop:
    mov (first, j, 8), %rax     # rax = *(first+j)
    mov j, %r9
    add $1, %r9
    mov %rax, (first, %r9, 8)   # *(first+j+1) = *(first+j)
    dec j                       # j--
    jmp sort

after_loop:
    add $1, j                   # j += 1
    mov current, (first, j, 8)  # *(first+j) = current
    inc i                       # i++
    jmp iter_all

end:
    ret

.size inserton_sort, . - insertion_sort

