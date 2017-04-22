    .global lcm_gcd
    .type lcm_gcd, @function

    .text

a = %rdi
b = %rsi
gcdA = %rcx
gcdB = %rbx

lcm_gcd:
    mov a, gcdA
    mov b, gcdB

gcd:
    cmp gcdB, gcdA  #
    jz lcm          # while (a != b)
    jb else         # a > b ?

    sub gcdB, gcdA  # if so: a -= b
    jmp then

else:
    sub gcdA, gcdB  # else: b -= a

then:
    jmp gcd

lcm:
    xor %rdx, %rdx
    mov a, %rax
    imul b           # a *= b
    
    idiv gcdA        # a /= gcd
    mov gcdA, %rdx
    ret

.size lcm_gcd, . - lcm_gcd

