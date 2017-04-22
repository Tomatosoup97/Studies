  .global clz
  .type clz,@function

  .section .text

number = %rdi
result = %rax
half = %rcx

clz:
    xor result, result
    mov $1, half
    sal $5, half

count:
    test half, half
    jz end
    mov number, %rbx
    sar %cl, %rbx
    test %rbx, %rbx
    jz else

    mov %rbx, number
    jmp et

else:
    add half, result

et:
    sar $1, half
    jmp count

end:
    ret

.size clz, . - clz

