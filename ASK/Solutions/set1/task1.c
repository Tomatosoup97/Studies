#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

void dec_to_binary(int k) {
    char result[32] = {0};
    int i=31;
    while (k != 0) {
        result[i--] = k%2;
        k /= 2;
    }
    for (int j=i+1; j < 32; j++)
        printf("%d", result[j]);
    printf("\n");
}

int main() {
    uint32_t i, k, x;
    uint32_t i_bit;
    printf("Pass i, k, x: ");
    scanf("%d%d%d", &i, &k, &x);

    dec_to_binary(x);

    i_bit = (x & (1 << i-1));
    x &= (~(1 << k-1));
    dec_to_binary(x);
    printf("i bit: ");
    dec_to_binary(i_bit);
    x |= i_bit << k-i;

    printf("copy %d bit on %d position: \n", i, k);
    dec_to_binary(x);

    return 0;
}
