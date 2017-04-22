#include <stdio.h>
#include <stdlib.h>


typedef struct {
    unsigned long lcm;
    unsigned long gcd;
} result_t;


result_t lcm_gcd(unsigned long, unsigned long);


int main(int argc, char **argv) {
    int a = atoi(argv[1]);
    int b = atoi(argv[2]);
    result_t result = lcm_gcd(a, b);

    printf("lcm: %lu, gcd: %lu\n", result.lcm, result.gcd);
    return 0;
}

