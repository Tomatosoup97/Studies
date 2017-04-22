#include <stdio.h>
#include <stdlib.h>


int clz(long);

int main(int argc, char **argv) {

    long x = strtol(argv[1], NULL, 10);

    printf("%d\n", clz(x));

    return 0;
}

