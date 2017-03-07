#include "stdio.h"

int compare(x, y) {
    int b = ((x-y)^((x^y)&((x-y)^x))) >> 31;
    return b;
}

int main() {
    int x=10, y=12;
    printf("%d\n", compare(x, y));
    x = 14;
    y = 9;
    printf("%d\n", compare(x, y));
    x = 30;
    y = 2;
    printf("%d\n", compare(x, y));
    x = 1;
    y = 150;
    printf("%d\n", compare(x, y));
}

