#include <stdio.h>

typedef struct {
    int wrap;
    ~Integer();
} Integer;

static void silnia(Integer x, Integer *acc) {
    if (x.wrap == 1) return;
    acc->wrap *= x.wrap--;
    silnia(x, acc);
}

int main() {
    Integer x = { 10 };
    Integer y = { 1 };
    silnia(x, &y);
    printf("%u\n", y.wrap);

    /*
    Integer a = { 10 };
    Integer b = { 1 };
    silnia(a, &b);
    printf("%u\n", b.wrap);
    */
}

