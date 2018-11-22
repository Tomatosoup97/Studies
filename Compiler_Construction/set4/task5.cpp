#include <iostream>

struct Integer {
    int wrap;
    ~Integer();
    /*
    ~Integer() {
        printf("~Integer: %d\n", wrap);
    }
    */
};

static void silnia(Integer x, Integer &acc) {
    if (x.wrap == 1) return;
    acc.wrap *= x.wrap--;
    silnia(x, acc);
}

int main() {
    Integer x = { 10 };
    Integer y = { 1 };
    silnia(x, y);
    printf("%u\n", y.wrap);
}

