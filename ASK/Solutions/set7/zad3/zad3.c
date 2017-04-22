#include <stdlib.h>
#include <stdio.h>


void insertion_sort(long *first, long *last);


void outputArray(int n, long tab []) {
    printf(" ");
    for (int i=0; i < n; i++) {
        printf("%ld ", tab[i]);
    }
}


int main(int argc, char **argv) {
    if (argc < 3)
        return EXIT_FAILURE;

    int n = argc - 1;
    long *tab = (long*) malloc(n * sizeof(long));

    for (int i=0; i < n; i++) {
        tab[i] = strtol(argv[i+1], NULL, 10);
    }
 
    printf("insertion_sort([");
    outputArray(n, tab);

    insertion_sort(tab, &tab[n-1]);

    printf("]) = [");
    outputArray(n, tab);
    printf("]\n");

    return EXIT_SUCCESS;
}


