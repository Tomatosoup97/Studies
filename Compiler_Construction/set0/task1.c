void set_row(long *a, long *b, long i, long n) {
    for (long j=0; j<n; j++)
        a[n*i+j] = b[j];
}

/* Improved */
void set_row2(long *a, long *b, long i, long n) {
    long *as = &a[n*i];
    for (long j=0; j<n; j++)
        *(as + j) = b[j];
}

