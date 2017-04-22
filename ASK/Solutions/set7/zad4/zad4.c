#include <stdio.h>
#include <stdlib.h>


unsigned long fibonacci(unsigned long n);


int main(int argc, char **argv) {
  if (argc < 2)
    return EXIT_FAILURE;

  long n = strtol(argv[1], NULL, 10);

  printf("fib(%lu) = %lu\n", n, fibonacci(n));

  return EXIT_SUCCESS;
}

