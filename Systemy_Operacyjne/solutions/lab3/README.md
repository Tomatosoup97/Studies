# Memory manager in C

Library providing basic memory management by implementing:
- posix_memalign
- malloc
- calloc
- realloc
- free

Look into `malloc(3)`, `posix_memalign(3)` for more details


### Compile to shared `libmalloc.so` library

    make

### Use instead of malloc(3)

    export LD_PRELOAD=/path/to/libmalloc.so

Exemplary working UNIX programs under this implementation:

- `cat`, `tail`,`ls`, `mkdir`, `rmdir`, `cp`, `pwd`, `touch`, `echo`

### Testing

    make run_tests

