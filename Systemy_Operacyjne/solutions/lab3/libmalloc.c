#include <unistd.h>
#include "mem_mgmt.h"

void *malloc(size_t size){
    return foo_malloc(size);
}

void *calloc(size_t count, size_t size){
    return foo_calloc(count, size);
}

void *realloc(void *ptr, size_t size){
    return foo_realloc(ptr, size);
}

int posix_memalign(void **memptr, size_t alignment, size_t size){
    return foo_posix_memalign(memptr, alignment, size);
}

void free(void *ptr){
    foo_free(ptr);
}
