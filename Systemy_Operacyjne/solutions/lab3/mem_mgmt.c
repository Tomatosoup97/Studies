#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include "queue.h"
#include "mem_mgmt.h"

mem_ctl_t mem_ctl = {
    LIST_HEAD_INITIALIZER(mem_ctl.ma_chunks),
    PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP,
};

void *my_malloc(size_t size) {
    void *ptr;
    int res;

    pthread_mutex_lock(&mem_ctl.mutex);

    res = posix_memalign(&ptr, WORDSIZE, size);

    if (res == ENOMEM) {
        errno = ENOMEM;
    }

    pthread_mutex_unlock(&mem_ctl.mutex);
    return ptr;
}

void *calloc(size_t count, size_t size) {
    // allocate mem for an array of *count* elements of *size* bytes each
    void *ptr;
    int res;

    pthread_mutex_lock(&mem_ctl.mutex);

    res = posix_memalign(&ptr, WORDSIZE, count * size);

    if (res == ENOMEM) {
        errno = ENOMEM;
    }

    pthread_mutex_unlock(&mem_ctl.mutex);
    return ptr;
}

void *realloc(void *ptr, size_t size) {
    // change size of em block pointed to by *ptr*
    // if ptr == NULL: return malloc(size)
    // if size == 0: free(ptr)
    if (size == 0) {
        free(ptr);
        return NULL;
    }
    if (ptr == NULL) {
        return malloc(size);
    }
    pthread_mutex_lock(&mem_ctl.mutex);

    // TODO

    pthread_mutex_unlock(&mem_ctl.mutex);
    return NULL;
}

void free(void *ptr) {
    // free memory space pointed by *ptr*
    // undefined behaviour on double free
    // if ptr == NULL do nothing
    if (ptr == NULL) {
        return;
    }
    pthread_mutex_lock(&mem_ctl.mutex);

    // TODO

    pthread_mutex_unlock(&mem_ctl.mutex);
}

void mdump() {
    // dump current memory manager state
    dump_chunk_list();
}

int posix_memalign(void **memptr, size_t alignment, size_t size) {
    // Allocate *size* bytes and places the address of the allocated
    // memory in *memptr*.

    if (!IS_POWER_OF_TWO(alignment) || alignment % sizeof(void*) != 0) {
        return EINVAL;
    }

    if (size == 0) {
        memptr = NULL;
        return 0;
    }

    pthread_mutex_lock(&mem_ctl.mutex);

    if (size > SEPARATE_CHUNK_THRESHOLD) {
        mem_chunk_t *chunk = allocate_chunk(size);
        mem_block_t *block = allocate_block(chunk, size);
    }

    pthread_mutex_unlock(&mem_ctl.mutex);
    return 0;
}

