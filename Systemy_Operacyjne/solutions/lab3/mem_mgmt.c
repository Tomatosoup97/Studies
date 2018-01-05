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
    // TODO: test
    void *ptr;
    int res;

    pthread_mutex_lock(&mem_ctl.mutex);
    res = posix_memalign(&ptr, WORDSIZE, size);
    if (res == ENOMEM) errno = ENOMEM;

    pthread_mutex_unlock(&mem_ctl.mutex);
    return ptr;
}

void *calloc(size_t count, size_t size) {
    // TODO: test
    void *ptr;

    pthread_mutex_lock(&mem_ctl.mutex);
    int res = posix_memalign(&ptr, WORDSIZE, count * size);
    if (res == ENOMEM) errno = ENOMEM;

    char *memory = (char*) ptr;
    if (ptr != NULL) for (int i=0; i<size; i++) memory[i] = 0;

    pthread_mutex_unlock(&mem_ctl.mutex);
    return ptr;
}

void *realloc(void *ptr, size_t size) {
    // change size of em block pointed to by *ptr*
    if (size == 0) {
        free(ptr);
        return NULL;
    }
    if (ptr == NULL)
        return malloc(size);

    pthread_mutex_lock(&mem_ctl.mutex);

    // TODO: implement

    pthread_mutex_unlock(&mem_ctl.mutex);
    return NULL;
}

void free(void *ptr) {
    mem_block_t *chunk;

    if (ptr == NULL)
        return;
    pthread_mutex_lock(&mem_ctl.mutex);

    chunk = find_chunk(ptr);
    // TODO: implement

    pthread_mutex_unlock(&mem_ctl.mutex);
}

void mdump() {
    // dump current memory manager state
    dump_chunk_list();
}


int posix_memalign(void **memptr, size_t alignment, size_t size) {
    size_t aligned_size;
    mem_block_t *free_block;
    mem_chunk_t *new_chunk;

    if (!IS_POWER_OF_TWO(alignment) || alignment % sizeof(void*) != 0)
        return EINVAL;

    if (size == 0) {
        *memptr = NULL;
        return 0;
    }

    pthread_mutex_lock(&mem_ctl.mutex);

    aligned_size = align_size(size, alignment);

    if (size <= SEPARATE_CHUNK_THRESHOLD &&
            (free_block = find_free_block_with_size(aligned_size)) != NULL) {
         *memptr = allocate_mem_in_block(&free_block, aligned_size);
    } else {
        mem_chunk_t *chunk = allocate_chunk(aligned_size);
        *memptr = allocate_mem_in_block(&chunk->ma_first, aligned_size);
    }

    pthread_mutex_unlock(&mem_ctl.mutex);
    return 0;
}

