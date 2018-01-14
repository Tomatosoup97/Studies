#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include "queue.h"
#include "mem_mgmt.h"

mem_ctl_t mem_ctl = {
    LIST_HEAD_INITIALIZER(mem_ctl.ma_chunks),
    PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP,
};

void *foo_malloc(size_t size) {
    void *ptr;
    pthread_mutex_lock(&mem_ctl.mutex);

    int res = foo_posix_memalign(&ptr, WORDSIZE, size);
    if (res == ENOMEM) errno = ENOMEM;

    pthread_mutex_unlock(&mem_ctl.mutex);
    return ptr;
}

void zero_init_memory(void *ptr, int32_t count) {
    char *memory = (char*) ptr;
    if (ptr == NULL)
        return;
    for (int i=0; i < count; i++)
        memory[i] = 0;
}

void *foo_calloc(size_t count, size_t size) {
    void *ptr;

    pthread_mutex_lock(&mem_ctl.mutex);
    int res = foo_posix_memalign(&ptr, WORDSIZE, count * size);
    if (res == ENOMEM) errno = ENOMEM;

    zero_init_memory(ptr, count);

    pthread_mutex_unlock(&mem_ctl.mutex);
    return ptr;
}

void *foo_realloc(void *ptr, size_t size) {
    if (size == 0) {
        foo_free(ptr);
        return NULL;
    }
    if (ptr == NULL)
        return foo_malloc(size);

    pthread_mutex_lock(&mem_ctl.mutex);

    void* new_ptr;
    mem_chunk_t *chunk = find_chunk(ptr);
    mem_block_t *block = find_block(ptr);
    mem_block_t *next_block;

    size_t aligned_size = ALIGN_SIZE(size, WORDSIZE);
    size_t mem_required = aligned_size - ABS(block->mb_size);

    if ((int64_t) aligned_size <= (int64_t) ABS(block->mb_size)) {
        // Shrink block size
        block->mb_size = MARK_SIZE_ALLOCATED(aligned_size);
        new_ptr = ptr;
    }
    else if (!IS_LAST_BLOCK(chunk, block) && \
            (next_block = GET_NEXT_BLOCK(chunk, block)) != NULL &&\
            (next_block->mb_size >= (int64_t) mem_required)
    ) {
        // Take part of memory from next free block
        shift_free_block_right(next_block, mem_required);
        block->mb_size = MARK_SIZE_ALLOCATED(aligned_size);
        new_ptr = ptr;
    }
    else {
        // Allocate memory in new place
        new_ptr = foo_malloc(aligned_size);
        memcpy(new_ptr, ptr, ABS(block->mb_size));
        foo_free(ptr);
    }

    pthread_mutex_unlock(&mem_ctl.mutex);
    return new_ptr;
}

int foo_posix_memalign(void **memptr, size_t alignment, size_t size) {
    size_t aligned_size;
    mem_chunk_block_tuple_t chunk_blk_tuple;

    if (!IS_POWER_OF_TWO(alignment) || alignment % sizeof(void*) != 0)
        return EINVAL;

    if (size == 0) {
        *memptr = NULL;
        return 0;
    }

    pthread_mutex_lock(&mem_ctl.mutex);

    aligned_size = ALIGN_SIZE(size, alignment);

    bool is_big_allocation = size > (size_t) SEPARATE_CHUNK_THRESHOLD;

    if (!is_big_allocation)
        chunk_blk_tuple = find_free_block_with_size(aligned_size + sizeof(mem_block_t));

    if (!is_big_allocation && chunk_blk_tuple.block != NULL) {
        *memptr = allocate_mem_in_block(chunk_blk_tuple.chunk,
                                        chunk_blk_tuple.block,
                                        aligned_size)->mb_data;
    } else {
        mem_chunk_t *chunk = allocate_chunk(aligned_size);

        *memptr = allocate_mem_in_block(chunk, chunk->ma_first, aligned_size)->mb_data;
    }

    pthread_mutex_unlock(&mem_ctl.mutex);
    return 0;
}

void foo_free(void *ptr) {
    if (ptr == NULL)
        return;

    pthread_mutex_lock(&mem_ctl.mutex);

    mem_chunk_t *chunk = find_chunk(ptr);
    free_block(chunk, ptr);

    if (IS_LAST_BLOCK(chunk, chunk->ma_first)) {
        assert(IS_BLOCK_FREE(chunk->ma_first));
        free_chunk(chunk);
    }

    pthread_mutex_unlock(&mem_ctl.mutex);
}

void mdump() {
    /* Dump current memory manager state */
    dump_chunks_all_blocks();
}
