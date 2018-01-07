#include <sys/mman.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include "chunk.h"
#include "queue.h"
#include "mem_mgmt.h"

extern mem_ctl_t mem_ctl;

size_t align_size(size_t size, size_t alignment) {
    return (size % alignment) ? size - (size % alignment) + alignment : size;
}

size_t calc_required_space(size_t size) {
    /* Required space for mem chunk aligned to page size
     * */
    int required_space = size + sizeof(mem_chunk_t);
    return align_size(required_space, PAGESIZE);
}

mem_block_t *create_initial_free_block(mem_chunk_t *chunk) {
    mem_block_t *initial_block = (void*) chunk + sizeof(mem_chunk_t);
    SET_CANARY(initial_block);
    initial_block->mb_size = chunk->size - sizeof(mem_block_t);
    initial_block->prev_block = NULL;

    assert(initial_block->mb_size > 0);
    return initial_block;
}

mem_chunk_t *allocate_chunk(size_t size) {
    /* Allocate new chunk of size :size: and create one free block
     * */
    mem_chunk_t *new_chunk;
    size_t required_space = calc_required_space(size);

    pthread_mutex_lock(&mem_ctl.mutex);

    new_chunk = mmap(NULL, required_space,
                     PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS,
                     -1, 0);

    if (new_chunk == MAP_FAILED) {
        // TODO: smarter error handling
        printf("Chunk mmap failed\n");
    }

    new_chunk->size = required_space - sizeof(mem_chunk_t);
    mem_block_t *initial_block = create_initial_free_block(new_chunk);

    new_chunk->ma_first = initial_block;
    LIST_INSERT_HEAD(&new_chunk->ma_freeblks, initial_block, mb_node);
    LIST_INSERT_HEAD(&mem_ctl.ma_chunks, new_chunk, ma_node);

    pthread_mutex_unlock(&mem_ctl.mutex);
    return new_chunk;
}

bool is_ptr_in_range(void *ptr, uint64_t from_addr, uint64_t interval) {
    uint64_t target_addr = (uint64_t) ptr;
    return target_addr > from_addr && target_addr < from_addr + interval;
}

bool is_pointer_in_chunk(mem_chunk_t *chunk, void *ptr) {
    return is_ptr_in_range(ptr, (uint64_t) chunk, FULL_CHUNK_SIZE(chunk));
}

bool is_pointer_in_block(mem_block_t *block, void *ptr) {
    return is_ptr_in_range(ptr, (uint64_t) block, FULL_BLOCK_SIZE(block));
}

mem_chunk_t *find_chunk(void *ptr) {
    /* Find chunk which holds given :ptr: address. Return NULL on failure
     * */
    mem_chunk_t *chunk;
    FOR_EACH_CHUNK(chunk)
        if (is_pointer_in_chunk(chunk, ptr))
            return chunk;
    return NULL;
}

mem_chunk_block_tuple_t *find_free_block_with_size(size_t size) {
    /* Find first block that has free :size: space */
    mem_chunk_block_tuple_t *chunk_blk_tuple;
    mem_chunk_t *chunk;
    mem_block_t *block;

    FOR_EACH_CHUNK(chunk)
        FOR_EACH_FREE_BLOCK(block, chunk)
            if (block->mb_size >= (int32_t) size) {
                CANARY_CHECK(block);
                chunk_blk_tuple->block = block;
                chunk_blk_tuple->chunk = chunk;
                return chunk_blk_tuple;
            }
    return NULL;
}

void free_chunk(mem_chunk_t *chunk) {
    assert(SND_FREE_BLK_IN_CHUNK(chunk) == NULL);
    // TODO: free_block(chink->ma_first) ?
    // TODO: free chunk mem space
    // munmap(chunk, FULL_CHUNK_SIZE(chunk));
    LIST_REMOVE(chunk, ma_node);
}

void *calc_block_address(mem_block_t *free_block, size_t size) {
    // TODO: consider making it macro definition
    return (void*) free_block + free_block->mb_size - size;
}

mem_block_t *create_allocated_block(mem_block_t *free_block, size_t size) {
    CANARY_CHECK(free_block);

    mem_block_t *block = calc_block_address(free_block, size);
    block->prev_block = free_block;
    SET_CANARY(block);

    // TODO: alignment!
    block->mb_data[0] = (uint64_t) (block + sizeof(mem_block_t));
    block->mb_size = (-1) * size; // when block is allocated size is negative

    return block;
}

mem_block_t *allocate_mem_in_block(
        mem_chunk_t *chunk,
        mem_block_t *free_block,
        size_t size
) {
    CANARY_CHECK(free_block);
    assert(free_block->mb_size >= (int32_t) size);
    pthread_mutex_lock(&mem_ctl.mutex);

    size_t free_block_space_size = free_block->mb_size;
    size_t alloc_block_size = size + sizeof(mem_block_t);

    mem_block_t *allocated_block = create_allocated_block(free_block, size);
    free_block->mb_size = free_block_space_size - alloc_block_size;

    if (!IS_LAST_BLOCK(chunk, allocated_block)) {
        mem_block_t *next_block = GET_NEXT_BLOCK(chunk, allocated_block);
        next_block->prev_block = allocated_block;
    }

    pthread_mutex_unlock(&mem_ctl.mutex);
    return allocated_block;
}

mem_block_t *get_first_block(mem_block_t *starting_block) {
    mem_block_t *block = starting_block;

    while (block->prev_block != NULL)
        block = GET_PREV_BLOCK(block);

    return block;
}

mem_block_t *find_fst_prev_free_block(mem_block_t *starting_block) {
    mem_block_t *block = starting_block->prev_block;

    while (block != NULL && !IS_BLOCK_FREE(block))
        block = GET_PREV_BLOCK(block);

    return block;
}

mem_block_t *find_block(void *ptr) {
    /* :ptr: pointer to block's mb_data */
    return container_of(ptr, mem_block_t, mb_data);
}

void left_coalesce_blocks(
        mem_chunk_t *chunk,
        mem_block_t *left_block,
        mem_block_t *right_block
) {
    pthread_mutex_lock(&mem_ctl.mutex);

    if (!IS_LAST_BLOCK(chunk, right_block)) {
        mem_block_t *next_block = GET_NEXT_BLOCK(chunk, right_block);
        next_block->prev_block = left_block;
    }
    if (IS_BLOCK_FREE(right_block))
        LIST_REMOVE(right_block, mb_node);

    left_block->mb_size += FULL_BLOCK_SIZE(right_block);

    pthread_mutex_unlock(&mem_ctl.mutex);
}

void right_coalesce_blocks(mem_block_t *left_block, mem_block_t *right_block) {
    assert(!IS_BLOCK_FREE(left_block) && IS_BLOCK_FREE(right_block));
    pthread_mutex_lock(&mem_ctl.mutex);

    mem_block_t *prev_block = find_fst_prev_free_block(right_block);
    mem_block_t *next_block = LIST_NEXT(right_block, mb_node);

    left_block->mb_size = ABS(left_block->mb_size) + FULL_BLOCK_SIZE(right_block);

    if (next_block != NULL) {
        // Insert before next list element
        LIST_REMOVE(right_block, mb_node);
        LIST_INSERT_BEFORE(next_block, left_block, mb_node);
    } else if (prev_block != NULL) {
        // Insert after previous list element
        LIST_REMOVE(right_block, mb_node);
        LIST_INSERT_AFTER(prev_block, left_block, mb_node);
    } else {
        // No free blocks on the list, insert on head
        mem_chunk_t *chunk = find_chunk((void*) left_block);
        LIST_INSERT_HEAD(&chunk->ma_freeblks, left_block, mb_node);
    }

    pthread_mutex_unlock(&mem_ctl.mutex);
}

void free_block(mem_chunk_t *chunk, void *ptr) {
    pthread_mutex_lock(&mem_ctl.mutex);
    mem_block_t *block = find_block(ptr);
    assert(block != NULL && IS_CANARY_VALID(block));

    mem_block_t *prev_block = GET_PREV_BLOCK(block);

    bool is_left_coalescing = prev_block != NULL && IS_BLOCK_FREE(prev_block);
    bool is_right_coalescing = !IS_LAST_BLOCK(chunk, block);

    if (is_right_coalescing) {
        // check right coalescing
        mem_block_t *next_block = GET_NEXT_BLOCK(chunk, block);
        is_right_coalescing = next_block != NULL && IS_BLOCK_FREE(next_block);
        if (is_right_coalescing) right_coalesce_blocks(block, next_block);
    }

    if (is_left_coalescing)
        left_coalesce_blocks(chunk, prev_block, block);

    if (!is_left_coalescing && !is_right_coalescing) {
        // No coalescing, attach to list
        mem_block_t *prev_free_block = find_fst_prev_free_block(block);
        block->mb_size = ABS(block->mb_size);
        LIST_INSERT_AFTER(prev_free_block, block, mb_node);
    }
    pthread_mutex_unlock(&mem_ctl.mutex);
}

void dump_chunks_all_blocks() {
    /* Output chunk list and all its blocks. Additionally, check if canaries
     * in blocks are valid. Negative size indicates that the block is allocated
     *
     * Format: [chunk size] :: [fst blk] -> ... -> [nth blk]
     * */
    mem_chunk_t *chunk;
    mem_block_t *block;
    pthread_mutex_lock(&mem_ctl.mutex);

    printf("\nMemory manager state\n\n");

    FOR_EACH_CHUNK(chunk) {
        printf("chunk: \t\t[size: %d addr: %p]\n", chunk->size, (void*) chunk);

        printf("All blocks:\t");

        block = chunk->ma_first;
        while (true) {
            CANARY_CHECK(block);
            printf("[size: %d, addr: %p]", block->mb_size, (void*) block);

            if (IS_LAST_BLOCK(chunk, block)) break;
            else printf(" -> ");
            block = GET_NEXT_BLOCK(chunk, block);
        }

        printf("\nFree blocks:\t");

        block = chunk->ma_first;
        FOR_EACH_FREE_BLOCK(block, chunk) {
            printf("[size: %d]", block->mb_size);
            if (LIST_NEXT(block, mb_node) != NULL) printf(" -> ");
        }
        if (LIST_NEXT(chunk, ma_node) != NULL) printf("\n|\n");
    }
    printf("\n");

    pthread_mutex_unlock(&mem_ctl.mutex);
}
