#include <sys/mman.h>
#include <stdio.h>
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

    mem_block_t *initial_block = (new_chunk + sizeof(mem_chunk_t));
    initial_block->mb_size = new_chunk->size - sizeof(mem_block_t);
    initial_block->prev_block = (void*) 0xDEADC0DE;

    assert(initial_block->mb_size > 0);

    new_chunk->ma_first = initial_block;
    LIST_INSERT_HEAD(&new_chunk->ma_freeblks, initial_block, mb_node);
    LIST_INSERT_HEAD(&mem_ctl.ma_chunks, new_chunk, ma_node);

    pthread_mutex_unlock(&mem_ctl.mutex);
    return new_chunk;
}

mem_chunk_t *find_chunk(void *ptr) {
    /* Find chunk which holds given :ptr: address. Return NULL on failure
     * */
    mem_chunk_t *chunk;
    uint64_t target_addr = (uint64_t) ptr;

    FOR_EACH_CHUNK(chunk) {
        uint64_t chunk_addr = (uint64_t) chunk;
        uint64_t chunk_size = FULL_CHUNK_SIZE(chunk);
        if ( target_addr > chunk_addr &&
             target_addr < chunk_addr + chunk_size) {
            return chunk;
        }
    }
    return NULL;
}

mem_chunk_block_tuple_t *find_free_block_with_size(size_t size) {
    /* Find first block that has free :size: space */
    mem_chunk_block_tuple_t *chunk_blk_tuple;
    mem_chunk_t *chunk;
    mem_block_t *block;

    FOR_EACH_CHUNK(chunk)
        FOR_EACH_FREE_BLOCK(block, chunk)
            if (block->mb_size >= size) {
                chunk_blk_tuple->block = block;
                chunk_blk_tuple->chunk = chunk;
                return chunk_blk_tuple;
            }
    return NULL;
}

void free_chunk(mem_chunk_t *chunk) {
    assert(SND_FREE_BLK_IN_CHUNK(chunk) == NULL);
    // TODO: free_block(chunk->ma_first) ?
    // TODO: free chunk mem space
    // munmap(chunk, FULL_CHUNK_SIZE(chunk));
    LIST_REMOVE(chunk, ma_node);
}

mem_block_t *allocate_mem_in_block(
        mem_chunk_t *chunk,
        mem_block_t *free_block,
        size_t size
) {
    assert(free_block->mb_size >= size);
    pthread_mutex_lock(&mem_ctl.mutex);

    mem_block_t *allocated_block;
    size_t free_block_space_size = free_block->mb_size;
    size_t alloc_block_size = size + sizeof(mem_block_t);

    allocated_block = free_block + free_block_space_size + sizeof(mem_block_t) \
                    - alloc_block_size;
    allocated_block->prev_block = free_block;

    // TODO: alignment!
    allocated_block->mb_data[0] = (void*) allocated_block + sizeof(mem_block_t);
    allocated_block->mb_size = (-1) * size; // when block is allocated size is negative

    free_block->mb_size = free_block_space_size - alloc_block_size;

    pthread_mutex_unlock(&mem_ctl.mutex);
    return allocated_block;
}

void dump_chunk_list() {
    /* Output chunk list and its free blocks */
    mem_chunk_t *chunk;
    mem_block_t *block;

    printf("\nFormat: [chunk size] :: [fst free blk size] -> ... -> [nth free blk size]\n\n");

    FOR_EACH_CHUNK(chunk) {
        printf("[%d] :: ", chunk->size);
        FOR_EACH_FREE_BLOCK(block, chunk) {
            printf("[%d]", block->mb_size);
            if (LIST_NEXT(block, mb_node) != NULL) printf(" -> ");
        }
        if (LIST_NEXT(chunk, ma_node) != NULL) printf("\n|\n");
    }
    printf("\n");
}
