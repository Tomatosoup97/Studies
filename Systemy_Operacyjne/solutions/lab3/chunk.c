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

    LIST_FOREACH(chunk, &mem_ctl.ma_chunks, ma_node) {
        uint64_t chunk_addr = (uint64_t) chunk;
        uint64_t chunk_size = chunk->size + sizeof(mem_chunk_t);
        if ( target_addr > chunk_addr &&
             target_addr < chunk_addr + chunk_size) {
            return chunk;
        }
    }
    return NULL;
}

void free_chunk(mem_chunk_t *chunk) {
    assert(SND_FREE_BLK_IN_CHUNK(chunk) == NULL);
    // TODO: free block mem space
    // TODO: free chunk mem space
    LIST_REMOVE(chunk, ma_node);
}

mem_block_t *allocate_mem_in_block(mem_chunk_t *chunk, mem_block_t *block, size_t size) {
    assert(block->mb_size >= size);
    pthread_mutex_lock(&mem_ctl.mutex);

    block->mb_size -= size;
    // TODO

    pthread_mutex_unlock(&mem_ctl.mutex);
    return NULL;
}

void dump_chunk_list() {
    /* Output chunk list and its free blocks */
    mem_chunk_t *chunk;
    mem_block_t *block;

    printf("Format: [chunk size] :: [fst free blk size] -> [snd free blk size]\n\n");

    LIST_FOREACH(chunk, &mem_ctl.ma_chunks, ma_node) {
        printf("[%d] :: ", chunk->size);
        LIST_FOREACH(block, &chunk->ma_freeblks, mb_node) {
            printf("[%d]", block->mb_size);
            if (LIST_NEXT(block, mb_node) != NULL) printf(" -> ");
        }
        if (LIST_NEXT(chunk, ma_node) != NULL) printf("\n|\n");
    }
    printf("\n");
}

