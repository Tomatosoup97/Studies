#ifndef _CHUNK_H_
#define _CHUNK_H_

#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <limits.h>
#include "queue.h"
#include "mem_mgmt.h"

#define WORDSIZE __WORDSIZE
#define PAGESIZE (getpagesize())
#define SEPARATE_CHUNK_THRESHOLD (PAGESIZE * 4)
#define BOTH_METADATA_SIZE (sizeof(mem_block_t) + sizeof(mem_chunk_t))
#define CANARY_ADDR 0xDEADC0DE

#define FULL_CHUNK_SIZE(chunk) (chunk->size + sizeof(mem_chunk_t))
#define FULL_BLOCK_SIZE(block) (block->mb_size + sizeof(mem_block_t))
#define IS_BLOCK_FREE(block) (block->mb_size > 0)
#define GET_NEXT_BLOCK(block) (block + FULL_BLOCK_SIZE(block))
#define FST_FREE_BLK_IN_CHUNK(chunk) LIST_FIRST(&chunk->ma_freeblks)

#define SND_FREE_BLK_IN_CHUNK(chunk) \
    LIST_NEXT(FST_FREE_BLK_IN_CHUNK(chunk), mb_node)

#define IS_LAST_BLOCK(chunk, block) ( \
    (block + FULL_BLOCK_SIZE(block)) >= (chunk + FULL_CHUNK_SIZE(chunk)))

#define FOR_EACH_CHUNK(chunk) \
    LIST_FOREACH(chunk, &mem_ctl.ma_chunks, ma_node)

#define FOR_EACH_FREE_BLOCK(block, chunk) \
    LIST_FOREACH(block, &chunk->ma_freeblks, mb_node)

#define offsetof(TYPE, MEMBER) ((size_t) &((TYPE *)0)->MEMBER)

#define container_of(ptr, type, member) ({                      \
        const typeof( ((type *)0)->member ) *__mptr = (ptr);    \
        (type *)( (char *)__mptr - offsetof(type,member) );})

typedef struct mem_block {
    int32_t mb_size;                    // mb_size < 0 => allocated
    void *prev_block;
    union {
        LIST_ENTRY(mem_block) mb_node;  // node on free bock list, valid if block is free
        uint64_t mb_data[0];            // user data pointer, valid if block is allocated
    };
} mem_block_t;

typedef struct mem_chunk {
    LIST_ENTRY(mem_chunk) ma_node;      // node on list of all chunks
    LIST_HEAD(, mem_block) ma_freeblks; // list of all free blocks in the chunk
    int32_t size;                       // chunk size minus sizeof(mem_chunk_t)
    mem_block_t *ma_first;              // first block in the chunk
} mem_chunk_t;

typedef struct mem_chunk_block_tuple {
    mem_block_t *block;
    mem_chunk_t *chunk;
} mem_chunk_block_tuple_t;

size_t align_size(size_t size, size_t alignment);
size_t calc_required_space(size_t size);

mem_chunk_t *allocate_chunk(size_t size);
mem_chunk_t *find_chunk(void *ptr);

mem_chunk_block_tuple_t *find_free_block_with_size(size_t size);
mem_block_t *left_coalesce_blocks(mem_block_t *left_block, mem_block_t *block);
mem_block_t *right_coalesce_blocks(mem_block_t *block, mem_block_t *right_block);
mem_block_t *create_allocated_block(mem_block_t *free_block, size_t size);
mem_block_t *get_first_block(mem_block_t *starting_block);
mem_block_t *find_fst_prev_free_block(mem_block_t *starting_block);
mem_block_t *find_block(void *ptr);
mem_block_t *allocate_mem_in_block(
        mem_chunk_t *chunk,
        mem_block_t *free_block,
        size_t size
);

void free_chunk(mem_chunk_t *chunk);
void dump_chunk_list();

#endif
