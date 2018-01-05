#ifndef _CHUNK_H_
#define _CHUNK_H_

#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <limits.h>
#include "queue.h"

#define WORDSIZE __WORDSIZE
#define PAGESIZE (getpagesize())
#define SEPARATE_CHUNK_THRESHOLD (PAGESIZE * 4)

#define SND_FREE_BLK_IN_CHUNK(chunk) \
    LIST_NEXT(LIST_FIRST(&chunk->ma_freeblks), mb_node)

typedef struct mem_block {
    int32_t mb_size;                    // mb_size < 0 => allocated
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

size_t align_size(size_t size, size_t alignment);

size_t calc_required_space(size_t size);

mem_chunk_t *allocate_chunk(size_t size);

mem_chunk_t *find_chunk(void *ptr);

void free_chunk(mem_chunk_t *chunk);

void dump_chunk_list();

#endif
