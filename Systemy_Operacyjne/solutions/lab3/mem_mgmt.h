#ifndef _MEM_MGMT_H_
#define _MEM_MGMT_H_

#include <stdlib.h>
#include <stdint.h>
#include <pthread.h>
#include "queue.h"
#include "chunk.h"

#define IS_POWER_OF_TWO(x) (x && !(x & (x - 1)))

typedef struct mem_ctl {
    LIST_HEAD(, mem_chunk) ma_chunks;
    pthread_mutex_t mutex;
} mem_ctl_t;

void *my_malloc(size_t size);

void *calloc(size_t count, size_t size);

void *realloc(void *ptr, size_t size);

void free(void *ptr);

int posix_memalign(void **memptr, size_t alignment, size_t size);

void mdump();

#endif

