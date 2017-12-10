#ifndef _BARRIER_H_
#define _BARRIER_H_

#include <semaphore.h>

#define IN_SHARED_MEM 1

typedef struct {
    int count;
    sem_t floodgate_in;
    sem_t floodgate_out;
} barrier_t;

void bar_init(barrier_t *barrier, int value);

void bar_wait(barrier_t *barrier);

void bar_destroy(barrier_t *barrier);

#endif

