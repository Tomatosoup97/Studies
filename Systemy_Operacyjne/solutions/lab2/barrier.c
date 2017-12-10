#include <semaphore.h>
#include "barrier.h"
#include "common.h"


void bar_init(barrier_t *barrier, int value) {
    barrier->count = value;

    if (sem_init(&barrier->floodgate_in, IN_SHARED_MEM, value) == -1)
        handle_error("sem_init");

    if (sem_init(&barrier->floodgate_out, IN_SHARED_MEM, 0) == -1)
        handle_error("sem_init");
}

void bar_wait(barrier_t *barrier) {
    sem_wait(&barrier->floodgate_in);

    int processes_left;
    sem_getvalue(&barrier->floodgate_in, &processes_left);

    if (processes_left == 0) {
         for (int i=0; i < barrier->count - 1; i++)
            sem_post(&barrier->floodgate_out);

         for (int i=0; i < barrier->count; i++)
            sem_post(&barrier->floodgate_in);
    } else {
        sem_wait(&barrier->floodgate_out);
    }
}

void bar_destroy(barrier_t *barrier) {
    sem_destroy(&barrier->floodgate_in);
    sem_destroy(&barrier->floodgate_out);
}

