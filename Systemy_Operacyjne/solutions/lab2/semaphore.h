#include <pthread.h>

#ifndef _SEMAPHORE_H_
#define _SEMAPHORE_H_

typedef struct {
    pthread_mutex_t critsec;
    pthread_mutexattr_t critsecAttr;
    pthread_cond_t waiters;
    int count;
} sem_t;

void sem_init(sem_t *sem, unsigned value);

void sem_wait(sem_t *sem);

void sem_post(sem_t *sem);

void sem_getvalue(sem_t *sem, int *sval);

#endif

