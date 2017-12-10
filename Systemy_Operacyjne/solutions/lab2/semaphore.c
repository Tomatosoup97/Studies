#include <pthread.h>
#include <assert.h>
#include <stdbool.h>
#include "common.h"
#include "semaphore.h"


void sem_init(sem_t *sem, unsigned value) {
    bool status = false;
    status = pthread_mutexattr_init(&sem->critsecAttr) != 0 || status;
    status = pthread_mutexattr_settype(&sem->critsecAttr,
                                       PTHREAD_MUTEX_ERRORCHECK) != 0 || status;
    status = pthread_mutex_init(&sem->critsec, &sem->critsecAttr) != 0 || status;
    status = pthread_cond_init(&sem->waiters, NULL) != 0 || status;

    if (status)
        handle_error("sem_init/pthread");

    sem->count = value;
}

void sem_wait(sem_t *sem) {
    assert(pthread_mutex_lock(&sem->critsec) == 0);

    while (sem->count == 0)
        pthread_cond_wait(&sem->waiters, &sem->critsec);

    assert(sem->count > 0);
    sem->count--;

    assert(pthread_mutex_unlock(&sem->critsec) == 0);
}

void sem_post(sem_t *sem) {
    assert(pthread_mutex_lock(&sem->critsec) == 0);


    if (sem->count == 0)
        pthread_cond_signal(&sem->waiters);

    sem->count++;

    assert(pthread_mutex_unlock(&sem->critsec) == 0);
}

void sem_getvalue(sem_t *sem, int *sval) {
    assert(pthread_mutex_lock(&sem->critsec) == 0);
    *sval = sem->count;
    assert(pthread_mutex_unlock(&sem->critsec) == 0);
}

