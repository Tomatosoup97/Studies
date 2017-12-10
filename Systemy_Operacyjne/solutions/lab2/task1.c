/*
 * Basic test case for semaphore module.
 *
 * */

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include "common.h"
#include "semaphore.h"

#define THREADS_NUM 50

int tally = 0;

sem_t sem;
sem_t tally_sem;

static void *execThread() {
    int sval;
    printf("thread 1: I'm alive\n");

    sem_wait(&sem);

    sem_getvalue(&sem, &sval);
    assert(sval == 0);

    printf("thread 1: Acquired semaphore\n");
    sleep(0.5);  // compute
    sem_post(&sem);

    printf("thread 1: Released semaphore\n");
    return (void *) 0;
}

void first_test_case() {
    pthread_t t1;
    void *res;
    int sval;

    printf("main: Initializing semaphore\n");
    sem_init(&sem, 1);

    printf("main: Creating new thread\n");
    if (pthread_create(&t1, NULL, execThread, "") != 0)
        handle_error("pthread_create");

    sem_wait(&sem);
    printf("main: Acquired semaphore\n");
    sleep(0.5);  // compute
    sem_getvalue(&sem, &sval);
    assert(sval == 0);
    sem_post(&sem);

    printf("main: Released semaphore\n");

    if (pthread_join(t1, &res) != 0)
        handle_error("pthread_join");

    printf("main: Finishing first test case\n");
}

void *execTallyThread() {
    for (int i=0; i < 10; i++) {
        sem_wait(&sem);

        int temp_tally = tally;
        tally += 1;
        assert(tally == temp_tally+1);

        sem_post(&sem);
    }
    return (void *) 0;
}

void second_test_case() {
    void *res;
    pthread_t threads[THREADS_NUM];
    sem_init(&tally_sem, 1);

    for (int i=0; i < THREADS_NUM; i++) {
        if (pthread_create(&threads[i], NULL, execTallyThread, "") != 0)
            handle_error("pthread_create");
    }

    for (int i=0; i < THREADS_NUM; i++) {
        if (pthread_join(threads[i], &res) != 0)
            handle_error("pthread_join");
    }
    assert(tally == THREADS_NUM * 10);
}

int main() {
    first_test_case();
    second_test_case();
    return 0;
}
