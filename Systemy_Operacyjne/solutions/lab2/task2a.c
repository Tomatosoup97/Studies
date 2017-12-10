#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <pthread.h>
#include <time.h>
#include "philosophers.h"
#include "semaphore.h"
#include "common.h"

static int n;

pthread_t *threads;
sem_t lock;
sem_t *forks;

void think(uint64_t i) {
    thread_safe_print("%" PRIu64 ": thinking...\n", i);
    usleep_rand(MIN_TIME, MAX_TIME);
}

void eat(uint64_t i) {
    thread_safe_print("%" PRIu64 ": eating...\n", i);
    usleep_rand(MIN_TIME, MAX_TIME);
    thread_safe_print("%" PRIu64 ": finished eating\n", i);
}

void take_forks(uint64_t i) {
    sem_wait(&lock);
    thread_safe_print("%" PRIu64 ": Taking forks\n", i);

    sem_wait(&forks[right_index(i, n)]);
    sem_wait(&forks[left_index(i, n)]);

    thread_safe_print("%" PRIu64 ": Finishing\n", i);
}

void put_forks(uint64_t i) {
    thread_safe_print("%" PRIu64 ": Putting forks down\n", i);

    sem_post(&forks[left_index(i, n)]);
    sem_post(&forks[right_index(i, n)]);

    sem_post(&lock);
}

void *philosopher(void *arg) {
    int *i = arg;
    thread_safe_print("%" PRIu64 ": Starting feast\n", *i);

    while (true) {
        think(*i);
        take_forks(*i);
        eat(*i);
        put_forks(*i);
    }
    return (void *) 0;
}

void sigint_handler() {
    thread_safe_print("Canceling %d threads...\n", n);
    for (int i=0; i < n; i++)
        pthread_cancel(threads[i]);
    exit(EXIT_SUCCESS);
}

void initialize_semaphores() {
    sem_init(&lock, n-1);

    for (int i=0; i<n; i++)
        sem_init(&forks[i], 1);
}

int main(int argc, char *argv[]) {
    void *res;
    int *indexes;
    set_sigint_handler(&sigint_handler);

    srand(time(NULL));
    n = get_philo_num(argc, argv[1]);

    threads = malloc(sizeof(pthread_t) * n);
    forks = malloc(sizeof(sem_t) * n);
    indexes = malloc(sizeof(int) * n);
    initialize_semaphores();

    for (int i=0; i<n; i++) {
        indexes[i] = i;
        if (pthread_create(&threads[i], NULL, philosopher, &indexes[i]) != 0)
            handle_error("pthread_create");
    }

    for (int i=0; i<n; i++)
        if (pthread_join(threads[i], &res) != 0)
            handle_error("pthread_join");

    free(threads);
    free(forks);
    free(indexes);

    return 0;
}

