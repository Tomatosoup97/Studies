#include <assert.h>
#include <fcntl.h>          // Defines O_* constants
#include <sys/stat.h>       // Defines mode constants
#include <sys/types.h>
#include <sys/wait.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <signal.h>
#include <unistd.h>
#include <time.h>
#include "common.h"
#include "philosophers.h"

#define SEM_LOCK_NAME "/semlock"
#define SEM_FORK_PREFIX "/semfork_"
#define SAFE true

typedef struct {
    int id;
    pid_t pid;
    sem_t *left_fork;
    sem_t *right_fork;
    sem_t *lock;
} philosopher_t;

int philo_num;

philosopher_t *philosophers;

void open_semaphores(philosopher_t *p);

void initialize_semaphores();

void unlink_semaphores(bool is_safe);

void get_fork_sem_name(char *buffer, int i);

void think(philosopher_t *p) {
    thread_safe_print("%d: thinking...\n", p->id);
    usleep_rand(MIN_TIME, MAX_TIME);
}

void eat(philosopher_t *p) {
    thread_safe_print("%d: thinking...\n", p->id);
    usleep_rand(MIN_TIME, MAX_TIME);
}

void take_forks(philosopher_t *p) {
    sem_wait(p->lock);
    thread_safe_print("%d: Taking forks\n", p->id);

    sem_wait(p->left_fork);
    sem_wait(p->right_fork);
}

void put_forks(philosopher_t *p) {
    thread_safe_print("%d: Putting forks down\n", p->id);

    sem_post(p->left_fork);
    sem_post(p->right_fork);
    sem_post(p->lock);
}

void philosopher(philosopher_t *p) {
    thread_safe_print("%d: Starting feast\n", p->id);
    int i=0;

    while (i<2) {
        think(p);
        take_forks(p);
        eat(p);
        put_forks(p);
    }
}

void spawn_philosopher(int i) {
    philosophers[i].id = i;
    switch (philosophers[i].pid = fork()) {
        case -1:
            handle_error("fork");
            break;
        case 0:
            open_semaphores(&philosophers[i]);
            philosopher(&philosophers[i]);
            break;
    }
}

void cleanup() {
    unlink_semaphores(SAFE);
    free(philosophers);
}

void sigint_handler() {
    for (int i=0; i<philo_num; i++)
        kill(philosophers[i].pid, SIGKILL);

    cleanup();
    exit(EXIT_SUCCESS);
}

int main(int argc, char *argv[]) {
    srand(time(NULL));

    philo_num = get_philo_num(argc, argv[1]);
    set_sigint_handler(&sigint_handler);
    unlink_semaphores(!SAFE); // Make sure no files with same name does not exist

    philosophers = malloc(sizeof(philosopher_t) * philo_num);
    initialize_semaphores();

    for (int i=0; i<philo_num; i++)
        spawn_philosopher(i);

    int status;
    for (int i=0; i<philo_num; i++)
        wait(&status);

    cleanup();
    return 0;
}

void open_semaphores(philosopher_t *p) {
    char buffer[64];
    int left = left_index(p->id, philo_num);
    int right = right_index(p->id, philo_num);

    p->lock = sem_open(SEM_LOCK_NAME, O_RDWR);

    int value=0;
    sem_getvalue(p->lock, &value);

    if (p->lock == SEM_FAILED)
        handle_error("sem_open (re-open existing)");

    assert(p->lock != SEM_FAILED);

    get_fork_sem_name(buffer, left);
    p->left_fork = sem_open(buffer,  O_RDWR);
    assert(p->left_fork != SEM_FAILED);

    get_fork_sem_name(buffer, right);
    p->right_fork = sem_open(buffer, O_RDWR);
    assert(p->right_fork != SEM_FAILED);
}

void initialize_semaphores() {
    char buffer[64];

    if (sem_open(SEM_LOCK_NAME,
                 O_CREAT | O_EXCL,
                 S_IRUSR | S_IWUSR,
                 philo_num - 1) == SEM_FAILED)
        handle_error("sem_open");

    for (int i=0; i<philo_num; i++) {
        get_fork_sem_name(buffer, i);
        if (sem_open(buffer,
                     O_CREAT | O_EXCL,
                     S_IRUSR | S_IWUSR,
                     1) == SEM_FAILED)
            handle_error("sem_open");
    }
}

void unlink_semaphores(bool is_safe) {
    char buffer[64];

    if (sem_unlink(SEM_LOCK_NAME) == -1 && is_safe)
        handle_error("sem_unlink");

    for (int i=0; i<philo_num; i++) {
        get_fork_sem_name(buffer, i);
        if (sem_unlink(buffer) == -1 && is_safe)
            handle_error("sem_unlink");
    }
}

void get_fork_sem_name(char *buffer, int i) {
    sprintf(buffer, "%s%d", SEM_FORK_PREFIX, i);
}

