#include <assert.h>
#include <sys/wait.h>
#include <stdio.h>
#include <semaphore.h>
#include <stdlib.h>
#include <signal.h>
#include <stdbool.h>
#include <time.h>
#include <unistd.h>
#include <fcntl.h>
#include "common.h"

#define COOK_ID (-42)
#define CAULDRON_SEM_NAME "/cauldron"
#define PORTIONS_SEM_NAME "/portions"
#define COOK_SEM_NAME "/cook"
#define FULL_GOULASH_SEM_NAME "/full_goulash"

typedef struct {
    int id;
    sem_t *cauldron;
    sem_t *cook;
    sem_t *full_goulash;
    sem_t *portions;
} savage_t;

pid_t *savages;
pid_t cook_pid;

int savages_count;
int cauldron_capacity;

void chatter(savage_t *s) {
    thread_safe_print("[%d] is chattering\n", s->id);
    usleep_rand(MIN_TIME, MAX_TIME);
}

void eat(savage_t *s) {
    int goulash_portions_left;
    sem_wait(s->cauldron);
    sem_getvalue(s->portions, &goulash_portions_left);

    if (goulash_portions_left == 0) {
        thread_safe_print("[%d] Caldron is empty!\n", s->id);
        sem_post(s->cook);
        sem_wait(s->full_goulash);

        sem_getvalue(s->portions, &goulash_portions_left);
        assert(goulash_portions_left == cauldron_capacity);
    }
    sem_wait(s->portions);
    sem_post(s->cauldron);

    thread_safe_print("[%d] is eating...\n", s->id);
    thread_safe_print("[  ] Portions left: %d\n", goulash_portions_left);
    usleep_rand(MIN_TIME, MAX_TIME);
}

void savage_sleep(savage_t *s) {
    thread_safe_print("[%d] is going to sleep\n", s->id);
    usleep_rand(MIN_TIME, MAX_TIME);
}

void open_semaphores(savage_t *s) {
    s->cauldron = sem_open(CAULDRON_SEM_NAME, O_RDWR);
    s->cook = sem_open(COOK_SEM_NAME, O_RDWR);
    s->full_goulash = sem_open(FULL_GOULASH_SEM_NAME, O_RDWR);
    s->portions = sem_open(PORTIONS_SEM_NAME, O_RDWR);
}

void savage(int i) {
    savage_t self;
    self.id = i;
    open_semaphores(&self);
    thread_safe_print("[%d] New savage came to the village\n", i);

    while (true) {
        chatter(&self);
        eat(&self);
        savage_sleep(&self);
    }
}

void cook() {
    int goulash_portions_left;
    savage_t self;
    self.id = COOK_ID;
    open_semaphores(&self);
    thread_safe_print("[%d] Cook is present in the village\n", self.id);

    while (true) {
        sem_wait(self.cook);

        sem_getvalue(self.portions, &goulash_portions_left);
        assert(goulash_portions_left == 0);

        thread_safe_print("[%d] Cooking new goulash...\n", self.id);
        usleep(MAX_TIME*2);
        thread_safe_print("[%d] Pouring soup to cauldron\n", self.id);
        usleep(MIN_TIME*2);

        for (int i=0; i<cauldron_capacity; i++)
            sem_post(self.portions);

        sem_post(self.full_goulash);
    }
}

void spawn_cook() {
    switch (cook_pid = fork()) {
        case -1:
            handle_error("fork");
            break;
        case 0:
            cook();
            break;
    }
}

void spawn_savages() {
    for (int i=0; i<savages_count; i++) {
        switch (savages[i] = fork()) {
            case -1:
                handle_error("fork");
                break;
            case 0:
                savage(i);
        }
    }
}

void _init_semaphore(char *name, int value) {
    if (sem_open(name,
                 O_CREAT | O_EXCL,
                 S_IRUSR | S_IWUSR,
                 value) == SEM_FAILED)
        handle_error("sem_open");
}

void init_semaphores() {
    _init_semaphore(PORTIONS_SEM_NAME, cauldron_capacity);
    _init_semaphore(CAULDRON_SEM_NAME, 1);
    _init_semaphore(COOK_SEM_NAME, 0);
    _init_semaphore(FULL_GOULASH_SEM_NAME, 0);
}

void unlink_semaphores() {
    sem_unlink(PORTIONS_SEM_NAME);
    sem_unlink(CAULDRON_SEM_NAME);
    sem_unlink(COOK_SEM_NAME);
    sem_unlink(FULL_GOULASH_SEM_NAME);
}

void cleanup() {
    unlink_semaphores();
    free(savages);
}

void sigint_handler() {
    for (int i=0; i<savages_count; i++)
        kill(savages[i], SIGKILL);

    cleanup();
}

void help() {
    printf("Usage: ./savages <savages_count> <cauldron_capacity>\n");
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {
    int status;
    srand(time(NULL));
    set_sigint_handler(&sigint_handler);
    unlink_semaphores();

    if (argc < 3)
        help();

    savages_count = atoi(argv[1]);
    cauldron_capacity = atoi(argv[2]);

    savages = malloc(sizeof(pid_t) * savages_count);

    init_semaphores();
    spawn_cook();
    spawn_savages();

    for (int i=0; i<savages_count; i++)
        wait(&status);

    cleanup();
    return 0;
}

