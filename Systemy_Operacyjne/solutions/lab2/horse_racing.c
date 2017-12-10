#define _GNU_SOURCE
#include <sys/mman.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include "common.h"
#include "barrier.h"

#define BARRIER_GATE_NAME "/starting_gate"

int rounds;
int horses_count;

pid_t *horses;
barrier_t *gate;

void run_horse(int horse_id) {
    struct timeval tval;
    char buffer[128];

    for (int i=0; i<rounds; i++) {
        bar_wait(gate);

        gettimeofday(&tval, NULL);
        sprintf(buffer, "Horse %d: running lap %d, time: %ld.%06ld\n",
                horse_id, i+1, tval.tv_sec, tval.tv_usec);

        fputs_unlocked(buffer, stdout);
        fflush_unlocked(stdout);

        usleep_rand(MIN_TIME, MAX_TIME);
    }
}

void start_racing() {
    printf("Leeeeet the racing beeegin!\n");
    for (int i=0; i<horses_count; i++) {
        switch (horses[i] = fork()) {
            case -1:
                handle_error("fork");
                break;
            case 0:
                run_horse(i);
                exit(EXIT_SUCCESS);
                break;
        }
    }
}

void init_barrier() {
    int fd = shm_open(BARRIER_GATE_NAME,
                      O_CREAT | O_RDWR,
                      S_IRUSR | S_IWUSR);
    if (fd == 0)
        handle_error("shm_open");

    ftruncate(fd, sizeof(barrier_t));
    gate = mmap(NULL,
                   sizeof(barrier_t),
                   PROT_READ | PROT_WRITE,
                   MAP_SHARED,
                   fd, 0);

    close(fd);
    bar_init(gate, horses_count);
}

void help() {
    printf("Usage: ./horse_raceing <horses: int> <rounds: int>\n");
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {
    int status;
    srand(time(NULL));

    if (argc < 3)
        help();

    horses_count = atoi(argv[1]);
    rounds = atoi(argv[2]);
    horses = malloc(sizeof(pid_t) * horses_count);

    init_barrier();
    start_racing();

    for (int i=0; i<horses_count; i++)
        wait(&status);

    exit(EXIT_SUCCESS);
}

