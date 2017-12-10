#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include "common.h"

void handle_error(char *err_msg) {
    const int BUFF_SIZE = 50;
    char err_str[BUFF_SIZE];
    strerror_r(errno, err_str, BUFF_SIZE);
    printf("An error occured, msg: %s, error: %s\n", err_str, err_msg);
    exit(EXIT_FAILURE);
}

void thread_safe_print(char *str, int arg) {
    char buffer[64];
    sprintf(buffer, str, arg);
    fprintf(stdout, buffer);
    fflush(stdout);
}

int rand_int(int from, int to) {
    return (rand() % (to - from)) + from;
}

void usleep_rand(int from, int to) {
    usleep(rand_int(from, to));
}

void set_sigint_handler(void (*handler)()) {
    struct sigaction sa;

    sigemptyset(&sa.sa_mask);
    sa.sa_handler = handler;

    if (sigaction(SIGINT, &sa, NULL) < 0)
        handle_error("sigaction");
}

