#include <stdio.h>
#include <stdlib.h>
#include "common.h"

int left_index(int i, int n) { return (i) % n; }

int right_index(int i, int n) { return (i+n-1) % n; }

void help() {
    printf("Usage: ./task2 <philosophers_num>\n");
}

int get_philo_num(int argc, char *argv) {
    if (argc < 2) {
        help();
        return 0;
    }
    return atoi(argv);
}

