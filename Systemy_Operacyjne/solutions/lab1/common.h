#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>


char *ENV[] = {
    "PATH=/bin:/usr/bin",
    0
};


void handle_error(char *err_msg) {
    const int BUFF_SIZE = 50;
    char err_str[BUFF_SIZE];
    strerror_r(errno, err_str, BUFF_SIZE);
    printf("An error occured, msg: %s, error: %s\n", err_str, err_msg);
    exit(EXIT_FAILURE);
}


