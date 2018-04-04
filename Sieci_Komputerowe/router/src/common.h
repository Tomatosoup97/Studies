#ifndef _COMMON_H_
#define _COMMON_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define TRUE 1
#define FALSE 0
#define DEBUG 0

#define last_k_bits_mask(k) (~((1 << (32 - (k))) - 1))

#define first_k_bits_mask(k) ((1 << (k)) - 1)

#define handle_error(err_msg) ({\
    const int BUFF_SIZE = 64;\
    char err_str[BUFF_SIZE];\
    strerror_r(errno, err_str, BUFF_SIZE);\
    fprintf(stderr, "Error occured. msg: %s, error: %s\n", err_str, err_msg);\
    exit(EXIT_FAILURE);\
})

#define print_buff(buffer, buff_len) ({\
    for (ssize_t i=0; i<buff_len; i++)\
        printf("%d ", buffer[i]);\
    printf("\n");\
})

#endif

