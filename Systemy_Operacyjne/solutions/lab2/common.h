#ifndef _COMMON_H_
#define _COMMON_H_

#define SEC_AS_MICROSEC 1000000
#define MIN_TIME (SEC_AS_MICROSEC * 0.5)
#define MAX_TIME (SEC_AS_MICROSEC * 1.5)

void handle_error(char *err_msg);

void thread_safe_print(char *str, int arg);

int rand_int(int from, int to);

void usleep_rand(int from, int to);

void set_sigint_handler(void (*handler)());

#endif

