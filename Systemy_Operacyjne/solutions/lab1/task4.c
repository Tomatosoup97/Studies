#define _GNU_SOURCE

#include <sys/mman.h>
#include <execinfo.h>
#include <signal.h>
#include <stdbool.h>
#include <unistd.h>
#include "common.h"

#define MEM_SIZE 1024
#define BUFF_SIZE 1024
#define TRACE_BUFF_SIZE 32


void segfault_handler(int sig, siginfo_t *siginfo, void *uctx) {
    void *trace_buff[TRACE_BUFF_SIZE];
    ucontext_t *context = (ucontext_t *) uctx;

    unsigned char *PC = (unsigned char *) context -> uc_mcontext.gregs[REG_RIP];
    unsigned char *SP = (unsigned char *) context -> uc_mcontext.gregs[REG_RSP];

    backtrace(trace_buff, TRACE_BUFF_SIZE);
    backtrace_symbols_fd(trace_buff, TRACE_BUFF_SIZE, 2);

    // reentrant way of printing error message
    char buff[BUFF_SIZE];
    int message;
    message = snprintf(
            buff, BUFF_SIZE,
            "Invalid address: %p \n"
            "Signal code: %d\n"
            "Program counter: %lx\n"
            "Stack pointer: %lx\n",
            siginfo->si_addr, siginfo->si_code, (unsigned long) PC, (unsigned long) SP);

    write(2, buff, message);
    exit(EXIT_FAILURE);
}


int main(int argc, char *argv[]) {
    struct sigaction sa;
    sa.sa_flags = SA_SIGINFO;
    sa.sa_sigaction = segfault_handler;

    char *ro_memory_addr = mmap(0, MEM_SIZE, PROT_READ,
                                MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (sigaction(SIGSEGV, &sa, NULL) < 0)
        handle_error("sigaction");

    if (argc > 1 && !strcmp(argv[1], "read-only")) {
        ro_memory_addr[0] = 'c';
    } else if (argc > 1 && !strcmp(argv[1], "unmapped")) {
        munmap(ro_memory_addr, MEM_SIZE);
        ro_memory_addr[0] = 'c';
    } else {
        printf("Usage: ./task4 <read-only | unmapped>\n");
    }

    return 0;
}
