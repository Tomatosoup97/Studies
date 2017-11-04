#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <sys/wait.h>


void dispatch_process() {
    pid_t pid = fork();

    if (pid == -1) {
        printf("Failed to fork!\n");
    }
    else if (pid > 0) {
        printf("In parent after forking child\n");
    }
    else {
        char *argv[] = { "./handler"};
        char *envp[] =
        {
            "HOME=/",
            "PATH=/bin:/usr/bin",
            0
        };
        execve(argv[0], &argv[0], envp);
    }
}


void signal_handler(int sig) {
    printf("\nReceived signal in dispatcher\n");
}


int main() {
    if (signal(SIGINT, signal_handler) == SIG_ERR) {
        printf("COULD NOT HANDLE SIGINT ERROR!");
    }

    dispatch_process();
    dispatch_process();

    for (;;) {
        sleep(10);
        printf("I am dispatcher!\n");
    }
    return 0;
}

