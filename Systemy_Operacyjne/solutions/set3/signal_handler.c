#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <sys/wait.h>


void signal_handler(int sig) {
    printf("\nOMG DONT KILL ME!\n");
}


int main() {
    if (signal(SIGINT, signal_handler) == SIG_ERR) {
        printf("COULD NOT HANDLE SIGINT ERROR!");
    }

    for (int i=0; ;i++) {
        printf("%d\n", i);
        sleep(2);
    }
    return 0;
}
