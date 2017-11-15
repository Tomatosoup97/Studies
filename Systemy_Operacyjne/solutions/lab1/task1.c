#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <libgen.h>
#include <sys/wait.h>
#include <sys/prctl.h>
#include "common.h"


void spawn_zombie() {
    pid_t pid = fork();

    switch (pid) {
        case -1:
            handle_error("fork zombie");
            break;
        case 0:
            printf("I am zombie: %d\n", getpid());
            exit(EXIT_SUCCESS);
            break;
    }
}


pid_t spawn_orphan() {
    pid_t pid = fork();

    switch(pid) {
        case -1:
            handle_error("fork orphan child");
            break;
        case 0:
            printf("I am child process: %d\n", getpid());

            switch(fork()) {
                case -1:
                    handle_error("fork orphan grandchild");
                    break;
                case 0:
                    printf("I am grandchild process: %d\n", getpid());
                    for (;;) {
                        sleep(1);
                    }
            }
            for (;;) {
                sleep(1);
            }
    }
    return pid;
}


pid_t spawn_process_reaper() {
    pid_t pid = fork();

    switch(pid) {
        case -1:
            handle_error("process reaper");
            break;
        case 0:
            printf("I am process reaper: %d\n", getpid());
            prctl(PR_SET_CHILD_SUBREAPER);
            break;
    }
    return pid;
}


pid_t exec_ps(char *progname) {
    pid_t pid = fork();
    char *argv[] = { "/usr/bin/ps", "-C", progname, "-o", "pid,ppid,stat,cmd", 0};

    switch(pid) {
        case -1:
            handle_error("fork");
            break;
        case 0:
            if (execve(argv[0], &argv[0], ENV) == -1) {
                handle_error("execve");
            }
            break;
    }
    return pid;
}


void ignore_sigchld() {
    struct sigaction sigact;
    sigact.sa_handler = SIG_IGN;

    if (sigaction(SIGCHLD, &sigact, NULL) < 0) {
        handle_error("ignore SIGCHLD sigaction");
    }
}


void help() {
    printf("Usage: ./task1 <zombie | orphan> [--ignore-sig | --reaper]\n");
}


int main(int argc, char *argv[]) {
    pid_t ps_pid;

    if (argc < 2) {
        help();
        return 0;
    } else if (argc > 2) {
        if (strcmp(argv[2], "--ignore-sig") == 0) {
            ignore_sigchld();
        }
        if (strcmp(argv[2], "--reaper") == 0) {
            prctl(PR_SET_CHILD_SUBREAPER);
            // spawn_process_reaper(); // (TODO: ?)
        }
    }

    printf("Main process: %d\n", getpid());


    if (strcmp(argv[1], "zombie") == 0) {
        spawn_zombie();
    } else if (strcmp(argv[1], "orphan") == 0) {
        pid_t child_pid = spawn_orphan();

        sleep(1);
        printf("-----\n");

        ps_pid = exec_ps(basename(argv[0]));
        waitpid(ps_pid, NULL, WCONTINUED);

        printf("-----\n");

        kill(child_pid, SIGTERM);
    } else {
        help();
        return 0;
    }
    printf("\n");
    ps_pid = exec_ps(basename(argv[0]));
    waitpid(ps_pid, NULL, WCONTINUED);

    return 0;
}

