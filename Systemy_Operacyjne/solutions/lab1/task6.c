#include <sys/resource.h>
#include <sys/stat.h>
#include <unistd.h>
#include <libgen.h>
#include <syslog.h>
#include <fcntl.h>
#include <signal.h>
#include <stdbool.h>
#include "common.h"


void daemonize(const char *cmd) {
    int i, fd0, fd1, fd2;
    pid_t pid;
    struct rlimit rl;
    struct sigaction sa;

    // Clear file creation mask
    umask(0);

    // Get maximum number of file descriptors
    if (getrlimit(RLIMIT_NOFILE, &rl) < 0)
        handle_error("could not get file limit");

    // Become a session leader to lose controlling terminal
    if ((pid = fork()) < 0)
        handle_error("could not fork");
    else if (pid != 0)  // parent
        exit(0);
    setsid();

    // Ensure future opens won't allocate controlling terminals
    sa.sa_handler = SIG_IGN;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    if (sigaction(SIGHUP, &sa, NULL) < 0)
        handle_error("cannot ignore SIGHUP");
    if ((pid = fork()) < 0)
        handle_error("could not fork");
    else if (pid != 0)  // parent
        exit(0);

    // Change current dir to root
    if (chdir("/") < 0)
        handle_error("could not change directory to /");

    // Close all open file descriptors
    if (rl.rlim_max == RLIM_INFINITY)
        rl.rlim_max = 1024;
    for (i = 0; i < rl.rlim_max; i++)
        close(i);

    // Attach file descriptors to /dev/null
    fd0 = open("/dev/null", O_RDWR);
    fd1 = dup(0);
    fd2 = dup(0);

    // Initialize the log file
    openlog(cmd, LOG_CONS, LOG_DAEMON);
    if (fd0 != 0 || fd1 != 1 || fd2 != 2) {
        syslog(LOG_ERR, "unexpected file descriptors %d %d %d", fd0, fd1, fd2);
        exit(1);
    }
    syslog(LOG_INFO, "daemon initiated properly");
}


int main(int argc, char *argv[]) {
    char *daemon_name = basename(argv[0]);
    int sig;
    sigset_t sigset;
    int counter = 0;

    daemonize(daemon_name);

    sigfillset(&sigset);

    if (sigprocmask(SIG_SETMASK, &sigset, NULL) < 0) {
        syslog(LOG_ERR, "cannot block signals");
        exit(EXIT_FAILURE);
    }

    syslog(LOG_INFO, "daemon %s started listening on signals...", daemon_name);

    while (true) {
        sigwait(&sigset, &sig);

        if (sig == SIGTERM) {
            syslog(LOG_INFO, "SIGTERM: exiting daemon...");
            break;
        } else if (sig == SIGUSR1) {
            syslog(LOG_INFO, "SIGUSR1: signals received: %d", ++counter);
        } else if (sig == SIGHUP) {
            syslog(LOG_INFO, "SIGHUP: zero'ing counter");
            counter = 0;
        }
    }

    return 0;
}
