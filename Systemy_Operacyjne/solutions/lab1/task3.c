#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <string.h>
#include "common.h"
#include "procword.h"

#define BUFF_SIZE 1024

static bool verbose = false;

int pipe_read[2];
int pipe_write[2];


void close_pipe(int *pipefd) {
    close(pipefd[0]);
    close(pipefd[1]);
}

void formatter() {
    char buffer[BUFF_SIZE];
    while (read(pipe_read[0], buffer, BUFF_SIZE)) {
        if (verbose) printf("formatter: processing input...\n");
        char *word = filter(buffer, strlen(buffer), isalnum_wrapper);
        write(pipe_write[1], word, strlen(word));

        if (!strcmp(buffer, EXIT_WORD))
            break;

        memset(buffer, 0, BUFF_SIZE);
    }
    printf("formatting finished\n");
}

void reader() {
    char buffer[BUFF_SIZE];
    static int words_count;

    if (verbose) printf("reader: pass a word \n");

    while (true) {
        char *word = buffer;
        if (scanf("%s", buffer) == EOF)
            word = EXIT_WORD;
        words_count++;
        write(pipe_read[1], word, strlen(word));

        if (!strcmp(word, EXIT_WORD))
            break;
    }
    printf("Words in total: %d\n", --words_count);
}


void writer() {
    char buffer[BUFF_SIZE];
    static int chars_count[CHARS_ARRAY_SIZE];

    while (read(pipe_write[0], buffer, BUFF_SIZE) && strcmp(buffer, EXIT_WORD)) {
        char *c = buffer;

        while (*c != '\0') {
            int index = get_char_index(*c);
            chars_count[index]++;
            c++;
        }

        if (verbose) printf("writer: the word = %s\n", buffer);
        memset(buffer, 0, BUFF_SIZE);
    }

    printf("Characters occurrences: \n");
    for (int i=0; i < CHARS_ARRAY_SIZE; ++i)
        if (chars_count[i])
            printf("%c: %d\n", get_char_by_index(i), chars_count[i]);
}

pid_t spawn_reader() {
    pid_t pid = fork();
    switch (pid) {
        case -1: handle_error("spawn_reader"); break;
        case 0:
            close_pipe(pipe_write);
            reader();
            close_pipe(pipe_read);
            exit(EXIT_SUCCESS);
    }
    return pid;
}

pid_t spawn_formatter() {
    pid_t pid = fork();
    switch (pid) {
        case -1: handle_error("spawn_formatter"); break;
        case 0:
            formatter();
            close_pipe(pipe_read);
            close_pipe(pipe_write);
            exit(EXIT_SUCCESS);
    }
    return pid;
}

pid_t spawn_writer() {
    pid_t pid = fork();
    switch (pid) {
        case -1: handle_error("spawn_writer"); break;
        case 0:
            close_pipe(pipe_read);
            writer();
            close_pipe(pipe_write);
            exit(EXIT_SUCCESS);
    }
    return pid;
}

void init_pipe(int *pipefd) {
     if (pipe(pipefd) == -1) {
        handle_error("pipe");
        exit(EXIT_FAILURE);
     }
}


int main(int argc, char *argv[]) {
    pid_t reader_pid, formatter_pid, writer_pid;

    verbose = (argc >= 2) && (strcmp(argv[1], "--verbose") == 0);

    if (verbose) printf("Creating pipe...\n");

    init_pipe(pipe_read);
    init_pipe(pipe_write);

    if (verbose) printf("Spawning processes...\n");

    reader_pid = spawn_reader();
    formatter_pid = spawn_formatter();
    writer_pid = spawn_writer();

    close_pipe(pipe_read);
    close_pipe(pipe_write);

    waitpid(reader_pid, NULL, WCONTINUED);
    waitpid(formatter_pid, NULL, WCONTINUED);
    waitpid(writer_pid, NULL, WCONTINUED);

    return 0;
}
