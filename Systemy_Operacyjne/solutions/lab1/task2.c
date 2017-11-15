#include <stdio.h>
#include <ucontext.h>
#include "common.h"
#include "procword.h"

#define F_STACK_SIZE 16384  // 2^14

static bool verbose;
static int chars_count[CHARS_ARRAY_SIZE];
static int words_count;
static char *word = "\0";

static ucontext_t uctx_main, uctx_reader, uctx_writer, uctx_formatter;

void swapcontext_wrapper(ucontext_t *uctx_from, ucontext_t *uctx_to,
                         char *name_from, char *name_to) {
    if (verbose)
        printf("\n%s: swapcontext -> %s\n", name_from, name_to);

    if (swapcontext(uctx_from, uctx_to) == -1)
        handle_error("swapcontext");
}

void process_word() {
    while (word && strcmp(word, EXIT_WORD) != 0) {
        if (verbose) printf("formatter: processsing input...");
        word = filter(word, strlen(word), isalnum_wrapper);

        swapcontext_wrapper(&uctx_formatter, &uctx_writer, "formatter", "writer");
    }
}

void print_word() {
    while (word && strcmp(word, EXIT_WORD) != 0) {

        char *c = word;

        while (*c != '\0') {
            int index = get_char_index(*c);
            chars_count[index]++;
            c++;
        }
        if (verbose) printf("writer: the word = %s", word);

        swapcontext_wrapper(&uctx_writer, &uctx_reader, "writer", "reader");
    }

    printf("Characters occurences: \n");
    for (int i=0; i < CHARS_ARRAY_SIZE; ++i)
        if (chars_count[i])
            printf("%c: %d\n", get_char_by_index(i), chars_count[i]);
}

void read_word() {
    if (verbose) printf("reader: pass a word ");

    while (strcmp(word, EXIT_WORD)) {
        if (scanf("%s", word) == EOF)
            word = EXIT_WORD;
        words_count++;
        swapcontext_wrapper(&uctx_reader, &uctx_formatter, "reader", "formatter");
    }
    printf("Words in total: %d\n", --words_count);
}

void create_context(ucontext_t *ucp, ucontext_t *uc_link, char *f_stack, void (*func)()) {

    if (getcontext(ucp) == -1)
        handle_error("getcontext");
    ucp->uc_stack.ss_sp = f_stack;
    ucp->uc_stack.ss_size = sizeof(f_stack);
    ucp->uc_link = uc_link;
    makecontext(ucp, func, 0);
}

int main(int argc, char *argv[]) {
    verbose = (argc >= 2) && (strcmp(argv[1], "--verbose") == 0);
    char reader_stack[F_STACK_SIZE];
    char formatter_stack[F_STACK_SIZE];
    char writer_stack[F_STACK_SIZE];

    word = malloc(1024 * sizeof(char));

    create_context(&uctx_reader, &uctx_main, reader_stack, read_word);
    create_context(&uctx_formatter, &uctx_writer, formatter_stack, process_word);
    create_context(&uctx_writer, &uctx_reader, writer_stack, print_word);

    printf("main: swapcontext -> reader\n");
    if (swapcontext(&uctx_main, &uctx_reader) == -1)
        handle_error("swapcontext main");

    printf("main: exiting\n");
    exit(EXIT_SUCCESS);
}

