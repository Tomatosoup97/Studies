#include <stdbool.h>
#include <ctype.h>
#include <stdlib.h>

#define AL_SIZE 26
#define EXIT_WORD "EXIT"
#define CHARS_ARRAY_SIZE (26 + 10)  // letters + digits

char* filter(char *word, int len, bool (*f)(char)) {
    char *c = word;
    char *new_word = malloc(len * sizeof(char));
    int i = 0;

    while (*c != '\0')
        if (f(*c++)) *(new_word + i++) = *(c-1);

    new_word[i] = '\0';
    return new_word;
}

bool isalnum_wrapper(char c) {
    // Wrapper that makes types match with filter function
    return isalnum(c);
}

int get_char_index(char c) {
    if (isdigit(c)) {
        return c - '0' + AL_SIZE;
    } else {
        return toupper(c) - (int) 'A';
    }
}

char get_char_by_index(int i) {
    if (i >= AL_SIZE) {
        return (i - AL_SIZE) + '0';
    } else {
        return 'A' + i;
    }
}

