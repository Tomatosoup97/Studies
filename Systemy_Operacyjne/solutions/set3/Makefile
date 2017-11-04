CFLAGS=-Wall -Werror

all:
    gcc ${CFLAGS} signal_handler.c -o handler
    gcc ${CFLAGS} dispatcher.c -o dispatcher

run:
    ./dispatcher
