CC = gcc -g
CFLAGS=-std=gnu11 -Wall -Werror

BINS = task1 task2 task3

all: $(BINS)

task1: task1.c common.h

task2: task2.c procword.h common.h

task3: task3.c procword.h common.h

clean:
	rm -vf $(BINS)

