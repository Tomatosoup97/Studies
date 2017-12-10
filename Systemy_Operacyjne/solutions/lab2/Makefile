CC = gcc -g
CFLAGS=-std=gnu11 -Wall -Werror -Wextra -pthread -lrt

BINS = task1 task2a task2b horse_racing savages

all: $(BINS)

# Tasks

task1: common.o semaphore.o task1.o
	$(CC) $(CFLAGS) common.o semaphore.o task1.o -o task1

task1.o: task1.c semaphore.h common.h
	$(CC) $(CFLAGS) -c task1.c

task2a: common.o philosophers.o semaphore.o task2a.o
	$(CC) $(CFLAGS) common.o philosophers.o semaphore.o task2a.o -o task2a

task2a.o: task2a.c philosophers.h semaphore.h common.h
	$(CC) $(CFLAGS) -c task2a.c

task2b: common.o philosophers.o task2b.o
	$(CC) $(CFLAGS) common.o philosophers.o task2b.o -o task2b

task2b.o: task2b.c philosophers.h common.h
	$(CC) $(CFLAGS) -c task2b.c

horse_racing: horse_racing.o barrier.o common.o
	$(CC) $(CFLAGS) horse_racing.o barrier.o common.o -o horse_racing

horse_racing.o: horse_racing.c barrier.h common.h
	$(CC) $(CFLAGS) -c horse_racing.c

savages: savages.o common.o
	$(CC) $(CFLAGS) savages.o common.o -o savages

savages.o: savages.c common.h
	$(CC) $(CFLAGS) -c savages.c

# Modules

semaphore.o: semaphore.c semaphore.h common.h
	$(CC) $(CFLAGS) -c semaphore.c

philosophers.o: philosophers.c philosophers.h common.h
	$(CC) $(CFLAGS) -c philosophers.c

common.o: common.c common.h
	$(CC) $(CFLAGS) -c common.c

barrier.o: barrier.c barrier.h common.h
	$(CC) $(CFLAGS) -c barrier.c

clean:
	rm -vf $(BINS)
	rm -vf *.o

