CC = gcc -g
CFLAGS = -g -std=gnu11 -Wall -Werror -Wextra -lrt -D_GNU_SOURCE

BINS = tests

all: $(BINS) libmalloc.so

run_tests: clean tests
	./tests

tests: mem_mgmt.o chunk.o
	$(CC) $(CFLAGS) tests.c mem_mgmt.o chunk.o -lm -o tests

mem_mgmt.o:
	$(CC) $(CFLAGS) -fPIC -c mem_mgmt.c chunk.h

chunk.o:
	$(CC) $(CFLAGS) -fPIC -c chunk.c chunk.h

libmalloc.so: mem_mgmt.o chunk.o
	$(CC) -shared -o libmalloc.so mem_mgmt.o chunk.o -pthread

clean:
	rm -vf $(BINS)
	rm -vf *.o
	rm -vf *.gch
	rm -vf *.so
