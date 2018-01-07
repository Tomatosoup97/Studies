CC = gcc -g
CFLAGS = -g -std=gnu11 -Wall -Wextra -lrt -D_GNU_SOURCE
#CFLAGS = -g -std=gnu11 -Wall -Werror -Wextra -lrt -D_GNU_SOURCE

BINS = tests

# all:
# 	echo "Not implemented"


run_tests: clean tests
	./tests

tests: mem_mgmt.o chunk.o
	$(CC) $(CFLAGS) tests.c mem_mgmt.o chunk.o -lm -o tests

mem_mgmt.o:
	$(CC) $(CFLAGS) -c mem_mgmt.c chunk.h

chunk.o:
	$(CC) $(CFLAGS) -c chunk.c chunk.h


clean:
	rm -vf $(BINS)
	rm -vf *.o
	rm -vf *.gch

