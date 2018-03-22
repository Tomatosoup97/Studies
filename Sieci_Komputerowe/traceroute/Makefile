CC = gcc -g
CFLAGS=-std=gnu99 -Wall -Wextra -Werror

BINS = traceroute

all: $(BINS)

traceroute: traceroute.c

clean:
	rm -vf $(BINS)
	rm -vf *.o

test: all
	./traceroute 216.58.214.78

