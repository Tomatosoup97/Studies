CC = gcc -g
CFLAGS=-std=gnu99 -Wall -Wextra

BINS = traceroute icmp_receive

all: $(BINS)

traceroute: traceroute.c

icmp_receive: icmp_receive.c

clean:
	rm -vf $(BINS)
	rm -vf *.o

test: all
	./traceroute 156.17.4.11

