CC = g++ -g
CFLAGS =-std=gnu++14 -Wall -Wextra -Wshadow -O2 -static

BINS = main

all: $(BINS)

run_tests: clean main
	./main < przyklad_A
	./main < przyklad_B
	./main < przyklad_C

main: main.cpp
	$(CC) $(CFLAGS) main.cpp -o main

clean:
	rm -vf $(BINS)

