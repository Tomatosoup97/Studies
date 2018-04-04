CC = gcc
CFLAGS=-std=gnu99 -Wall -Wextra

BINS = router

all: clean $(BINS)

router: router.o udp.o table.o node.o

clean:
	rm -vf $(BINS)
	rm -vf *.o

test: export MACHINE_NAME=test
test: clean all
	./router < ../test1/b

