CC = gcc -g
CFLAGS=-std=gnu99 -Wall -Wextra

BINS = router

all: $(BINS)

router: router.o table.o

clean:
	rm -vf $(BINS)
	rm -vf *.o

test: export MACHINE_NAME=test
test: clean all
	./router < ../test1/a
	./router < ../test1/b

