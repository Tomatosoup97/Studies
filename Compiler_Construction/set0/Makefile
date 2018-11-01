CC = gcc -march=nehalem -fomit-frame-pointer -Os
CC_0 = gcc -march=nehalem -fomit-frame-pointer -O0

all: task1 task2 task3 task5

task1:
	$(CC) -S task1.c

task2:
	$(CC) -S task2.c
	$(CC_0) -S task2.c -o task20.s

task3:
	$(CC) -S task3.c

strrchr:
	$(CC) -fdump-tree-all strrchr.c -o strrchr.o

task5:
	$(CC) -S task5.c -o task5s.s
	$(CC_0) -S task5.c -o task50.s
	$(CC) task5.c -o task5s
	$(CC_0) task5.c -o task50

clean:
	rm -f *.s
	rm -f *.o
	rm -f strrchr.c.*
	rm -f task5s
	rm -f task50

