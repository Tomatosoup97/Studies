CC = g++ -Os

all: task5

task5:
	$(CC) -S task5.cpp

bin: clean
	$(CC) task5.cpp -o task5

clean:
	rm -f *.s
	rm -f *.o
	rm -f task5

