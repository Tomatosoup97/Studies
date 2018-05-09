from random import randint
MAX_SIZE = 50000
MAX_N = 10**6


def main():
    with open('test_many_inserts_deletes.in', 'r+') as f:
        with open('test_many_inserts_deletes.out', 'r+') as out:
            n = MAX_N
            number_of_ops = MAX_N - 1 * MAX_SIZE

            f.write("{}\n".format(n))

            for i in range(MAX_SIZE):
                f.write("I {}\n".format(i))

            for i in range(number_of_ops//2):
                random_int = randint(1, MAX_SIZE-1)
                f.write("D {}\n".format(random_int))
                f.write("I {}\n".format(random_int))
                out.write("OK\n")

            # for i in range(MAX_SIZE):
            #     f.write("D {}\n".format(i))
            #     out.write("OK\n")


if __name__ == '__main__':
    main()

