from random import randint
MAX_SIZE = 50000
MAX_N = 10**6


def main():
    with open('test_all_non_existing.in', 'r+') as f:
        with open('test_all_non_existing.out', 'r+') as out:
            n = MAX_N
            number_of_ops = MAX_N - 1 * MAX_SIZE

            f.write("{}\n".format(n))

            for i in range(MAX_SIZE):
                f.write("I {}\n".format(i))

            for i in range(number_of_ops):
                random_int = (-1) * randint(MAX_SIZE+1, 5*MAX_SIZE)
                f.write("D {}\n".format(random_int))
                # f.write("L {}\n".format(random_int))
                # out.write("{}\n".format(random_int))
                out.write("BRAK\n")

            # for i in range(MAX_SIZE):
            #     f.write("D {}\n".format(i))
            #     out.write("OK\n")


if __name__ == '__main__':
    main()

