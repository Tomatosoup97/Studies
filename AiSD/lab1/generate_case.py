
N = 10 ** 6


def main():
    with open('przyklad_G', 'r+') as f:
        f.write("{} {}\n".format(N, 2))

        for i in range(1, N):
            f.write("{}\n".format(1))

        f.write("1 {}\n".format(N))
        f.write("{} 1\n".format(N))


if __name__ == '__main__':
    main()

