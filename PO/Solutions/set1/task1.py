
def torba():
    return []


def zbior():
    return ()


def wstaw(k, e):
    if isinstance(k, list):
        k.append(e)
    else:
        # I can't mirror the behaviour, because tuples are immutable...
        k = set(k)
        k.add(e)
        return tuple(k)


def rozmiar(k):
    return len(k)


def szukaj(k, e):
    return k.count(e)


def szukaj_v2(k, e):
    # This is much faster on big data sets than the function above
    from collections import Counter
    return Counter(k)[e]


def test():
    print('Torba: ')
    t = torba()
    print(t)
    wstaw(t, 1); wstaw(t, 2); wstaw(t, 2)
    print(t)
    print(rozmiar(t))
    print(szukaj(t, 2))

    print('\n\nZbior: ')
    z = zbior()
    print(z)
    z = wstaw(z, 1)
    z = wstaw(z, 2)
    z = wstaw(z, 2)
    print(z)
    print(rozmiar(z))
    print(szukaj(z, 2))


if __name__ == '__main__':
    test()
