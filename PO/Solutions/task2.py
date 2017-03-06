
def Figura(x1, y1, x2=None, y2=None):
    return [x1, y1, x2, y2]


def narysuj(f):
    # Point
    if (f[2], f[3]) == (None, None):
        print(f[:2])

    # Circle
    elif f[3] is None: 
        print(f[:3])

    # Square
    else:
        print(f)


def przesun(f, x, y):
    # Point, Circle
    if f[3] == None:
        f[0] += x
        f[1] += y

    # Square
    else:
        f[0] += x
        f[2] += x
        f[1] += y
        f[3] += y


def zawiera(f, x, y):
    # Point
    if (f[2], f[3]) == (None, None):
        return (x, y) == (f[0], f[1])

    # Circle
    elif f[3] is None: 
        return (abs(x - f[0]) <= f[2] and \
                abs(y - f[1]) <= f[2])

    # Square
    else:
        return (x >= f[0] and y >= f[1] and \
                x <= f[2] and y <= f[3])


def Point(x, y):
    return Figura(x, y)


def Circle(x, y, r):
    return Figura(x, y, r)


def Square(x1, y1, x2, y2):
    return Figura(x1, y1, x2, y2)


def test_przesun():
    print("Przesun: \n")

    print("Kwadrat: \n")
    f = Figura(1, 2, 3 ,4)
    narysuj(f)
    przesun(f, 1, 2)
    narysuj(f)

    print("\nKolo: \n")
    f = Figura(1, 2, 3)
    narysuj(f)
    przesun(f, 1, 2)
    narysuj(f)

    print("\nPunkt: \n")
    f = Figura(1, 2)
    narysuj(f)
    przesun(f, 1, 2)
    narysuj(f)
    print('---')


def test_zawiera():
    print("\nZawiera: \n")

    print("Kwadrat: \n")
    f = Figura(1, 2, 3 ,4)
    narysuj(f)
    print('zawiera 2, 3: ')
    print(zawiera(f, 2, 3))

    print("\nKolo: \n")
    f = Figura(1, 2, 3)
    narysuj(f)
    print('zawiera 2, 3: ')
    print(zawiera(f, 2, 3))

    print("\nPunkt: \n")
    f = Figura(1, 2)
    narysuj(f)
    print('zawiera 2, 3: ')
    print(zawiera(f, 2, 3))


if __name__ == '__main__':
    test_przesun()
    test_zawiera()
