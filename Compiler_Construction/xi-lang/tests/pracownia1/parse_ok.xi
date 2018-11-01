
f()

f(x:int)

f():int
{
    return 0
}

f(x:int, y:int)

f(): bool

f(): int, int

f()
{
    x:int
}

f() { x:int y:int }

f() {
    x:int = 1
}

f()
{
    return "Wroclaw"
}

f()
{
    return 5, 10, 15, "zaraz sie zacznie"
}

f()
{
    x:int x = 10 y = 17
}

f()
{
    x = 1 + 2 + 3
    x = 1 - 2 - 3
    x = 1 * 2 * 3
    x = 1 / 2 / 3
    x = 1 % 2 % 3
    x = 1 & 2 & 3
    x = 1 | 2 | 3
}


f()
{
    if (x > 10) y = 1
    if x > 10 y = 1
    if x > 10 {
        return "42"
    }
    if pred() y = 1
    if pred() & y | z x = 1
}

f()
{
    if (x > 10) y = 1 else z = 1
    if x > 10 y = 1 else z = 1
    if x > 10 { 
        return "42"
    } else z = 1

    if pred() y = 1 else {
        z = 1
    }
    if pred() & y | z x = 1 else {
        z = 3
    }
}

f()
{
    if x if y z = 1 else z = 2
}

f()
{
    while (x > 10) y = 1
    while x > 10 y = 1
    while x y = 1
    while pred() y = 1
    while pred() {
        zmienna = wartosc
        return
    }
}

f()
{
    g()
    g(1, 2)
    g(1, 2, 3)
}

f()
{
    x:int, y:int = f()
    x:int, y:int = f(1, 2)
    _, y:int = f(1, 2)
    _, _ = f(1, 2)
    x:int, _ = f(1, 2)
}

f()
{
    z = length("x")
    z = length(x + y)
    z = length(x - y)
    z = length(x & y)
    z = length(x / y)
    z = length({1,2,3} + 1)
}

g()
{
    z = 'a'
    z = 'b'
    z = '\n'
    z = '\\'
    z = '\''
}

g()
{
    z = "a"
    z = "abece de"
    z = "abece\nde"
    z = "abece\\de"
    z = "abece\"de"
}

g(x:int[][]):bool[][]

g(x:int[1][]):bool[][x]

g()
{
    x:int[][][]
    x:int[][][1]
    x:int[2][][1]
    x:int[][n][]
}

g()
{
    x[0] = 1
    x[0][1] = 2
    x[a][b] = c[d]
}

//@PRACOWNIA
//@stop_after parser
