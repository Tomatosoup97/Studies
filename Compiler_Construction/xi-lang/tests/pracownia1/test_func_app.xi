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

//@PRACOWNIA
//@stop_after parser
