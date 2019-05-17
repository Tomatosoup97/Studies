from functools import reduce


def compose(*funcs):
    return reduce(lambda f, g: lambda *args: f(g(*args)), funcs, lambda x: x)
