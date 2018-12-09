
main():int {
    xs:int[2][3][4][5];
    ys:int[2][3][4][];
    xs[1][2][3][4] = 25
    ys[1][2][3] = {1, 17, 3}

    return xs[1][2][3][4] + ys[1][2][3][1]
}

//@PRACOWNIA
//@out Exit code: 42
