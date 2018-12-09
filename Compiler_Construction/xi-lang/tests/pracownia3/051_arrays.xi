foo() : int[], int[][] {
        x:int[] = {1,2}
        y:int[3][3]

        y[0][0] = 3
        y[0][1] = 4
        y[0][2] = 5
        y[1][0] = 6
        y[1][1] = 7
        y[1][2] = 8
        y[2][0] = 9
        y[2][1] = 10
        y[2][2] = 11

        return x, y
}

main():int {
    xs:int[], ys:int[][] = foo()


    return xs[0] + xs[1] + ys[0][0] + ys[2][2]
}

//@PRACOWNIA
//@out Exit code: 17
