test()
{
    x = a | b & c;
    x = a & b | c & d;
    x = a | b | c;
    x = a < b < c;
    x = a < b == c < d;
    x = a < b * c == c + d < e * f
    x = a * b / d % e

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

//@PRACOWNIA
//@stop_after parser
