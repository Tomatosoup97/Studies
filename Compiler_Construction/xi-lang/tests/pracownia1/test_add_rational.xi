ratadd(p1:int, q1:int, p2:int, q2:int) : int, int {
    g:int = gcd(q1,q2)
    p3:int = p1*(q2/g) + p2*(q1/g)
    return p3, q1/g*q2
}

//@PRACOWNIA
//@stop_after parser
