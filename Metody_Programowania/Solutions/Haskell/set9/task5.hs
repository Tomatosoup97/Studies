

roots :: (Double, Double, Double) -> [Double]

roots (a, b, c) = 
    if a == 0 then
        if b == 0 then []
        else [ -c / b ]

    else
        case compare delta 0 of
        LT -> []
        EQ -> [ -b / (2 * a) ]
        GT -> [ -b + sqrt(delta) / (2 * a), -b - sqrt(delta) / (2 * a)]
        where delta = b * b - 4 * a * c



roots' :: (Double, Double, Double) -> [Double]

roots' (a, b, c) 
    | (a, b) == (0, 0) = []
    | a == 0 = [ -c / b]
    | otherwise =
        case compare delta 0 of
        LT -> []
        EQ -> [ -b / dbl_a ]
        GT -> [ -b + sqrt(delta) / dbl_a,
                -b - sqrt(delta) / dbl_a]
        where delta = b * b - 4 * a * c
              dbl_a = 2 * a

