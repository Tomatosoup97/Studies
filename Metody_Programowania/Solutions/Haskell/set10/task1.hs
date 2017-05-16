nat2 :: [(Integer, Integer)]
nat2 = [(x, y) | z <- [0..], x <- [0..z], y <- [0..z], (x + y) == z]

nat2' :: [(Integer, Integer)]
nat2' = [(x, y-x) | y <- [0..], x <- [0..y]]
