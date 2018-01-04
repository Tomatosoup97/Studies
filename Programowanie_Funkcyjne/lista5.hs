-- Mateusz Urbańczyk Lista 5


repeat' x 0 = []
repeat' x n
    | n > 0 = x : repeat' x (n-1)
    | otherwise = error "negative n"


lrepeat :: (Int -> Int) -> [a] -> [a]
lrepeat = aux 0
    where aux n f [] = []
          aux n f (x:xs) = repeat' x (f n) ++ aux (n+1) f xs


test_lrepeat0 = lrepeat (+1) [1..]
test_lrepeat1 = lrepeat (+1) [2, 4..]
test_lrepeat2 = lrepeat id [5, 6, 7, 8]
test_lrepeat3 = lrepeat id []

