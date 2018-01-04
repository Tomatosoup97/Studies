-- Mateusz Urbańczyk lista 2

-- Zad. 6

repeatNum x = aux [] x x where
    aux xs x y
        | x <= 0 || y <= 0 = xs
        | otherwise = aux (x:xs) x (y-1)

testRepeatNum1 = repeatNum 0
testRepeatNum2 = repeatNum 4


repeatList :: [Int] -> [Int]
repeatList [] = []
repeatList (x:xs) = (repeatNum x) ++ (repeatList xs)

testRepeatList1 = repeatList []
testRepeatList2 = repeatList [0, 0]
testRepeatList3 = repeatList [1, 0, 4, 3]


