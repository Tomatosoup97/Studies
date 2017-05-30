sublist1 :: [a] -> [[a]]
sublist1 [] = [[]]
sublist1 (x:xs) = [zs | ys <- sublist1 xs, zs <- [x:ys, ys]]


sublist2 :: [a] -> [[a]]
sublist2 [] = [[]]
sublist2 (x:xs) = concatMap (\ys -> [x:ys, ys]) $ sublist2 xs

sublist3 :: [a] -> [[a]]
sublist3 [] = [[]]
sublist3 (x:xs) = do
    ys <- sublist3 xs
    [(x:ys), ys]

