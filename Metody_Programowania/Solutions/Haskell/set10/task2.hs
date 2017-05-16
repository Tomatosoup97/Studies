cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) = pair (f . fst, g . snd)

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

halve :: [a] -> ([a], [a])
halve xs = (take len xs, drop len xs)
    where len = length xs `div` 2

merge :: Ord a => ([a], [a]) -> [a]
merge ([], xs) = xs
merge (xs, []) = xs
merge (x:xs, y:ys)
    | x <= y = x : merge (xs, y:ys)
    | otherwise = y : merge (x:xs, ys)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge . cross (msort, msort) . halve $ xs
