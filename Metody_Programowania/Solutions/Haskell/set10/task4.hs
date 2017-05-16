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

msortn :: Ord a => [a] -> Int -> [a]
msortn xs 0 = []
msortn (x:xs) 1 = [x]
msortn xs n = merge ((msortn xs l), msortn (drop l xs) (n - l))
    where l = n `div` 2

msort :: Ord a => [a] -> [a]
msort xs = msortn xs n
    where n = length xs
