-- Mateusz Urbańczyk Lista 3

-- Zadanie 4

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)


cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) = pair (f . fst, g . snd)


-- This halve does not provide merge sort stability, although is
-- more efficient and tail recursive
halve' :: [a] -> ([a], [a])
halve' = aux [] []
    where aux ys ys' xs = case xs of
            [] -> (ys, ys')
            [x] -> (x:ys, ys')
            x:x':xs -> aux (x:ys) (x':ys') xs


halve :: [a] -> ([a], [a])
halve xs = (take len xs, drop len xs)
    where len = length xs `div` 2


merge_aux :: (a -> a -> Bool) -> [a] -> [a] -> [a] -> [a]
merge_aux cmp acc [] xs = reverse acc ++ xs
merge_aux cmp acc xs [] = reverse acc ++ xs
merge_aux cmp acc (xs@(x:xs')) (ys@(y:ys'))
        | cmp x y = merge_aux cmp (x:acc) xs' ys
        | otherwise = merge_aux cmp (y:acc) xs ys'


merge :: (a -> a -> Bool) -> ([a], [a]) -> [a]
merge cmp (xs, ys) = merge_aux cmp [] xs ys

mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort cmp [] = []
mergesort cmp [x] = [x]
mergesort cmp xs = (merge cmp) . cross (mergesort cmp, mergesort cmp) . halve $ xs


test_mergesort_0 = mergesort (\a -> \b -> True) []
test_mergesort_1 = mergesort (<=) [5, 7, 1, 2, 42, 2 ,5, 8, 2]
test_mergesort_2 = mergesort (>=) [5, 7, 1, 2, 42, 2 ,5, 8, 2]

-- Stability tests

cmp_fst a b = fst a <= fst b

test_mergesort_3 = mergesort cmp_fst [(1, 1), (2, 1), (1, 2), (2, 2)]

test_mergesort_4 = mergesort cmp_fst [(1, 2), (2, 1), (1, 1), (2, 2)]

test_mergesort_5 = mergesort cmp_fst [(1, 2), (1, 1), (2, 2), (2, 1)]

