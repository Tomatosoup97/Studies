
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM f acc [] = return acc
foldrM f acc (x:xs) = f x <<= foldrM f acc xs
    where (<<=) = flip (>>=)


prod :: (Eq a, Num a) => [a] -> Maybe a
prod xs = foldrM (\n p -> if n == 0 then Nothing else Just (n * p)) 1 xs


prod' :: [Integer] -> Integer
prod' = foldr (\ n p -> if n==0 then 0 else p * n) 1

