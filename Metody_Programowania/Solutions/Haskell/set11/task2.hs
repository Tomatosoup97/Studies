
addGreater :: Ord a => [a] -> a -> [a]
addGreater xs x
    | x > (head xs) = x:xs
    | otherwise = xs


ssm :: Ord a => [a] -> [a]
ssm [] = []
ssm (x:xs) = reverse $ foldl addGreater [x] xs
