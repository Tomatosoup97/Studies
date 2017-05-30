
lengthL :: [a] -> Int
lengthL = foldl (const . (1+)) 0


lengthR :: [a] -> Int
lengthR = foldr (const (1+)) 0


(++) :: [a] -> [a] -> [a]
(++) = flip $ foldr (:)


concat :: Foldable t => t [a] => [a]
concat = foldr (++) []


reverse :: [a] -> [a]
reverse = foldl (flip (:)) []


sum :: [a] -> Int
sum = foldl (+) 0

