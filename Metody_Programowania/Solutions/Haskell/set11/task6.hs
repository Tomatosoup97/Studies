{-# LANGUAGE FlexibleContexts #-}

f :: (Num ((a -> b) -> [a] -> [b]),
      Num (t -> (a -> b) -> [a] -> [b])) =>
        t -> (a -> b) -> [a] -> [b]
f x = map - (1 x)

f' :: Num (a -> b) => [a] -> [b]
f' x = map (-1) x

g :: Num [a] => a -> [[a]]
g x = [x] : [1]


h :: (Num (b -> a), Floating a) => (b -> a) -> b -> a
h x = x * (sin . 1)
