import Control.Monad


tails1 :: [a] -> [[a]]
tails1 [x, y] = return [y]
tails1 xs@(x:xs') = return xs' `mplus` do tails1 xs'


tails2 :: [a] -> [[a]]
tails2 [x, y] = return [y]
tails2 xs@(x:xs') = xs' : [ys | ys <- tails2 xs']


tails3 :: [a] -> [[a]]
tails3 [x, y] = return [y]
tails3 xs@(x:xs') = xs' : tails2 xs'
