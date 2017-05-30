import Data.List


perm1 :: Eq a => [a] -> [[a]]
perm1 [] = [[]]
perm1 (x:xs) = [zs | ys <- perm1 xs, zs <- insert' x ys]


perm2 [] = [[]]
perm2 (x:xs) = concatMap (insert' x) (perm xs)


perm :: [a] -> [[a]]
perm [] = [[]]
perm (x:xs) = do
    ys <- perm xs
    zs <- insert' x ys
    return zs


insert' x [] = [[x]]
insert' x ys@(y:ys') = [x:ys] ++ (map (y:) $ insert' x ys')

