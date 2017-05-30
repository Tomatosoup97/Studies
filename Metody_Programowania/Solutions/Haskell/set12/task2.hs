import Data.List


perms1 :: Eq a => [a] -> [[a]]
perms1 [] = [[]]
perms1 xs = [(y:zs) | y <- xs, zs <- perms1 $ delete y xs]


perms2 :: Eq a => [a] -> [[a]]
perms2 [] = [[]]
perms2 xs = concatMap (\x -> map (x:) $ perms2 $ delete x xs) xs


perms3 :: Eq a => [a] -> [[a]]
perms3 [] = [[]]
perms3 xs = do
    y <- xs
    zs <- perms3 $ delete y xs
    return (y:zs)

